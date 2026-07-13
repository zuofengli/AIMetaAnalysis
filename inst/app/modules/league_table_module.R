# modules/league_table_module.R
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(shinyjqui)   # 拖拽排序
library(openxlsx)    # Excel 导出及样式
library(gt)          # 高质量表格构建
library(webshot2)    # PDF 渲染（基于 Chrome）

# 统一清洗函数（与主模块保持一致）
clean_treatment_unified <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  trimws(x)
}

# ---------- 辅助：判断单元格是否显著（与 JS 逻辑完全一致）----------
is_significant_cell <- function(cell, eff_type) {
  if (is.null(cell) || is.na(cell) || cell == "") return(FALSE)
  cell <- trimws(cell)

  # 对角线参考值不标红
  if (eff_type %in% c("OR","RR","HR") && grepl("^1\\s*\\(ref\\)$", cell)) return(FALSE)
  if (eff_type %in% c("RD","MD")   && grepl("^0\\s*\\(ref\\)$", cell)) return(FALSE)

  # 提取括号内的下限和上限
  pattern <- "\\((-?[0-9.]+),\\s*(-?[0-9.]+)\\)"
  m <- regmatches(cell, regexec(pattern, cell))[[1]]
  if (length(m) >= 3) {
    lower <- as.numeric(m[2])
    upper <- as.numeric(m[3])

    if (eff_type %in% c("OR","RR","HR")) {
      return(!(lower <= 1 && upper >= 1))   # OR/RR/HR：区间不包含 1
    } else {
      return(!(lower <= 0 && upper >= 0))   # RD/MD：区间不包含 0
    }
  }
  return(FALSE)
}

# ---------- 辅助：将联赛表绘制为 PDF（使用 gt + webshot2）----------
draw_league_table_pdf <- function(df, eff_type, file) {
  # 检查依赖包
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("Please install gt: install.packages('gt')")
  }
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    stop("Please install webshot2: install.packages('webshot2')")
  }

  # 1. 准备数据
  mat <- as.matrix(df[, -1, drop = FALSE])
  rownames(mat) <- df[[1]]
  colnames(mat) <- df[[1]]

  # 2. 构建 gt 表格
  gt_tbl <- mat %>%
    as.data.frame() %>%
    gt(rownames_to_stub = TRUE) %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(10),
      column_labels.font.size = px(10),
      column_labels.font.weight = "bold",
      data_row.padding = px(3),
      stub.font.size = px(10),
      stub.font.weight = "bold",
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      heading.border.bottom.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.top.color = "black"
    ) %>%
    tab_header(
      title = paste("League Table -", eff_type, " (95% CI)")
    ) %>%
    cols_align(align = "center", columns = everything()) %>%
    tab_style(
      style = cell_text(align = "left"),
      locations = cells_stub()
    )

  # 3. 对显著单元格应用红色加粗
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      cell_text <- mat[i, j]
      if (is_significant_cell(cell_text, eff_type)) {
        gt_tbl <- gt_tbl %>%
          tab_style(
            style = list(cell_text(color = "red", weight = "bold")),
            locations = cells_body(columns = colnames(mat)[j], rows = i)
          )
      }
    }
  }

  # 4. 生成 HTML（inline_css = TRUE 确保样式内嵌）
  temp_html <- tempfile(fileext = ".html")
  gtsave(gt_tbl, filename = temp_html, inline_css = TRUE)

  # 5. 使用 selector 精确裁剪
  tryCatch({
    webshot2::webshot(
      url = temp_html,
      file = file,
      vwidth = 2000,
      vheight = 2000,
      zoom = 1.5,
      selector = "table",
      expand = 0,
      delay = 0.5
    )
  }, error = function(e) {
    # 降级方案：使用 viewport + 手动估算
    warning("Selector method failed, falling back to viewport clipping: ", e$message)
    n_trt <- ncol(mat)
    col_width_px <- 100
    stub_width_px <- 120
    vwidth <- stub_width_px + n_trt * col_width_px + 40
    vheight <- 100 + nrow(mat) * 30 + 30
    webshot2::webshot(
      url = temp_html,
      file = file,
      vwidth = max(vwidth, 800),
      vheight = max(vheight, 600),
      zoom = 1.5,
      cliprect = "viewport",
      delay = 0.5
    )
  })

  unlink(temp_html)
}

# ---------- 新增：导出 LaTeX 表格代码 ----------
generate_latex_table <- function(df, eff_type, file) {
  # 准备矩阵
  mat <- as.matrix(df[, -1, drop = FALSE])
  rownames(mat) <- df[[1]]
  colnames(mat) <- df[[1]]

  n <- nrow(mat)
  trt_names <- rownames(mat)

  # 对角线参考值
  ref_val <- if (eff_type %in% c("OR", "RR", "HR")) "1 (ref)" else "0 (ref)"

  # 表头行
  header <- paste0(" & ", paste(trt_names, collapse = " & "))

  # 列格式：第一列左对齐，其余居中
  col_spec <- paste0("l|", paste(rep("c", n), collapse = ""))

  # 构建每一行
  rows <- character(n)
  for (i in 1:n) {
    row_name <- trt_names[i]
    row_cells <- character(n)
    for (j in 1:n) {
      if (i == j) {
        row_cells[j] <- ref_val
      } else {
        cell_text <- mat[i, j]
        # 解析 "OR (lower, upper)" 格式
        pattern <- "^([0-9.]+)\\s*\\(([0-9.]+),\\s*([0-9.]+)\\)$"
        matches <- regexec(pattern, cell_text)
        extracted <- regmatches(cell_text, matches)[[1]]
        if (length(extracted) == 4) {
          or_val <- extracted[2]
          lower <- extracted[3]
          upper <- extracted[4]
          ci_str <- paste0("(", lower, ", ", upper, ")")
          # 构建 makecell 换行
          cell_content <- paste0("\\makecell{", or_val, " \\\\ ", ci_str, "}")
          # 显著性标记：若显著，整个 makecell 红色加粗
          if (is_significant_cell(cell_text, eff_type)) {
            cell_content <- paste0("\\textbf{\\textcolor[HTML]{FF0000}{", cell_content, "}}")
          }
          row_cells[j] <- cell_content
        } else {
          # 若解析失败（例如空白单元格），保留原文本
          row_cells[j] <- cell_text
        }
      }
    }
    rows[i] <- paste0("\\multicolumn{1}{l}{", row_name, "} & ", paste(row_cells, collapse = " & "))
  }

  # 构建完整 LaTeX 代码（使用 resizebox 缩放至页面宽度）
  table_code <- paste0(
    "\\renewcommand{\\arraystretch}{1.3}\n",
    "\\resizebox{\\textwidth}{!}{%\n",
    "  \\begin{tabular}{", col_spec, "}\n",
    "    \\toprule\n",
    header, " \\\\\n",
    "    \\midrule \\addlinespace[2.5pt]\n",
    paste(rows, collapse = " \\\\\n"),
    " \\\\\n",
    "    \\bottomrule\n",
    "  \\end{tabular}%\n",
    "}"
  )

  writeLines(table_code, con = file)
}

leagueTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("联赛表 (League Table)"),
    div(
      class = "well",
      style = "background-color: #f5f5f5; padding: 10px; margin-bottom: 15px; border-left: 4px solid #337ab7;",
      h5("📖 如何解读联赛表？"),
      tags$ul(
        tags$li(HTML("🔍 <strong>上三角展示结果</strong>，方向：<strong>列治疗 vs 行治疗</strong>")),
        tags$li(HTML("📌 <strong>列治疗</strong>为分子/被比较组（实验组），<strong>行治疗</strong>为分母/参照组（对照组）")),
        tags$li(HTML("📊 <strong>效应量判断</strong>：若为 OR/RR/HR，看区间是否包含 1；若为 RD/MD，看区间是否包含 0。")),
        tags$li(HTML("🎯 <strong>红色加粗</strong>表示该比较具有统计学显著性。")),
        tags$li(HTML("📐 <strong>上下三角关系</strong>：下三角数值 = 1/上三角数值（OR/RR/HR）或 -上三角（RD/MD）"))
      ),
      actionLink(ns("show_guide"), "📘 点击查看详细解读案例", icon = icon("book"))
    ),
    uiOutput(ns("dynamic_guide")),
    fluidRow(
      column(4,
             selectInput(ns("sort_by"), "排序方式",
                         choices = c("原始顺序" = "original",
                                     "按SUCRA降序" = "sucra",
                                     "按治疗名称排序" = "name",
                                     "自定义顺序（拖拽）" = "custom"),
                         selected = "original")
      ),
      column(3,
             checkboxInput(ns("show_lower_tri"), "显示下三角", value = FALSE)
      ),
      column(3,
             checkboxInput(ns("show_diag_names"), "对角线显示治疗名称（隐藏行列标题）", value = FALSE)
      ),
      column(2,
             actionButton(ns("reset_order"), "重置顺序",
                          icon = icon("undo"), class = "btn-warning")
      )
    ),
    fluidRow(
      column(4,  # 调整列宽以容纳三个按钮
             downloadButton(ns("download"), "下载 Excel", class = "btn-success"),
             downloadButton(ns("download_pdf"), "下载 PDF", class = "btn-success"),
             downloadButton(ns("download_latex"), "下载 LaTeX (.tex)", class = "btn-primary")  # 新增
      ),
      column(8,
             div(style = "text-align: right;",
                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'custom'", ns("sort_by")),
                   div(style = "margin-top: 5px; padding: 5px; background: #f9f9f9; border: 1px solid #ddd; border-radius: 4px;",
                       h5("↕️ 拖拽治疗标签调整顺序（上方为行/列的第一行/列）"),
                       uiOutput(ns("custom_sort_ui"))
                   )
                 )
             )
      )
    ),
    verbatimTextOutput(ns("error_msg")),
    DTOutput(ns("table"))
  )
}

# ---------- 辅助函数：从贝叶斯模型构建联赛表 ----------
build_league_table_bayesian <- function(bayes_res, measure) {
  if (is.null(measure) || length(measure) == 0 || is.na(measure)) {
    measure <- "OR"
  }

  model <- bayes_res$model
  network <- bayes_res$network
  if (is.null(network)) {
    stop("模型中没有 network 信息")
  }

  tbl <- tryCatch(
    gemtc::relative.effect.table(model),
    error = function(e) stop("无法生成相对效应表: ", e$message)
  )

  mat <- NULL
  if (is.matrix(tbl)) {
    mat <- tbl
  } else if (is.data.frame(tbl)) {
    mat <- as.matrix(tbl)
  } else {
    tryCatch({
      mat <- as.matrix(as.data.frame(tbl))
    }, error = function(e) {
      stop("无法将 relative.effect.table 结果转换为矩阵: ", e$message)
    })
  }

  trt_names <- rownames(mat)
  if (is.null(trt_names)) trt_names <- colnames(mat)

  convert_to_or <- function(cell) {
    if (is.na(cell) || cell == "" || grepl("\\(ref\\)", cell)) return(cell)
    pattern <- "^(-?[0-9.]+(?:e[+-]?[0-9]+)?)\\s*\\((-?[0-9.]+(?:e[+-]?[0-9]+)?),\\s*(-?[0-9.]+(?:e[+-]?[0-9]+)?)\\)$"
    matches <- regexec(pattern, cell)
    extracted <- regmatches(cell, matches)[[1]]
    if (length(extracted) == 4) {
      log_or <- as.numeric(extracted[2])
      log_lower <- as.numeric(extracted[3])
      log_upper <- as.numeric(extracted[4])
      if (!any(is.na(c(log_or, log_lower, log_upper)))) {
        or <- exp(log_or)
        lower <- exp(log_lower)
        upper <- exp(log_upper)
        return(sprintf("%.2f (%.2f, %.2f)", or, lower, upper))
      }
    }
    num <- suppressWarnings(as.numeric(cell))
    if (!is.na(num)) {
      return(sprintf("%.2f", exp(num)))
    }
    return(cell)
  }

  or_mat <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (i == j) {
        or_mat[i, j] <- ifelse(measure %in% c("OR","RR","HR"), "1 (ref)", "0 (ref)")
      } else {
        or_mat[i, j] <- convert_to_or(mat[i, j])
      }
    }
  }
  colnames(or_mat) <- trt_names
  rownames(or_mat) <- trt_names

  df <- as.data.frame(or_mat, stringsAsFactors = FALSE)
  df <- cbind(Treatment = trt_names, df)
  rownames(df) <- NULL
  df
}

# ==================== Server ====================
# 修改：增加 outcome 和 nma_info 参数（与排名图模块一致）
leagueTableServer <- function(id, nma_result, rank_result, nma_method,
                              outcome = NULL, nma_info = NULL,
                              subgroup = NULL, selected_levels = NULL) {
  moduleServer(id, function(input, output, session) {

    # 获取亚组变量名
    subgroup_val <- reactive({
      if (!is.null(subgroup) && is.function(subgroup)) {
        subgroup()
      } else {
        subgroup   # 如果是静态值
      }
    })

    # 获取选定的亚组水平列表（通常是单个）
    selected_levels_val <- reactive({
      if (!is.null(selected_levels) && is.function(selected_levels)) {
        selected_levels()
      } else {
        selected_levels
      }
    })


    # ---------- 效应量类型 ----------
    get_effect_type <- reactive({
      res <- nma_result()
      if (is.null(res)) return("OR")
      if (res$type == "binary") {
        measure <- res$measure %||% "OR"
        if (measure %in% c("RD", "OR", "RR")) return(measure)
        return("OR")
      } else {
        return("HR")
      }
    })

    # 统一的显著性条件字符串，供 JS 使用
    signif_cond_text <- reactive({
      eff <- get_effect_type()
      if (eff %in% c("OR","RR","HR")) {
        "!(lower <= 1 && upper >= 1)"
      } else {
        "(lower > 0 || upper < 0)"
      }
    })

    map_nma_method <- function(method) {
      switch(method,
             freq_REML = "REML",
             freq_fixed = "fixed",
             freq_NH = "NH",
             bayes = "REML",
             "REML")
    }

    # ---------- 原始联赛表 ----------
    league_table_raw <- reactive({
      res <- nma_result()
      req(res)

      if (res$method_family == "bayesian") {
        bayes_res <- res$result
        measure <- get_effect_type()
        league_df <- tryCatch(
          build_league_table_bayesian(bayes_res, measure),
          error = function(e) {
            output$error_msg <- renderText(paste("贝叶斯联赛表生成失败：", e$message))
            return(NULL)
          }
        )
        if (is.null(league_df)) return(NULL)
        output$error_msg <- renderText("")
        return(league_df)
      }

      if (is.null(res$setup)) {
        output$error_msg <- renderText("❌ 错误：NMA 结果尚未准备好")
        return(NULL)
      }

      current_effect_type <- get_effect_type()
      eform_flag <- !(current_effect_type %in% c("RD", "MD"))
      method_nma <- map_nma_method(nma_method())

      league <- tryCatch(
        NMA::nmaleague(res$setup,
                       method = method_nma,
                       eform = eform_flag),
        error = function(e) {
          output$error_msg <- renderText(paste("❌ 联赛表生成失败：", conditionMessage(e)))
          NULL
        }
      )
      if (is.null(league)) return(NULL)

      if (is.list(league) && !is.data.frame(league) && !is.null(league$R) && is.matrix(league$R)) {
        mat_raw <- league$R
        trt_names <- diag(mat_raw)
        n_trt <- length(trt_names)
        if (n_trt < 2) {
          output$error_msg <- renderText("❌ 治疗组数量不足（少于2个）")
          return(NULL)
        }
        mat_raw[cbind(seq_len(n_trt), seq_len(n_trt))] <- "1 (ref)"
        league_df <- as.data.frame(mat_raw, stringsAsFactors = FALSE)
        colnames(league_df) <- trt_names
        league_df <- cbind(Treatment = trt_names, league_df)
        rownames(league_df) <- NULL
        output$error_msg <- renderText("")
        return(league_df)
      } else {
        output$error_msg <- renderText("❌ 联赛表格式异常")
        return(NULL)
      }
    })

    # ---------- 排序相关 ----------
    get_original_order <- reactive({
      df <- league_table_raw()
      req(df)
      as.character(df[[1]])
    })

    get_sucra_order <- reactive({
      rank_df <- rank_result()
      req(rank_df)
      if(is.data.frame(rank_df) && "Treatment" %in% names(rank_df)) {
        if ("SUCRA" %in% names(rank_df)) {
          sorted <- rank_df %>% arrange(desc(SUCRA))
          return(sorted$Treatment)
        } else if ("P_best" %in% names(rank_df)) {
          sorted <- rank_df %>% arrange(desc(P_best))
          return(sorted$Treatment)
        }
      }
      return(get_original_order())
    })

    get_name_order <- reactive({
      order <- get_original_order()
      sort(order)
    })

    # 动态生成拖拽 UI
    output$custom_sort_ui <- renderUI({
      req(input$sort_by == "custom")
      trt <- get_original_order()
      if (length(trt) == 0) return(NULL)
      current_order <- isolate(input$custom_sort_list)
      if (is.null(current_order) || !all(trt %in% current_order) || length(current_order) != length(trt)) {
        current_order <- trt
      }
      shinyjqui::orderInput(
        inputId = session$ns("custom_sort_list"),
        label = NULL,
        items = current_order,
        item_class = 'info',
        width = "100%"
      )
    })

    custom_order_dragged <- reactive({
      if (input$sort_by != "custom") return(NULL)
      res <- input$custom_sort_list
      if (is.null(res) || length(res) == 0) return(get_original_order())
      all_trt <- get_original_order()
      res[res %in% all_trt]
    })

    # 最终治疗顺序
    treatment_order <- reactive({
      df <- league_table_raw()
      req(df)
      cur_trts <- get_original_order()

      new_order <- switch(input$sort_by,
                          "original" = cur_trts,
                          "sucra"    = get_sucra_order(),
                          "name"     = get_name_order(),
                          "custom"   = {
                            cd <- custom_order_dragged()
                            if (is.null(cd) || length(cd) == 0) return(cur_trts)
                            cd
                          }
      )

      if (is.null(new_order) || length(new_order) == 0) {
        return(cur_trts)
      }
      new_order <- c(new_order, setdiff(cur_trts, new_order))
      new_order
    })

    # 重置按钮
    observeEvent(input$reset_order, {
      orig <- get_original_order()
      if (input$sort_by == "custom") {
        shinyjqui::updateOrderInput(session, "custom_sort_list", items = orig)
      }
      updateSelectInput(session, "sort_by", selected = "original")
      showNotification("顺序已重置", type = "message", duration = 2)
    })

    observeEvent(get_original_order(), {
      req(input$sort_by == "custom")
      shinyjqui::updateOrderInput(session, "custom_sort_list", items = get_original_order())
    }, ignoreInit = TRUE)

    # ---------- 排序后的联赛表（支持对角线显示名称）----------
    reordered_league_table <- reactive({
      df <- league_table_raw()
      req(df)

      order <- treatment_order()
      current_treatments <- get_original_order()

      mat_original <- as.matrix(df[, -1, drop = FALSE])
      rownames(mat_original) <- current_treatments
      colnames(mat_original) <- current_treatments

      mat_reordered <- mat_original[order, order, drop = FALSE]

      if (!input$show_lower_tri) {
        mat_reordered[lower.tri(mat_reordered)] <- ""
      }

      # 设置对角线：若勾选“对角线显示治疗名称”，则用治疗名称，否则用 “1 (ref)” 等
      eff_type <- get_effect_type()
      if (input$show_diag_names) {
        diag(mat_reordered) <- order   # 直接用当前顺序的治疗名称
      } else {
        diag_val <- ifelse(eff_type %in% c("OR","RR","HR"), "1 (ref)", "0 (ref)")
        diag(mat_reordered) <- diag_val
      }

      df_reordered <- as.data.frame(mat_reordered, stringsAsFactors = FALSE)
      df_reordered <- cbind(Treatment = order, df_reordered)
      rownames(df_reordered) <- NULL
      return(df_reordered)
    })

    # ---------- 动态解读指南（含对角线模式的提示）----------
    output$dynamic_guide <- renderUI({
      eff_type <- get_effect_type()
      if (is.null(eff_type)) return(NULL)

      diag_tip <- if (input$show_diag_names) {
        div(style = "background-color: #d9edf7; padding: 8px; margin-bottom: 10px; border-left: 4px solid #31708f; border-radius: 4px;",
            p(strong("ℹ️ 提示："), "当前已隐藏行名和列名，请直接查看对角线上的治疗名称来识别对应的行列。")
        )
      } else NULL

      if (eff_type %in% c("OR", "RR", "HR")) {
        guide <- div(style = "background-color: #fcf8e3; padding: 12px; margin-bottom: 12px; border-left: 4px solid #f0ad4e; border-radius: 4px;",
                     p(strong("📌 快速解读指南（OR / RR / HR）"), style = "margin-bottom: 8px;"),
                     p("• 表中数字为 ", strong("比值比 (OR)"), "，已从 log OR 指数转换得到。"),
                     p("• ", strong("列治疗 vs 行治疗"), "：数值 > 1 表示列治疗的事件（如 pCR）发生率更高；数值 < 1 表示列治疗的事件发生率更低。"),
                     p("• ", strong("红色加粗"), " 表示 95% 可信区间不包含 1（统计学显著）。"),
                     p("• ", strong("计算过程"), "：贝叶斯模型估计 log OR 的后验中位数和 95% 区间，然后取指数（exp）得到 OR。")
        )
      } else {
        guide <- div(style = "background-color: #fcf8e3; padding: 12px; margin-bottom: 12px; border-left: 4px solid #f0ad4e; border-radius: 4px;",
                     p(strong("📌 快速解读指南（RD / MD）"), style = "margin-bottom: 8px;"),
                     p("• 表中数字为 ", strong("均数差 (MD) 或风险差 (RD)")),
                     p("• ", strong("列治疗 vs 行治疗"), "：数值 > 0 表示列治疗效果更优；数值 < 0 表示行治疗效果更优。"),
                     p("• ", strong("红色加粗"), " 表示 95% 可信区间不包含 0（统计学显著）。")
        )
      }

      tagList(diag_tip, guide)
    })

    # ---------- DT 表格输出（根据勾选隐藏行列标题）----------
    output$table <- renderDT({
      df <- reordered_league_table()
      req(df)

      eff_type <- get_effect_type()

      row_names <- as.character(df[[1]])
      data_matrix <- df[, -1, drop = FALSE]

      # 根据“对角线显示治疗名称”决定是否隐藏行列标题
      if (input$show_diag_names) {
        rownames_arg <- FALSE
        colnames_arg <- rep("", ncol(data_matrix))   # 空字符串作为列标题
      } else {
        rownames_arg <- row_names
        colnames_arg <- row_names
      }

      dt <- datatable(
        data_matrix,
        rownames = rownames_arg,
        colnames = colnames_arg,
        options = list(
          pageLength = 50,
          scrollX = TRUE,
          dom = 't',
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),
            list(width = '130px', targets = '_all')
          )
        )
      )

      # JS 回调：跳过对角线，使用统一的显著性条件
      js_code <- sprintf(
        "function(row, data, index) {
          for (var j = 0; j < data.length; j++) {
            if (j === index) continue;
            var cell = data[j];
            if (!cell || cell === '') continue;
            var match = cell.match(/\\((-?[0-9.]+),\\s*(-?[0-9.]+)\\)/);
            if (!match) continue;
            var lower = parseFloat(match[1]);
            var upper = parseFloat(match[2]);
            var isSignif = %s;
            if (isSignif) {
              $('td:eq(' + j + ')', row).css({'color': 'red', 'font-weight': 'bold'});
            }
          }
        }",
        signif_cond_text()
      )

      dt$x$options$rowCallback <- JS(js_code)
      dt
    })

    # ---------- 辅助函数：生成包含元信息的文件名 ----------
    # ---------- 辅助函数：生成包含元信息的文件名 ----------
    get_download_filename <- function(extension, prefix = "league_table") {
      # 获取结局
      out <- if (!is.null(outcome) && is.function(outcome)) {
        outcome()
      } else if (!is.null(outcome)) {
        outcome
      } else {
        NULL
      }
      if (is.null(out) || out == "") out <- "outcome"
      out_clean <- gsub("[^A-Za-z0-9]", "_", out)

      # ---- 新增：获取亚组信息 ----
      sub <- if (!is.null(subgroup) && is.function(subgroup)) {
        subgroup()
      } else {
        subgroup
      }
      #browser()
      sel <- if (!is.null(selected_levels) && is.function(selected_levels)) {
        selected_levels()
      } else {
        selected_levels
      }

      # 构建亚组后缀
      sub_suffix <- ""
      if (!is.null(sub) && sub != "Overall" && length(sel) == 1 && !is.null(sel) && sel != "") {
        # 亚组变量非 "Overall" 且只有一个选定水平时，添加该水平
        sub_suffix <- paste0("_", gsub("[^A-Za-z0-9]", "_", sel[1]))
      } else if (!is.null(sub) && sub != "Overall" && length(sel) > 1) {
        # 多个水平时使用 "multi" 作为标记（可根据需要调整）
        sub_suffix <- "_multi"
      }
      # 若亚组为 "Overall" 或 NULL，则不添加后缀

      # 获取方法信息
      if (!is.null(nma_info) && is.function(nma_info)) {
        info <- nma_info()
        method_family <- info$method_family %||% "unknown"
        model_type <- info$model_type %||% "unknown"
      } else if (!is.null(nma_info) && is.list(nma_info)) {
        method_family <- nma_info$method_family %||% "unknown"
        model_type <- nma_info$model_type %||% "unknown"
      } else {
        # 回退：从 nma_method 推断
        method <- nma_method()
        if (is.character(method)) {
          method_family <- ifelse(startsWith(method, "bayes"), "bayes", "freq")
          model_type <- ifelse(grepl("fixed", method, ignore.case = TRUE), "fixed", "random")
        } else {
          method_family <- "unknown"
          model_type <- "unknown"
        }
      }

      # 组合文件名：方法-结局[亚组]-模型-前缀_日期.扩展名
      paste0(method_family, "-", out_clean, "-", sub, "-", sub_suffix, "-", model_type, "-", prefix, "_", Sys.Date(), ".", extension)
    }

    # ---------- Excel 下载 ----------
    output$download <- downloadHandler(
      filename = function() {
        get_download_filename("xlsx")
      },
      content = function(file) {
        df <- reordered_league_table()
        if (is.null(df) || nrow(df) == 0) {
          wb <- createWorkbook()
          addWorksheet(wb, "Empty")
          writeData(wb, "Empty", data.frame(Message = "未生成联赛表"))
          saveWorkbook(wb, file, overwrite = TRUE)
          return()
        }

        eff_type <- get_effect_type()
        wb <- createWorkbook()

        # --- Sheet 1: League Table ---
        addWorksheet(wb, "League Table")
        writeData(wb, sheet = "League Table", x = df, startCol = 1, startRow = 1,
                  rowNames = FALSE, colNames = TRUE)


        # 【必须优先执行】先定义行列数，供后续的样式和列宽调用
        n_row <- nrow(df)
        n_col <- ncol(df)


        # ===== 【新增】表头旋转 -90 度，并垂直居中 =====
        headerStyle <- createStyle(textRotation = -90, halign = "center", valign = "center")
        addStyle(wb, sheet = "League Table", style = headerStyle, rows = 1, cols = 1:n_col, gridExpand = TRUE)
        # ==============================================

        boldRedStyle <- createStyle(fontColour = "#FF0000", textDecoration = "bold")


        if (n_col >= 2) {
          for (r in 1:n_row) {
            for (c in 2:n_col) {
              # 跳过对角线单元格（Excel 中行 r（数据行，从第2行开始）对应关系：r+1 == c 时为对角线）
              if (r == c - 1) next
              cell_value <- as.character(df[r, c])
              if (is_significant_cell(cell_value, eff_type)) {
                addStyle(wb, sheet = "League Table", style = boldRedStyle,
                         rows = r + 1, cols = c, gridExpand = TRUE)
              }
            }
          }
        }

        # ===== 【修改】列宽不再使用 auto，针对旋转后的表头设定固定宽度 =====
        # 第一列 (治疗名称) 宽 25，其余数据列宽 16 （您可调整这两个数值）
        col_widths <- c(25, rep(16, n_col - 1))
        setColWidths(wb, sheet = "League Table", cols = 1:n_col, widths = col_widths)
        # ================================================================

        # --- Sheet 2: Model Info ---
        addWorksheet(wb, "Model Info")
        res <- nma_result()
        method_text <- if (is.null(res)) "Unknown" else {
          paste(res$method_family, nma_method(), sep = " / ")
        }
        info_df <- data.frame(
          Item = c("效应量", "模型方法", "排序方式", "显示下三角", "对角线显示治疗名称", "生成时间"),
          Value = c(
            eff_type,
            method_text,
            input$sort_by,
            ifelse(input$show_lower_tri, "是", "否"),
            ifelse(input$show_diag_names, "是", "否"),
            as.character(Sys.time())
          ),
          stringsAsFactors = FALSE
        )
        writeData(wb, sheet = "Model Info", x = info_df, startCol = 1, startRow = 1,
                  rowNames = FALSE, colNames = TRUE)
        setColWidths(wb, sheet = "Model Info", cols = 1:2, widths = c(20, 40))

        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    # ---------- PDF 下载（使用 gt + webshot2）----------
    output$download_pdf <- downloadHandler(
      filename = function() {
        get_download_filename("pdf")
      },
      content = function(file) {
        df <- reordered_league_table()
        if (is.null(df) || nrow(df) == 0) {
          pdf(file, width = 8, height = 6)
          plot.new()
          text(0.5, 0.5, "No league table data available", cex = 1.5)
          dev.off()
          return()
        }

        eff_type <- get_effect_type()
        tryCatch({
          draw_league_table_pdf(df, eff_type, file)
        }, error = function(e) {
          pdf(file, width = 8, height = 6)
          plot.new()
          text(0.5, 0.5, paste("PDF generation failed:", e$message), cex = 1.2, col = "red")
          dev.off()
        })
      }
    )

    # ---------- 新增：LaTeX 下载 ----------
    output$download_latex <- downloadHandler(
      filename = function() {
        get_download_filename("tex")
      },
      content = function(file) {
        df <- reordered_league_table()
        if (is.null(df) || nrow(df) == 0) {
          # 生成空表格提示
          writeLines("\\begin{table}[htbp]\\centering No data\\end{table}", con = file)
          return()
        }

        eff_type <- get_effect_type()
        tryCatch({
          generate_latex_table(df, eff_type, file)
        }, error = function(e) {
          # 如果失败，写入错误信息
          writeLines(paste0("\\begin{table}[htbp]\\centering LaTeX generation failed: ", e$message, "\\end{table}"), con = file)
        })
      }
    )

    # ---------- 详细解读弹窗 ----------
    observeEvent(input$show_guide, {
      eff_type <- get_effect_type()
      invalid_line <- ifelse(eff_type %in% c("OR", "RR", "HR"), "1", "0")
      measure_label <- switch(eff_type,
                              OR = "OR",
                              RR = "RR",
                              RD = "RD",
                              HR = "HR",
                              eff_type)

      showModal(modalDialog(
        title = "📘 联赛表详细解读指南",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("关闭"),
        tags$div(
          h5("1. 对比方向"),
          p("上三角方向：", strong("列治疗 vs 行治疗")),
          p("• 列 = 比较组（实验组）"),
          p("• 行 = 参照组（对照组）"),
          h5("2. 红色加粗的含义"),
          p(strong(span("红色加粗", style = "color: red;")), paste0("表示该比较具有统计学显著性（", measure_label, " 的 95%CI 不包含 ", invalid_line, "）")),
          h5("3. RD 的特殊说明"),
          p("• 区间 (-0.22, -0.00) 包含 0，", strong("不标红")),
          p("• 区间 (-0.25, -0.05) 不包含 0，", strong(span("标红", style = "color: red;"))),
          h5("4. 上下三角关系"),
          if(eff_type %in% c("OR", "RR", "HR")) {
            p("下三角 = 1 / 上三角")
          } else {
            p("下三角 = - 上三角")
          },
          p("显著性在上下三角中一致。"),
          h5("5. 示例"),
          if(eff_type %in% c("OR", "RR", "HR")) {
            tags$pre("
上三角：          Drug A   Placebo
        Drug A    1.00     0.50
        Placebo            1.00
")
            p("解读：Drug A vs Placebo = 0.50，95%CI 不包含 1 → 显著（红色加粗）")
          } else {
            tags$pre("
上三角：          Drug A   Placebo
        Drug A    0.00    -0.15
        Placebo            0.00
")
            p("解读：Drug A vs Placebo = -0.15，95%CI 不包含 0 → 显著（红色加粗）")
          }
        )
      ))
    })

    return(list(data = reordered_league_table))
  })
}
