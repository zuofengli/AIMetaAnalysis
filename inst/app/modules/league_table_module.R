# modules/league_table_module.R
# 独立的联赛表模块 - 修复 RD 显著性判断逻辑
library(shiny)
library(DT)
library(dplyr)
library(tidyr)

# ===================== UI 模块 =====================
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

    # 动态解读指南
    uiOutput(ns("dynamic_guide")),

    fluidRow(
      column(4,
             selectInput(ns("sort_by"), "排序方式",
                         choices = c("原始顺序" = "original",
                                     "按SUCRA降序" = "sucra",
                                     "按治疗名称排序" = "name"),
                         selected = "original")
      ),
      column(3,
             checkboxInput(ns("show_lower_tri"), "显示下三角", value = FALSE)
      ),
      column(3,
             actionButton(ns("reset_order"), "重置顺序",
                          icon = icon("undo"), class = "btn-warning")
      ),
      column(2,
             downloadButton(ns("download"), "下载联赛表 (CSV)",
                            class = "btn-success pull-right")
      )
    ),
    verbatimTextOutput(ns("error_msg")),
    DTOutput(ns("table"))
  )
}

# ===================== Server 模块 =====================
leagueTableServer <- function(id,
                              nma_result,      # NMA 结果对象
                              rank_result,     # 排名结果对象
                              effect_type,     # 效应类型 ("OR", "RR", "HR", "RD", "MD")
                              nma_method      # NMA 方法 ("REML", "fixed", "NH")
) {
  moduleServer(id, function(input, output, session) {

    # 存储治疗组的顺序
    treatment_order <- reactiveVal(NULL)

    # 获取原始联赛表数据
    league_table_raw <- reactive({
      req(nma_result())

      if (is.null(nma_result()$setup)) {
        output$error_msg <- renderText("❌ 错误：NMA 结果尚未准备好")
        return(NULL)
      }

      # 根据效应类型确定 eform
      current_effect_type <- effect_type()
      eform_flag <- !(current_effect_type %in% c("RD", "MD"))

      league <- tryCatch(
        NMA::nmaleague(nma_result()$setup,
                       method = nma_method(),
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

        # 设置对角线
        mat_raw[cbind(seq_len(n_trt), seq_len(n_trt))] <- "1 (ref)"

        # 转换为数据框
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

    # 获取原始治疗组顺序
    get_original_order <- reactive({
      df <- league_table_raw()
      req(df)
      as.character(df[[1]])
    })

    # 根据 SUCRA 排序
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

    # 按治疗名称排序
    get_name_order <- reactive({
      order <- get_original_order()
      sort(order)
    })

    # 监听排序方式变化
    observeEvent(input$sort_by, {
      req(league_table_raw())
      new_order <- switch(input$sort_by,
                          "original" = get_original_order(),
                          "sucra" = get_sucra_order(),
                          "name" = get_name_order()
      )
      if(!is.null(new_order) && length(new_order) > 0) {
        treatment_order(new_order)
      }
    })

    # 重置顺序按钮
    observeEvent(input$reset_order, {
      treatment_order(get_original_order())
      updateSelectInput(session, "sort_by", selected = "original")
      showNotification("顺序已重置", type = "message", duration = 2)
    })

    # 重新排序联赛表
    reordered_league_table <- reactive({
      df <- league_table_raw()
      req(df)

      order <- treatment_order()
      if(is.null(order)) {
        order <- get_original_order()
        treatment_order(order)
      }

      current_treatments <- as.character(df[[1]])

      if(!all(current_treatments %in% order)) {
        new_treatments <- current_treatments[!current_treatments %in% order]
        order <- c(order, new_treatments)
      }

      order <- order[order %in% current_treatments]

      mat_original <- as.matrix(df[, -1])
      rownames(mat_original) <- current_treatments
      colnames(mat_original) <- current_treatments

      mat_reordered <- mat_original[order, order, drop = FALSE]

      if (!input$show_lower_tri) {
        mat_reordered[lower.tri(mat_reordered)] <- ""
      }

      diag(mat_reordered) <- "1 (ref)"

      df_reordered <- as.data.frame(mat_reordered, stringsAsFactors = FALSE)
      df_reordered <- cbind(Treatment = order, df_reordered)
      rownames(df_reordered) <- NULL

      return(df_reordered)
    })

    # ========== 核心修复：显著性判断函数 ==========
    is_cell_significant <- function(cell, eff_type) {
      # 空单元格或对角线不判断
      if (!is.character(cell) || cell == "1 (ref)" || cell == "" || is.na(cell)) {
        return(FALSE)
      }

      # 检查是否包含置信区间格式
      if (!grepl("\\([^,]+,\\s*[^)]+\\)", cell)) {
        return(FALSE)
      }

      # 提取数值和置信区间（支持负数、科学计数法）
      # 模式：数值 (下界, 上界)
      pattern <- "(-?[0-9.]+(?:e[+-]?[0-9]+)?)\\s*\\((-?[0-9.]+(?:e[+-]?[0-9]+)?),\\s*(-?[0-9.]+(?:e[+-]?[0-9]+)?)\\)"
      matches <- regmatches(cell, regexec(pattern, cell, perl = TRUE))

      if (length(matches[[1]]) != 4) {
        # 尝试简单模式
        pattern2 <- "(-?[0-9.]+)\\s*\\((-?[0-9.]+),\\s*(-?[0-9.]+)\\)"
        matches <- regmatches(cell, regexec(pattern2, cell, perl = TRUE))
        if (length(matches[[1]]) != 4) {
          return(FALSE)
        }
      }

      # 提取上下界
      lower <- suppressWarnings(as.numeric(matches[[1]][3]))
      upper <- suppressWarnings(as.numeric(matches[[1]][4]))

      if (is.na(lower) || is.na(upper)) {
        return(FALSE)
      }

      # 根据效应类型判断
      if (eff_type %in% c("OR", "RR", "HR")) {
        # 区间不包含 1 为显著
        # 使用容差避免浮点误差
        tol <- 1e-10
        contains_one <- (lower <= 1 + tol) && (upper >= 1 - tol)
        return(!contains_one)
      } else {
        # RD / MD：区间不包含 0 为显著
        # 关键：区间必须完全在 0 左侧或完全在 0 右侧
        # 使用容差处理 -0.00 和 0.00
        tol <- 1e-10
        is_positive_side <- lower > tol      # 整个区间 > 0
        is_negative_side <- upper < -tol     # 整个区间 < 0
        return(is_positive_side || is_negative_side)
      }
    }

    # 动态解读指南
    output$dynamic_guide <- renderUI({
      eff_type <- effect_type()
      if (is.null(eff_type)) return(NULL)

      if (eff_type %in% c("OR", "RR", "HR")) {
        div(style = "background-color: #fcf8e3; padding: 12px; margin-bottom: 12px; border-left: 4px solid #f0ad4e; border-radius: 4px;",
            p(strong("📌 快速解读指南："), style = "margin-bottom: 8px;"),
            p("• ", strong("列 (比较组)"), " = 分子/实验组"),
            p("• ", strong("行 (参照组)"), " = 分母/对照组"),
            p("• ", strong("数值 < 1"), " 表示列治疗优于行治疗"),
            p("• ", strong(span("红色加粗", style = "color: red;")), " 表示 95%CI 不包含 1（统计学显著）")
        )
      } else {
        div(style = "background-color: #fcf8e3; padding: 12px; margin-bottom: 12px; border-left: 4px solid #f0ad4e; border-radius: 4px;",
            p(strong("📌 快速解读指南："), style = "margin-bottom: 8px;"),
            p("• ", strong("列 (比较组)"), " = 分子/实验组"),
            p("• ", strong("行 (参照组)"), " = 分母/对照组"),
            p("• ", strong("数值 < 0"), " 表示列治疗优于行治疗"),
            p("• ", strong(span("红色加粗", style = "color: red;")), " 表示 95%CI 不包含 0（统计学显著）"),
            p("• ", strong("注意"), "：区间如 (-0.22, -0.00) 包含 0，不标红")
        )
      }
    })

    # 渲染联赛表
    output$table <- renderDT({
      df <- reordered_league_table()
      req(df)

      eff_type <- effect_type()
      if (is.null(eff_type)) eff_type <- "OR"

      row_names <- as.character(df[[1]])
      data_matrix <- df[, -1, drop = FALSE]

      n_rows <- nrow(data_matrix)
      n_cols <- ncol(data_matrix)

      # 预先计算每个单元格的显著性
      signif_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
      for(i in 1:n_rows) {
        for(j in 1:n_cols) {
          cell <- data_matrix[i, j]
          if(cell != "" && cell != "1 (ref)") {
            signif_matrix[i, j] <- is_cell_significant(cell, eff_type)
          }
        }
      }

      # 调试：打印显著单元格数量
      cat("显著单元格数量:", sum(signif_matrix), "/", n_rows * n_cols, "\n")

      # 创建 DT 表格
      dt <- datatable(
        data_matrix,
        rownames = row_names,
        colnames = row_names,
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

      # JS 实现真正的“单元格级”高亮
      dt <- dt %>% formatStyle(
        columns = colnames(data_matrix),
        valueColumns = colnames(data_matrix),
        backgroundColor = styleEqual("", "white") # 占位
      )

      # 用 JS override
      dt$x$options$rowCallback <- JS(
        "function(row, data, index) {",
        "  for (var j = 0; j < data.length; j++) {",
        "    var cell = data[j];",
        "",
        "    // 跳过空值和对角线",
        "    if (!cell || cell === '1 (ref)') continue;",
        "",
        "    var match = cell.match(/\\((-?[0-9.]+),\\s*(-?[0-9.]+)\\)/);",
        "    if (!match) continue;",
        "",
        "    var lower = parseFloat(match[1]);",
        "    var upper = parseFloat(match[2]);",
        "",
        "    var isSignif = false;",
        "",
        if (effect_type() %in% c("OR","RR","HR")) {
          "    isSignif = !(lower <= 1 && upper >= 1);"
        } else {
          "    isSignif = (lower > 0 || upper < 0);"
        },
        "",
        "    if (isSignif) {",
        "      $('td:eq(' + j + ')', row).css({'color': 'red', 'font-weight': 'bold'});",
        "    }",
        "  }",
        "}"
      )

      dt
    })

    # 下载功能
    output$download <- downloadHandler(
      filename = function() paste0("league_table_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- reordered_league_table()
        if (is.null(df)) {
          write.csv(data.frame(Message = "No league table available"), file, row.names = FALSE)
        } else {
          write.csv(df, file, row.names = FALSE)
        }
      }
    )

    # 详细解读指南（包含上下三角解释）
    observeEvent(input$show_guide, {
      eff_type <- effect_type()
      invalid_line <- ifelse(eff_type %in% c("OR", "RR", "HR"), "1", "0")

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
          p(strong(span("红色加粗", style = "color: red;")), "表示该比较具有统计学显著性"),
          p("判断标准：95% 置信区间", strong(span(paste0("不包含 ", invalid_line), style = "color: red;"))),

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
