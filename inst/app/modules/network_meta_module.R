# modules/network_meta_module.R (修复版 - 确保UI显示)
library(shiny)
library(DT)
library(dplyr)
library(tidyr)

if (!requireNamespace("NMA", quietly = TRUE)) {
  stop("请安装 NMA 包：install.packages('NMA')")
}
library(NMA)

# ===================== UI 模块 =====================
networkMetaUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("网络荟萃分析 (Network Meta-Analysis)"),

    # 绿色配置面板 - 移到了最前面确保可见
    div(
      class = "well",
      style = "background-color: #d9edf7; padding: 15px; margin-bottom: 15px; border-left: 4px solid #5cb85c;",
      h5("🔧 治疗组列映射设置", style = "color: #3c763d; margin-top: 0;"),
      fluidRow(
        column(5, uiOutput(ns("exp_col_ui"))),
        column(5, uiOutput(ns("comp_col_ui"))),
        column(2, br(), actionButton(ns("apply_col_mapping"), "应用映射",
                                     icon = icon("sync"), class = "btn-info btn-sm"))
      ),
      p(class = "text-muted", style = "margin-bottom: 0; margin-top: 8px;",
        "💡 提示：系统会自动检测并推荐列名，您也可以手动选择其他列。")
    ),

    # 原有配置区域
    fluidRow(
      column(3, selectInput(ns("nma_method"), "估计方法",
                            choices = c("限制性最大似然 (REML)" = "REML",
                                        "固定效应" = "fixed",
                                        "随机效应 (NH)" = "NH"),
                            selected = "REML")),
      column(3, uiOutput(ns("ref_trt_ui"))),
      column(3, uiOutput(ns("measure_ui"))),
      column(3, br(), actionButton(ns("run_nma"), "运行网络分析",
                                   icon = icon("play"), class = "btn-primary"))
    ),

    fluidRow(
      column(3, checkboxInput(ns("show_pop_size"), "在节点上显示样本量", value = TRUE)),
      column(9, textOutput(ns("nma_status")))
    ),
    hr(),
    h4("预处理数据"),
    uiOutput(ns("data_type_hint")),
    DTOutput(ns("pretable")),
    hr(),
    h4("网络图 (Network Plot)"),
    plotOutput(ns("netplot"), height = "500px"),
    br(),
    h4("NMA 森林图 (Forest Plot)"),
    plotOutput(ns("nmaforest"), height = "600px"),
    br(),
    h4("分析结果摘要"),
    verbatimTextOutput(ns("nma_summary")),
    hr(),
    h4("治疗排名 (SUCRA)"),
    uiOutput(ns("rank_output_ui")),
    hr(),

    # ========== 联赛表区域 ==========
    h4("联赛表 (League Table)"),
    div(
      class = "well",
      style = "background-color: #f5f5f5; padding: 10px; margin-bottom: 15px; border-left: 4px solid #337ab7;",
      h5("📖 如何解读联赛表？"),
      tags$ul(
        tags$li(HTML("🔍 <strong>上三角展示结果</strong>，方向：<strong>行治疗 vs 列治疗</strong>（行/列 或 行−列；行治疗为分子/被比较组，列治疗为分母/参照组）。")),
        tags$li("📊 **效应量判断**：若为 OR/RR/HR，看区间是否包含 1；若为 RD/MD，看区间是否包含 0。"),
        tags$li("🎯 **红色加粗**表示该比较具有统计学显著性。"),
        tags$li("⚖️ **下三角已清空**，上三角值互为倒数（OR/RR/HR）或相反数（MD/RD）—— 高分期刊常规做法。")
      ),
      actionLink(ns("show_league_guide"), "📘 点击查看详细解读案例", icon = icon("book"))
    ),
    fluidRow(
      column(6, p(strong("对比方向："), "行治疗 vs 列治疗（行治疗为分子/被比较组，列治疗为分母/参照组）")),
      column(6, downloadButton(ns("download_league"), "下载联赛表 (CSV)", class = "btn-success pull-right"))
    ),
    verbatimTextOutput(ns("league_error")),
    DTOutput(ns("league_table")),
    hr(),

    h4("原始输入数据预览"),
    DTOutput(ns("data_table"))
  )
}

# ===================== Server 模块 =====================
networkMetaServer <- function(id, data_type, outcome, selected_studies, current_data, studies_info) {
  moduleServer(id, function(input, output, session) {

    # 存储用户选择的列映射
    col_mapping <- reactiveValues(
      exp_col = NULL,
      comp_col = NULL
    )

    # 自动检测可能的 Experimental 列
    detect_exp_columns <- reactive({
      req(studies_info)
      all_cols <- names(studies_info)

      # 定义匹配模式（按优先级）
      patterns <- c(
        "^Experimental$",
        "^experimental$",
        "^treat1$",
        "^Experimental_Test_\\d+$",
        "^Exp_\\d+$",
        "^Treatment$",
        "^Intervention$"
      )

      matched <- c()
      for (pattern in patterns) {
        matched <- c(matched, grep(pattern, all_cols, value = TRUE, ignore.case = FALSE))
      }

      # 去重并保持唯一
      matched <- unique(matched)

      # 如果没有匹配到，提供所有列供选择
      if (length(matched) == 0) {
        matched <- all_cols
      }

      return(matched)
    })

    # 自动检测可能的 Comparator 列
    detect_comp_columns <- reactive({
      req(studies_info)
      all_cols <- names(studies_info)

      # 定义匹配模式（按优先级）
      patterns <- c(
        "^Comparator$",
        "^comparator$",
        "^treat2$",
        "^Comparator_Test_\\d+$",
        "^Comp_\\d+$",
        "^Control$",
        "^Placebo$"
      )

      matched <- c()
      for (pattern in patterns) {
        matched <- c(matched, grep(pattern, all_cols, value = TRUE, ignore.case = FALSE))
      }

      # 去重并保持唯一
      matched <- unique(matched)

      # 如果没有匹配到，提供所有列供选择
      if (length(matched) == 0) {
        matched <- all_cols
      }

      return(matched)
    })

    # 渲染 Experimental 列选择器
    output$exp_col_ui <- renderUI({
      req(studies_info)
      exp_options <- detect_exp_columns()

      # 确定默认值
      default_exp <- NULL
      if ("Experimental" %in% exp_options) {
        default_exp <- "Experimental"
      } else if ("experimental" %in% exp_options) {
        default_exp <- "experimental"
      } else if ("treat1" %in% exp_options) {
        default_exp <- "treat1"
      } else if (length(exp_options) > 0) {
        default_exp <- exp_options[1]
      }

      selectInput(session$ns("selected_exp_col"),
                  label = tagList(icon("flask"), "Experimental 列 (治疗组)"),
                  choices = exp_options,
                  selected = default_exp)
    })

    # 渲染 Comparator 列选择器
    output$comp_col_ui <- renderUI({
      req(studies_info)
      comp_options <- detect_comp_columns()

      # 确定默认值
      default_comp <- NULL
      if ("Comparator" %in% comp_options) {
        default_comp <- "Comparator"
      } else if ("comparator" %in% comp_options) {
        default_comp <- "comparator"
      } else if ("treat2" %in% comp_options) {
        default_comp <- "treat2"
      } else if (length(comp_options) > 0) {
        default_comp <- comp_options[1]
      }

      selectInput(session$ns("selected_comp_col"),
                  label = tagList(icon("chart-line"), "Comparator 列 (对照组)"),
                  choices = comp_options,
                  selected = default_comp)
    })

    # 应用列映射
    observeEvent(input$apply_col_mapping, {
      req(input$selected_exp_col, input$selected_comp_col)

      col_mapping$exp_col <- input$selected_exp_col
      col_mapping$comp_col <- input$selected_comp_col

      showNotification(
        paste("✅ 已应用映射：", col_mapping$exp_col, "→ 治疗组，",
              col_mapping$comp_col, "→ 对照组"),
        type = "message",  # ✅ 修复：使用 "message"
        duration = 3
      )
    })

    output$nma_status <- renderText({
      if (is.null(nma_result())) {
        "状态：尚未运行 NMA（请点击\"运行网络分析\"按钮）"
      } else {
        "状态：NMA 已运行成功"
      }
    })

    output$data_type_hint <- renderUI({
      if (data_type() == "binary") {
        p("二分类数据：两行长格式")
      } else {
        p("生存数据：trans.armdata 转换后的长格式")
      }
    })

    output$measure_ui <- renderUI({
      if (data_type() == "binary") {
        selectInput(session$ns("binary_measure"), "效应指标 (二分类)",
                    choices = c("风险差 (RD)" = "RD",
                                "风险比 (OR)" = "OR",
                                "相对风险 (RR)" = "RR"),
                    selected = "RD")
      } else {
        NULL
      }
    })

    # 修改 treat_info 函数，使用用户选择的列映射
    treat_info <- reactive({
      req(studies_info)

      # 优先使用用户选择的列
      if (!is.null(col_mapping$exp_col) && !is.null(col_mapping$comp_col)) {
        exp_col <- col_mapping$exp_col
        comp_col <- col_mapping$comp_col

        # 验证列是否存在
        if (!(exp_col %in% names(studies_info))) {
          showNotification(paste("选定的 Experimental 列 '", exp_col, "' 不存在于数据中"), type = "error")
          return(NULL)
        }
        if (!(comp_col %in% names(studies_info))) {
          showNotification(paste("选定的 Comparator 列 '", comp_col, "' 不存在于数据中"), type = "error")
          return(NULL)
        }
      } else {
        # 如果没有用户选择，使用自动检测逻辑
        exp_col <- NULL
        comp_col <- NULL

        if ("Experimental" %in% names(studies_info) && "Comparator" %in% names(studies_info)) {
          exp_col <- "Experimental"
          comp_col <- "Comparator"
        } else if ("experimental" %in% names(studies_info) && "comparator" %in% names(studies_info)) {
          exp_col <- "experimental"
          comp_col <- "comparator"
        } else if ("treat1" %in% names(studies_info) && "treat2" %in% names(studies_info)) {
          exp_col <- "treat1"
          comp_col <- "treat2"
        } else {
          showNotification("studies_info 中缺少治疗相关列，请手动选择", type = "warning")
          return(NULL)
        }
      }

      # 检查 studies_info 是否有 unique_id 列
      if (!"unique_id" %in% names(studies_info)) {
        showNotification("studies_info 必须包含 'unique_id' 列", type = "error")
        return(NULL)
      }

      # 提取并重命名
      result <- studies_info %>%
        dplyr::select(unique_id, treat1 = !!sym(exp_col), treat2 = !!sym(comp_col)) %>%
        dplyr::distinct(unique_id, .keep_all = TRUE)

      return(result)
    })

    all_treatments <- reactive({
      info <- treat_info()
      req(info)
      unique(c(info$treat1, info$treat2))
    })

    output$ref_trt_ui <- renderUI({
      req(all_treatments())
      default_ref <- if ("Chemotherapy" %in% all_treatments()) "Chemotherapy" else all_treatments()[1]
      selectInput(session$ns("ref_trt"), "参考治疗", choices = all_treatments(), selected = default_ref)
    })

    output$data_table <- renderDT({
      dat <- current_data()
      req(dat)
      if (nrow(dat) == 0) return(NULL)
      datatable(dat, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    nma_data <- reactive({
      req(current_data(), treat_info())
      study_ids <- selected_studies()
      if (is.null(study_ids)) return(NULL)

      dat <- current_data()
      if (!is.data.frame(dat)) {
        if (is.list(dat)) {
          if (!is.null(dat$data.ab) && is.data.frame(dat$data.ab)) {
            dat <- dat$data.ab
          } else if (!is.null(dat$data) && is.data.frame(dat$data)) {
            dat <- dat$data
          } else {
            dat <- tryCatch(as.data.frame(dat), error = function(e) NULL)
          }
        } else {
          dat <- tryCatch(as.data.frame(dat), error = function(e) NULL)
        }
      }
      req(is.data.frame(dat))

      dat <- dat %>% dplyr::filter(unique_id %in% study_ids)
      if (nrow(dat) < 2) {
        showNotification("至少需要 2 项研究", type = "error")
        return(NULL)
      }

      info <- treat_info()
      dat <- dat %>%
        left_join(info, by = "unique_id") %>%
        dplyr::filter(!is.na(treat1) & !is.na(treat2))

      if (nrow(dat) == 0) {
        showNotification("合并治疗信息后无有效数据", type = "error")
        return(NULL)
      }

      if (data_type() == "binary") {
        long_data <- bind_rows(
          dat %>% transmute(study = study_display, trt = treat1, n = n_ici, d = event_ici),
          dat %>% transmute(study = study_display, trt = treat2, n = n_ctrl, d = event_ctrl)
        ) %>% dplyr::filter(!is.na(trt), !is.na(n), !is.na(d), n > 0, d >= 0, d <= n)

        if (nrow(long_data) == 0) {
          showNotification("二分类数据转换后无有效记录", type = "error")
          return(NULL)
        }
        list(type = "binary", data = long_data, measure = input$binary_measure)
      } else {
        if (!all(c("te", "se_te", "n1", "n2") %in% names(dat))) {
          showNotification("生存数据缺少 te, se_te, n1, n2 列", type = "error")
          return(NULL)
        }
        dat <- dat %>% dplyr::filter(!is.na(te) & !is.na(se_te) & is.finite(te) & is.finite(se_te))
        if (nrow(dat) == 0) {
          showNotification("生存数据中存在无效的 te 或 seTE", type = "error")
          return(NULL)
        }
        tryCatch({
          trans_data <- NMA::trans.armdata(study = study_display, treat1 = treat1, treat2 = treat2,
                                           n1 = n1, n2 = n2, y = te, SE = se_te, measure = "logHR", data = dat)
          list(type = "survival", data = trans_data, measure = "RD")
        }, error = function(e) {
          showNotification(paste("trans.armdata 转换失败：", e$message), type = "error")
          NULL
        })
      }
    })

    output$pretable <- renderDT({
      nd <- nma_data()
      req(nd)
      if (nd$type == "binary") {
        df <- nd$data
        datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
                  colnames = c("研究", "治疗组", "样本量 (n)", "事件数 (d)"))
      } else {
        df <- nd$data
        if (is.data.frame(df)) {
          datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        } else {
          datatable(as.data.frame(df), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
        }
      }
    })

    nma_result <- eventReactive(input$run_nma, {
      nd <- nma_data()
      req(nd)
      ref_trt <- input$ref_trt

      if (nd$type == "binary") {
        setup_obj <- tryCatch({
          NMA::setup(study = study, trt = trt, d = d, n = n, measure = nd$measure, ref = ref_trt, data = nd$data)
        }, error = function(e) {
          showNotification(paste("setup 失败：", e$message), type = "error")
          NULL
        })
        if (is.null(setup_obj)) return(NULL)

        res <- tryCatch({
          NMA::nma(setup_obj, method = input$nma_method, eform = (nd$measure != "RD"))
        }, error = function(e) {
          showNotification(paste("NMA 运行失败：", e$message), type = "error")
          NULL
        })
        list(result = res, setup = setup_obj, type = "binary", measure = nd$measure)
      } else {
        setup_obj <- tryCatch({
          NMA::setup(study = study, trt = trt, d = d, n = n, measure = nd$measure, ref = ref_trt, data = nd$data)
        }, error = function(e) {
          showNotification(paste("setup 失败：", e$message), type = "error")
          NULL
        })
        if (is.null(setup_obj)) return(NULL)

        res <- tryCatch({
          NMA::nma(setup_obj, method = input$nma_method, eform = TRUE)
        }, error = function(e) {
          showNotification(paste("NMA 运行失败：", e$message), type = "error")
          NULL
        })
        list(result = res, setup = setup_obj, type = "survival", measure = nd$measure)
      }
    })

    output$netplot <- renderPlot({
      req(nma_result())
      setup_obj <- nma_result()$setup
      tryCatch({
        NMA::netplot(setup_obj, text = TRUE, base.cex = 1.2)
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = paste("网络图绘制失败:\n", e$message))
      })
    })

    output$nmaforest <- renderPlot({
      req(nma_result())
      NMA::nmaforest(nma_result()$setup, method = input$nma_method)
    })

    output$nma_summary <- renderPrint({
      res <- nma_result()
      if (is.null(res) || is.null(res$result)) {
        cat("未获得有效结果。")
      } else {
        print(res$result)
      }
    })

    rank_result <- reactive({
      res <- nma_result()
      if (is.null(res) || is.null(res$result)) return(NULL)
      setup_obj <- res$setup

      trt_names <- NULL
      if (!is.null(setup_obj$trt.id)) {
        trt_names <- names(setup_obj$trt.id)
      } else if (!is.null(setup_obj$data) && is.data.frame(setup_obj$data) && "trt" %in% names(setup_obj$data)) {
        trt_names <- sort(unique(setup_obj$data$trt))
      } else {
        trt_names <- all_treatments()
      }
      if (is.null(trt_names) || length(trt_names) == 0) trt_names <- paste0("Tx", seq_len(length(rank$SUCRA)))

      rank <- tryCatch({
        NMA::nmarank(res$result, method = input$nma_method)
      }, error = function(e) {
        tryCatch({
          NMA::nmarank(setup_obj, method = input$nma_method)
        }, error = function(e2) {
          showNotification(paste("排名计算失败:", e2$message), type = "warning")
          NULL
        })
      })
      if (is.null(rank)) return(NULL)

      make_rank_df <- function(vec, col_name) {
        vec <- as.numeric(vec)
        if (length(vec) == length(trt_names)) {
          names(vec) <- trt_names
        } else {
          if (is.null(names(vec)) || any(names(vec) %in% as.character(1:length(vec)))) {
            names(vec) <- trt_names[seq_along(vec)]
          }
        }
        df <- data.frame(Treatment = names(vec), Value = vec, stringsAsFactors = FALSE)
        names(df)[2] <- col_name
        df <- df[order(-df[[col_name]]), ]
        df
      }

      if (is.list(rank) && !is.data.frame(rank)) {
        if ("SUCRA" %in% names(rank)) return(make_rank_df(rank$SUCRA, "SUCRA"))
        if ("Pbest" %in% names(rank)) return(make_rank_df(rank$Pbest, "P_best"))
        return(rank)
      } else if (is.data.frame(rank)) {
        if ("Treatment" %in% colnames(rank) && is.numeric(rank$Treatment))
          rank$Treatment <- trt_names[rank$Treatment]
        if ("trt" %in% colnames(rank) && is.numeric(rank$trt)) {
          rank$Treatment <- trt_names[rank$trt]
          rank$trt <- NULL
        }
        if ("SUCRA" %in% colnames(rank)) rank <- rank[order(-rank$SUCRA), ]
        if ("P_best" %in% colnames(rank)) rank <- rank[order(-rank$P_best), ]
        return(rank)
      } else if (is.numeric(rank)) {
        return(make_rank_df(rank, "Score"))
      }
      NULL
    })

    output$rank_output_ui <- renderUI({
      rank <- rank_result()
      if (is.null(rank)) return(p("无法获取排名结果，请检查 NMA 模型是否成功运行。"))
      if (is.data.frame(rank) && nrow(rank) > 0) {
        DTOutput(session$ns("rank_table"))
      } else {
        verbatimTextOutput(session$ns("rank_text"))
      }
    })

    output$rank_table <- renderDT({
      rank <- rank_result()
      req(is.data.frame(rank) && nrow(rank) > 0)
      datatable(rank, options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
        formatRound(columns = which(sapply(rank, is.numeric)), digits = 3)
    })

    output$rank_text <- renderPrint({
      rank <- rank_result()
      cat("=== 治疗排名信息 ===\n")
      if (is.null(rank)) cat("未能计算排名。") else print(rank)
    })

    # ========== 联赛表（nmaleague 解析 + 上三角） ==========
    observeEvent(input$show_league_guide, {
      showModal(modalDialog(
        title = "📘 联赛表详细解读指南",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("关闭"),
        tags$div(
          h5("1. 对比方向"),
          p("上三角为：行治疗 vs 列治疗。行治疗为分子/被比较组，列治疗为分母/参照组。"),
          h5("2. 显著性判断"),
          p("• OR/RR/HR：区间不包含 1 为显著；RD/MD：区间不包含 0 为显著。"),
          h5("3. 示例"),
          tags$table(class = "table table-bordered",
                     tags$thead(tags$tr(tags$th(""), tags$th("药物A"), tags$th("药物B"), tags$th("安慰剂"))),
                     tags$tbody(
                       tags$tr(tags$td("药物A"), tags$td("1 (ref)"), tags$td("1.50 (0.80, 2.20)"), tags$td("3.20 (2.10, 4.30)")),
                       tags$tr(tags$td("药物B"), tags$td(""), tags$td("1 (ref)"), tags$td("1.70 (1.10, 2.30)")),
                       tags$tr(tags$td("安慰剂"), tags$td(""), tags$td(""), tags$td("1 (ref)"))
                     )
          ),
          p("药物A vs 安慰剂：3.20 (2.10, 4.30) → 显著；药物A vs 药物B：1.50 (0.80, 2.20) → 不显著。"),
          h5("4. 为什么有负值？"),
          p("当指标为风险差 (RD) 或均值差 (MD) 时，数值可正可负，以 0 为无效线。这是完全正确的。")
        )
      ))
    })

    league_table <- reactive({
      res <- nma_result()
      if (is.null(res) || is.null(res$setup)) {
        output$league_error <- renderText("❌ 错误：NMA 结果尚未准备好，请先运行网络分析。")
        return(NULL)
      }

      eform_flag <- TRUE
      if (res$type == "binary" && !is.null(res$measure) && res$measure == "RD") {
        eform_flag <- FALSE
      }

      league <- tryCatch(
        NMA::nmaleague(res$setup, method = input$nma_method, eform = eform_flag),
        error = function(e) {
          output$league_error <- renderText(paste("❌ nmaleague 运行失败：", conditionMessage(e)))
          NULL
        }
      )

      if (is.null(league)) return(NULL)

      if (is.list(league) && !is.data.frame(league) && !is.null(league$R) && is.matrix(league$R)) {
        mat_raw <- league$R
        trt_names <- diag(mat_raw)
        n_trt <- length(trt_names)
        if (n_trt < 2) return(NULL)

        # 对角线设置为 "1 (ref)"
        mat_raw[cbind(seq_len(n_trt), seq_len(n_trt))] <- "1 (ref)"
        league_df <- as.data.frame(mat_raw, stringsAsFactors = FALSE)
        colnames(league_df) <- trt_names
        league_df <- cbind(Treatment = trt_names, league_df)
        rownames(league_df) <- NULL

        # 只保留上三角（下三角清空）
        m <- as.matrix(league_df[, -1])
        m[lower.tri(m)] <- ""
        league_df[, -1] <- m

        output$league_error <- renderText("")
        return(league_df)
      } else {
        output$league_error <- renderText("❌ 联赛表格式异常，请联系开发者。")
        return(NULL)
      }
    })

    get_effect_type <- reactive({
      res <- nma_result()
      if (is.null(res)) return("OR")
      if (res$type == "binary") {
        if (!is.null(res$measure) && res$measure == "RD") return("RD")
        else return("OR")
      } else {
        return("HR")
      }
    })

    output$league_table <- renderDT({
      df <- league_table()
      req(df)

      eff_type <- get_effect_type()
      threshold <- if (eff_type %in% c("OR", "RR", "HR")) 1 else 0

      signif_col <- rep(FALSE, ncol(df))
      for (j in 2:ncol(df)) {
        any_signif <- FALSE
        for (i in 1:nrow(df)) {
          cell <- df[i, j]
          if (is.character(cell) && cell != "1 (ref)" && cell != "" && grepl("\\(.+,.+\\)", cell)) {
            matches <- regmatches(cell, regexec("\\(([0-9.]+),\\s*([0-9.]+)\\)", cell))
            if (length(matches[[1]]) == 3) {
              lower <- as.numeric(matches[[1]][2])
              upper <- as.numeric(matches[[1]][3])
              if (!is.na(lower) && !is.na(upper)) {
                if ((threshold == 1 && (lower > 1 || upper < 1)) ||
                    (threshold == 0 && (lower > 0 || upper < 0))) {
                  any_signif <- TRUE
                  break
                }
              }
            }
          }
        }
        signif_col[j] <- any_signif
      }

      dt <- datatable(df, options = list(pageLength = 50, scrollX = TRUE), rownames = FALSE)
      for (j in 2:ncol(df)) {
        if (signif_col[j]) {
          dt <- dt %>% formatStyle(j, color = "red", fontWeight = "bold")
        }
      }
      dt
    })

    output$download_league <- downloadHandler(
      filename = function() paste0("league_table_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- league_table()
        if (is.null(df)) {
          write.csv(data.frame(Message = "No league table available"), file, row.names = FALSE)
        } else {
          write.csv(df, file, row.names = FALSE)
        }
      }
    )

    meta_result <- reactive({ nma_result()$result })
    model <- reactive({ input$nma_method })
    return(list(meta_result = meta_result, model = model))
  })
}
