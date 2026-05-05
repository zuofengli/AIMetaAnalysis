# modules/network_meta_module.R (修复条件显示问题)
library(shiny)
library(DT)
library(dplyr)
library(tidyr)

if (!requireNamespace("NMA", quietly = TRUE)) {
  stop("请安装 NMA 包：install.packages('NMA')")
}
library(NMA)

networkMetaUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("网络荟萃分析 (Network Meta-Analysis)"),
    fluidRow(
      column(3, selectInput(ns("nma_method"), "估计方法",
                            choices = c("限制性最大似然 (REML)" = "REML",
                                        "固定效应" = "fixed",
                                        "随机效应 (NH)" = "NH"),
                            selected = "REML")),
      column(3, uiOutput(ns("ref_trt_ui"))),
      column(3, uiOutput(ns("measure_ui"))),  # 动态显示二分类效应指标选择
      column(3, br(), actionButton(ns("run_nma"), "运行网络分析",
                                   icon = icon("play"), class = "btn-primary"))
    ),
    hr(),
    h4("预处理数据"),
    uiOutput(ns("data_type_hint")),  # 动态提示信息
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
    h4("原始输入数据预览"),
    DTOutput(ns("data_table"))
  )
}

networkMetaServer <- function(id, data_type, outcome, selected_studies, current_data, studies_info) {
  moduleServer(id, function(input, output, session) {

    # ---------- 动态 UI：数据类型提示 ----------
    output$data_type_hint <- renderUI({
      if (data_type() == "binary") {
        p("二分类数据：两行长格式（每个研究两行，试验组和对照组）")
      } else {
        p("生存数据：trans.armdata 转换后的长格式")
      }
    })

    # ---------- 动态 UI：效应指标选择（仅二分类显示）----------
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

    # ---------- 治疗信息映射 ----------
    treat_info <- reactive({
      req(studies_info)
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
        showNotification("studies_info 中缺少治疗相关列（Experimental/Comparator 或 treat1/treat2）", type = "error")
        return(NULL)
      }
      studies_info %>%
        select(unique_id, treat1 = !!sym(exp_col), treat2 = !!sym(comp_col)) %>%
        distinct(unique_id, .keep_all = TRUE)
    })

    # ---------- 所有治疗名称 ----------
    all_treatments <- reactive({
      info <- treat_info()
      req(info)
      unique(c(info$treat1, info$treat2))
    })

    # ---------- 参考治疗 UI ----------
    output$ref_trt_ui <- renderUI({
      req(all_treatments())
      default_ref <- if ("Chemotherapy" %in% all_treatments()) "Chemotherapy" else all_treatments()[1]
      selectInput(session$ns("ref_trt"), "参考治疗", choices = all_treatments(), selected = default_ref)
    })

    # ---------- 原始数据预览 ----------
    output$data_table <- renderDT({
      dat <- current_data()
      req(dat)
      if (nrow(dat) == 0) return(NULL)
      datatable(dat, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    # ---------- 准备 NMA 数据 ----------
    nma_data <- reactive({
      req(current_data(), treat_info())
      study_ids <- selected_studies()
      if (is.null(study_ids)) return(NULL)

      dat <- current_data() %>% dplyr::filter(unique_id %in% study_ids)
      if (nrow(dat) < 2) {
        showNotification("至少需要 2 项研究才能进行网络荟萃分析", type = "error")
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
          dat %>%
            transmute(
              study = study_display,
              trt = treat1,
              n = n_ici,
              d = event_ici
            ),
          dat %>%
            transmute(
              study = study_display,
              trt = treat2,
              n = n_ctrl,
              d = event_ctrl
            )
        ) %>%
          dplyr::filter(!is.na(trt), !is.na(n), !is.na(d), n > 0, d >= 0, d <= n)

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
          showNotification("生存数据中存在无效的 te 或 seTE (NA/Inf)", type = "error")
          return(NULL)
        }

        tryCatch({
          trans_data <- NMA::trans.armdata(
            study = study_display,
            treat1 = treat1,
            treat2 = treat2,
            n1 = n1,
            n2 = n2,
            y = te,
            SE = se_te,
            measure = "logHR",
            data = dat
          )
          list(type = "survival", data = trans_data, measure = "RD")  # measure 在 survival 中实际用不到
        }, error = function(e) {
          showNotification(paste("trans.armdata 转换失败：", e$message), type = "error")
          NULL
        })
      }
    })

    # ---------- 预处理数据表 ----------
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

    # ---------- 运行 NMA ----------
    nma_result <- eventReactive(input$run_nma, {
      nd <- nma_data()
      req(nd)
      ref_trt <- input$ref_trt

      if (nd$type == "binary") {
        setup_obj <- tryCatch({
          NMA::setup(
            study = study,
            trt = trt,
            d = d,
            n = n,
            measure = nd$measure,
            ref = ref_trt,
            data = nd$data
          )
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
        list(result = res, setup = setup_obj, type = "binary")

      } else {
        setup_obj <- tryCatch({
          NMA::setup(
            study = study,
            trt = trt,
            d = d,
            n = n,
            measure = nd$measure,
            ref = ref_trt,
            data = nd$data
          )
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
        list(result = res, setup = setup_obj, type = "survival")
      }
    })

    output$netplot <- renderPlot({
      req(nma_result())
      NMA::netplot(nma_result()$setup)
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

    meta_result <- reactive({ nma_result()$result })
    model <- reactive({ input$nma_method })
    return(list(meta_result = meta_result, model = model))
  })
}
