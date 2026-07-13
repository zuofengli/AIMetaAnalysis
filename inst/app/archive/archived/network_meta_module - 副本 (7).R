# modules/network_meta_module.R
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(coda)

if (!requireNamespace("NMA", quietly = TRUE)) {
  stop("请安装 NMA 包：install.packages('NMA')")
}
if (!requireNamespace("gemtc", quietly = TRUE)) {
  stop("请安装 gemtc 包：install.packages('gemtc')")
}
if (!requireNamespace("coda", quietly = TRUE)) {
  stop("请安装 coda 包：install.packages('coda')")
}

library(NMA)

source("modules/league_table_module.R")

# ========== 统一的治疗名称清洗函数 ==========
clean_treatment_unified <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_]", "_", x)   # 非法字符 → 下划线
  x <- gsub("_+", "_", x)              # 合并连续下划线
  x <- gsub("^_|_$", "", x)           # 去除首尾下划线
  trimws(x)
}

# ========== 频率学派 NMA 核心函数 ==========
run_nma_frequentist <- function(nd, ref_trt, method_choice) {
  method <- switch(method_choice,
                   freq_REML = "REML",
                   freq_fixed = "fixed",
                   freq_NH = "NH",
                   "REML")

  nd$data$trt <- clean_treatment_unified(nd$data$trt)
  ref_trt <- clean_treatment_unified(ref_trt)

  if (nd$type == "binary") {
    setup_obj <- tryCatch({
      NMA::setup(study = study, trt = trt, d = d, n = n,
                 measure = nd$measure, ref = ref_trt, data = nd$data)
    }, error = function(e) {
      showNotification(paste("频率学派 setup 失败：", e$message), type = "error")
      return(NULL)
    })
    if (is.null(setup_obj)) return(NULL)

    res <- tryCatch({
      NMA::nma(setup_obj, method = method, eform = (nd$measure != "RD"))
    }, error = function(e) {
      showNotification(paste("频率学派 NMA 运行失败：", e$message), type = "error")
      return(NULL)
    })
    if (is.null(res)) return(NULL)

    return(list(result = res, setup = setup_obj, type = "binary",
                measure = nd$measure, method_family = "frequentist"))
  }
  else {  # survival
    setup_obj <- tryCatch({
      NMA::setup(study = study, trt = trt, d = d, n = n,
                 measure = nd$measure, ref = ref_trt, data = nd$data)
    }, error = function(e) {
      showNotification(paste("频率学派 setup 失败：", e$message), type = "error")
      return(NULL)
    })
    if (is.null(setup_obj)) return(NULL)

    res <- tryCatch({
      NMA::nma(setup_obj, method = method, eform = TRUE)
    }, error = function(e) {
      showNotification(paste("频率学派 NMA 运行失败：", e$message), type = "error")
      return(NULL)
    })
    if (is.null(res)) return(NULL)

    return(list(result = res, setup = setup_obj, type = "survival",
                measure = nd$measure, method_family = "frequentist"))
  }
}

# ========== 贝叶斯 NMA 核心函数（支持固定/随机效应） ==========
run_nma_bayesian <- function(nd, ref_trt, method_choice) {
  if (method_choice == "bayes_fixed") {
    linearModel <- "fixed"
  } else {
    linearModel <- "random"
  }

  nd$data$trt <- clean_treatment_unified(nd$data$trt)
  ref_trt <- clean_treatment_unified(ref_trt)

  if (nd$type != "binary") {
    showNotification("贝叶斯 NMA 当前仅支持二分类数据", type = "error")
    return(NULL)
  }

  if (nd$measure == "RD") {
    showNotification("⚠️ 贝叶斯 NMA 不支持 RD，已自动切换为 OR", type = "warning")
    nd$measure <- "OR"
  }

  data_ab <- nd$data %>%
    transmute(
      study = as.character(study),
      treatment = trt,
      responders = round(d),
      sampleSize = round(n)
    ) %>%
    dplyr::filter(
      !is.na(treatment),
      !is.na(responders),
      !is.na(sampleSize),
      responders >= 0,
      sampleSize > 0,
      responders <= sampleSize
    )

  treatments_present <- unique(data_ab$treatment)
  if (length(treatments_present) < 2) {
    showNotification("至少需要两个治疗", type = "error")
    return(NULL)
  }

  if (!(ref_trt %in% treatments_present)) {
    ref_trt <- treatments_present[1]
  }

  treatments_df <- data.frame(
    id = treatments_present,
    description = treatments_present,
    stringsAsFactors = FALSE
  )

  network <- tryCatch(
    gemtc::mtc.network(
      data.ab = data_ab,
      treatments = treatments_df
    ),
    error = function(e) {
      showNotification(paste("网络构建失败：", e$message), type = "error")
      return(NULL)
    }
  )
  if (is.null(network)) return(NULL)

  model <- tryCatch({
    if (linearModel == "random") {
      gemtc::mtc.model(
        network,
        type = "consistency",
        likelihood = "binom",
        link = "logit",
        linearModel = "random",
        n.chain = 4,
        hy.prior = gemtc::mtc.hy.prior("std.dev", "dunif", 0, 5)
      )
    } else {
      gemtc::mtc.model(
        network,
        type = "consistency",
        likelihood = "binom",
        link = "logit",
        linearModel = "fixed",
        n.chain = 4
      )
    }
  }, error = function(e) {
    showNotification(paste("模型失败：", e$message), type = "error")
    return(NULL)
  })
  if (is.null(model)) return(NULL)

  result <- tryCatch(
    gemtc::mtc.run(
      model,
      sampler = "rjags",
      n.adapt = 5000,
      n.iter = 25000,
      thin = 5
    ),
    error = function(e) {
      showNotification(paste("MCMC失败：", e$message), type = "error")
      return(NULL)
    }
  )
  if (is.null(result)) return(NULL)

  list(
    result = list(
      model = result,
      network = network,
      ref_trt = ref_trt,
      treatments_df = treatments_df,
      linearModel = linearModel
    ),
    setup = list(data = data_ab),
    type = nd$type,
    measure = nd$measure,
    method_family = "bayesian"
  )
}

# ===================== UI 模块 =====================
networkMetaUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("网络荟萃分析 (Network Meta-Analysis)"),
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
    fluidRow(
      column(3, selectInput(ns("nma_method"), "估计方法",
                            choices = c(
                              "频率学派 - REML" = "freq_REML",
                              "频率学派 - 固定效应" = "freq_fixed",
                              "频率学派 - 随机效应 (NH)" = "freq_NH",
                              "贝叶斯 - 固定效应" = "bayes_fixed",
                              "贝叶斯 - 随机效应" = "bayes_random"
                            ),
                            selected = "freq_REML")),
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
    h4("网络结构图 (Network Graph)"),
    plotOutput(ns("netplot"), height = "500px"),
    br(),
    conditionalPanel(
      condition = "input.nma_method == 'bayes_fixed' || input.nma_method == 'bayes_random'",
      ns = ns,
      h4("MCMC 诊断图 (Trace & Density)"),
      p("标准 gemtc 诊断图（包含 trace 和 density）。",
        style = "color: #31708f;"),
      uiOutput(ns("bayes_diag_plots")),
      br(),
      verbatimTextOutput(ns("bayes_params_info"))
    ),
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
    leagueTableUI(ns("league_table")),
    hr(),
    h4("原始输入数据预览"),
    DTOutput(ns("data_table"))
  )
}

# ===================== Server 模块 =====================
networkMetaServer <- function(id, data_type, outcome, selected_studies, current_data, studies_info) {
  moduleServer(id, function(input, output, session) {

    col_mapping <- reactiveValues(exp_col = NULL, comp_col = NULL)

    # ---------- 列检测函数 ----------
    detect_exp_columns <- reactive({
      req(studies_info)
      all_cols <- names(studies_info)
      patterns <- c("^Experimental$", "^experimental$", "^treat1$",
                    "^Experimental_Test_\\d+$", "^Exp_\\d+$", "^Treatment$", "^Intervention$")
      matched <- unique(unlist(lapply(patterns, function(p) grep(p, all_cols, value = TRUE, ignore.case = FALSE))))
      if (length(matched) == 0) matched <- all_cols
      matched
    })

    detect_comp_columns <- reactive({
      req(studies_info)
      all_cols <- names(studies_info)
      patterns <- c("^Comparator$", "^comparator$", "^treat2$",
                    "^Comparator_Test_\\d+$", "^Comp_\\d+$", "^Control$", "^Placebo$")
      matched <- unique(unlist(lapply(patterns, function(p) grep(p, all_cols, value = TRUE, ignore.case = FALSE))))
      if (length(matched) == 0) matched <- all_cols
      matched
    })

    output$exp_col_ui <- renderUI({
      req(studies_info)
      exp_options <- detect_exp_columns()
      default_exp <- if ("Experimental" %in% exp_options) "Experimental"
      else if ("experimental" %in% exp_options) "experimental"
      else if ("treat1" %in% exp_options) "treat1"
      else exp_options[1]
      selectInput(session$ns("selected_exp_col"), label = tagList(icon("flask"), "Experimental 列 (治疗组)"),
                  choices = exp_options, selected = default_exp)
    })

    output$comp_col_ui <- renderUI({
      req(studies_info)
      comp_options <- detect_comp_columns()
      default_comp <- if ("Comparator" %in% comp_options) "Comparator"
      else if ("comparator" %in% comp_options) "comparator"
      else if ("treat2" %in% comp_options) "treat2"
      else comp_options[1]
      selectInput(session$ns("selected_comp_col"), label = tagList(icon("chart-line"), "Comparator 列 (对照组)"),
                  choices = comp_options, selected = default_comp)
    })

    observeEvent(input$apply_col_mapping, {
      req(input$selected_exp_col, input$selected_comp_col)
      col_mapping$exp_col <- input$selected_exp_col
      col_mapping$comp_col <- input$selected_comp_col
      showNotification(paste("已应用映射：", col_mapping$exp_col, "→ 治疗组，",
                             col_mapping$comp_col, "→ 对照组"), type = "message", duration = 3)
    })

    # ---------- 治疗名称列表（统一清洗）----------
    all_treatments <- reactive({
      req(selected_studies(), col_mapping$exp_col, col_mapping$comp_col)
      studies_filtered <- studies_info %>%
        dplyr::filter(unique_id %in% selected_studies()) %>%
        dplyr::select(!!sym(col_mapping$exp_col), !!sym(col_mapping$comp_col))
      treat_vec <- unique(c(studies_filtered[[1]], studies_filtered[[2]]))
      treat_vec <- treat_vec[!is.na(treat_vec)]
      treat_vec <- clean_treatment_unified(treat_vec)
      if (length(treat_vec) == 0) return(NULL)
      return(treat_vec)
    })

    output$ref_trt_ui <- renderUI({
      req(all_treatments())
      default_ref <- if ("Chemotherapy" %in% all_treatments()) "Chemotherapy" else all_treatments()[1]
      selectInput(session$ns("ref_trt"), "参考治疗", choices = all_treatments(), selected = default_ref)
    })

    output$measure_ui <- renderUI({
      if (data_type() == "binary") {
        selectInput(session$ns("binary_measure"), "效应指标 (二分类)",
                    choices = c("风险差 (RD)" = "RD", "风险比 (OR)" = "OR", "相对风险 (RR)" = "RR"),
                    selected = "RD")
      } else NULL
    })

    nma_result_val <- reactiveVal(NULL)
    last_run_studies <- reactiveVal(NULL)

    observeEvent(
      list(selected_studies(), col_mapping$exp_col, col_mapping$comp_col,
           data_type(), input$binary_measure),
      {
        if (!is.null(nma_result_val())) {
          nma_result_val(NULL)
          last_run_studies(NULL)
          showNotification("⚠️ 研究选择或数据设置已改变，之前 NMA 结果已清空，请重新运行",
                           type = "warning", duration = 5)
        }
      },
      ignoreNULL = FALSE, ignoreInit = TRUE
    )

    # ---------- 数据预处理 ----------
    output$data_type_hint <- renderUI({
      if (data_type() == "binary") p("二分类数据：两行长格式") else p("生存数据：trans.armdata 转换后的长格式")
    })

    output$data_table <- renderDT({
      dat <- current_data()
      req(dat)
      if (nrow(dat) == 0) return(NULL)
      datatable(dat, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    treat_info <- reactive({
      req(studies_info, col_mapping$exp_col, col_mapping$comp_col)
      exp_col <- col_mapping$exp_col
      comp_col <- col_mapping$comp_col
      if (!(exp_col %in% names(studies_info)) || !(comp_col %in% names(studies_info))) {
        showNotification("选定的列不存在于数据中", type = "error")
        return(NULL)
      }
      result <- studies_info %>%
        dplyr::select(unique_id, treat1 = !!sym(exp_col), treat2 = !!sym(comp_col)) %>%
        dplyr::distinct(unique_id, .keep_all = TRUE)
      result
    })

    nma_data <- reactive({
      req(current_data(), treat_info())
      study_ids <- selected_studies()
      if (is.null(study_ids) || length(study_ids) < 2) {
        showNotification("至少需要选择 2 项研究", type = "warning")
        return(NULL)
      }
      dat <- current_data()
      if (!is.data.frame(dat)) {
        if (is.list(dat) && !is.null(dat$data.ab) && is.data.frame(dat$data.ab)) dat <- dat$data.ab
        else if (is.list(dat) && !is.null(dat$data) && is.data.frame(dat$data)) dat <- dat$data
        else dat <- tryCatch(as.data.frame(dat), error = function(e) NULL)
      }
      req(is.data.frame(dat))
      dat <- dat %>% dplyr::filter(unique_id %in% study_ids)
      if (nrow(dat) < 2) {
        showNotification("至少需要 2 项研究的数据", type = "error")
        return(NULL)
      }
      info <- treat_info()
      dat <- dat %>% left_join(info, by = "unique_id") %>%
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
        long_data$trt <- clean_treatment_unified(long_data$trt)
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
          trans_data$trt <- clean_treatment_unified(trans_data$trt)
          list(type = "survival", data = trans_data, measure = "HR")
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
        datatable(as.data.frame(df), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
      }
    })

    # ---------- 运行 NMA ----------
    observeEvent(input$run_nma, {
      nd <- nma_data()
      if (is.null(nd)) {
        showNotification("无法运行 NMA：数据准备失败，请检查研究选择和数据完整性", type = "error")
        nma_result_val(NULL)
        return()
      }
      ref_trt <- clean_treatment_unified(input$ref_trt)

      withProgress(message = '运行 NMA 分析...', value = 0.3, {
        final_res <- NULL
        if (startsWith(input$nma_method, "freq_")) {
          incProgress(0.6, detail = "使用频率学派方法...")
          final_res <- run_nma_frequentist(nd, ref_trt, input$nma_method)
        } else if (input$nma_method %in% c("bayes_fixed", "bayes_random")) {
          incProgress(0.6, detail = "运行贝叶斯模型（可能需要几分钟）...")
          final_res <- run_nma_bayesian(nd, ref_trt, input$nma_method)
        } else {
          showNotification("未知的方法类型", type = "error")
          return()
        }
        if (!is.null(final_res)) {
          nma_result_val(final_res)
          last_run_studies(selected_studies())
          showNotification("NMA 运行成功！", type = "message")
        } else {
          nma_result_val(NULL)
        }
      })
    })

    nma_result <- reactive({ nma_result_val() })

    output$nma_status <- renderText({
      if (is.null(nma_result_val())) "状态：尚未运行 NMA（请点击\"运行网络分析\"按钮）"
      else {
        current_ids <- selected_studies()
        last_ids <- last_run_studies()
        if (!is.null(last_ids) && !identical(sort(current_ids), sort(last_ids)))
          "⚠️ 警告：当前显示的研究与最后一次运行 NMA 时不一致，请重新运行！"
        else if (nma_result_val()$method_family == "bayesian")
          "✅ 状态：贝叶斯 NMA 已运行成功（基于当前选择的研究）"
        else
          "✅ 状态：频率学派 NMA 已运行成功（基于当前选择的研究）"
      }
    })

    # ---------- 网络结构图 ----------
    output$netplot <- renderPlot({
      res <- nma_result_val()
      req(res)
      if (res$method_family == "bayesian") {
        plot(res$result$network, cex = 1.2)
        title("Network Plot (Bayesian)")
      } else {
        tryCatch({
          NMA::netplot(res$setup, text = TRUE, base.cex = 1.2)
        }, error = function(e) {
          plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
               main = paste("网络图绘制失败:\n", e$message), col = "red")
          text(1, 1, "请检查数据是否满足NMA分析要求", cex = 1.2)
        })
      }
    })

    # ---------- 贝叶斯诊断图（使用 gemtc 标准绘图）----------
    # ---------- 根据参数数量动态生成诊断图容器 ----------
    output$bayes_diag_plots <- renderUI({
      res <- nma_result_val()
      req(res, res$method_family == "bayesian")
      samples <- res$result$model$samples
      if (is.null(samples)) return(p("MCMC 样本为空"))

      params <- colnames(samples[[1]])
      ns <- session$ns

      # 为每个参数生成一个 plotOutput，并设置足够的高度
      plot_list <- lapply(seq_along(params), function(i) {
        plot_id <- paste0("bayes_param_", i)
        plotOutput(ns(plot_id), height = "400px")
      })

      # 将所有 plotOutput 放入一个 tagList
      do.call(tagList, plot_list)
    })

    # ---------- 为每个参数创建 renderPlot ----------
    observeEvent(nma_result_val(), {
      res <- nma_result_val()
      if (is.null(res) || res$method_family != "bayesian") return()

      samples <- res$result$model$samples
      if (is.null(samples)) return()

      params <- colnames(samples[[1]])
      ns <- session$ns

      for (i in seq_along(params)) {
        local({
          param <- params[i]
          plot_id <- paste0("bayes_param_", i)
          output[[plot_id]] <- renderPlot({
            # 提取该参数所有链的数据
            vals <- do.call(c, lapply(samples, function(chain) as.vector(chain[, param])))
            n_iters <- nrow(samples[[1]])
            n_chains <- length(samples)

            # 构造数据框
            df <- data.frame(
              Iteration = rep(1:n_iters, n_chains),
              Value = vals,
              Chain = factor(rep(1:n_chains, each = n_iters))
            )

            # 设置左右分图布局
            par(mfrow = c(1, 2))

            # 左侧：trace 图
            plot(df$Iteration, df$Value, type = "n",
                 xlab = "Iteration", ylab = param,
                 main = paste("Trace -", param))
            for (ch in levels(df$Chain)) {
              sub <- df[df$Chain == ch, ]
              lines(sub$Iteration, sub$Value, col = as.integer(ch))
            }
            legend("topright", legend = levels(df$Chain),
                   col = 1:n_chains, lty = 1, cex = 0.8)

            # 右侧：density 图
            plot(density(df$Value), main = paste("Density -", param),
                 xlab = param)
          })
        })
      }
    }, priority = 100)


    output$bayes_params_info <- renderPrint({
      res <- nma_result_val()
      req(res, res$method_family == "bayesian")

      # 提取 MCMC 样本中的变量名（coda 的 mcmc.list）
      samples <- res$result$model$samples
      if (is.null(samples)) {
        cat("MCMC 样本为空，无法提取参数信息。\n")
        return()
      }

      all_params <- coda::varnames(samples)  # coda 的 varnames 函数
      cat("模型参数总数:", length(all_params), "\n")
      cat("参数列表:\n")
      cat(paste(all_params, collapse = ", "), "\n")

      # 可选：按类型分组显示
      d_params <- grep("^d\\.", all_params, value = TRUE)
      sd_params <- grep("^sd\\.", all_params, value = TRUE)
      if (length(d_params) > 0) {
        cat("\n相对效应参数 (d.*):", length(d_params), "个\n")
        cat(paste(d_params, collapse = ", "), "\n")
      }
      if (length(sd_params) > 0) {
        cat("\n异质性参数 (sd.*):", length(sd_params), "个\n")
        cat(paste(sd_params, collapse = ", "), "\n")
      }
    })

    # ---------- 森林图 ----------
    output$nmaforest <- renderPlot({
      res <- nma_result_val()
      req(res)

      if (res$method_family == "bayesian") {
        gemtc::forest(res$result$model)
        title(main = "Forest Plot (Bayesian NMA)")
        return()
      }

      method_nma <- switch(input$nma_method,
                           freq_REML = "REML",
                           freq_fixed = "fixed",
                           freq_NH = "NH",
                           "REML")
      tryCatch({
        suppressWarnings({
          NMA::nmaforest(res$setup, method = method_nma)
        })
      }, error = function(e) {
        plot.new()
        title("森林图生成失败", col.main = "red")
        text(0.5, 0.5, paste0("错误信息：", e$message), cex = 1.1)
      })
    })

    # ---------- 结果摘要 ----------
    output$nma_summary <- renderPrint({
      res <- nma_result_val()
      if (is.null(res)) {
        cat("未获得有效结果。请检查：\n1. 至少选择2个研究\n2. 数据中治疗组名称匹配\n3. 治疗效果指标选择正确\n")
      } else if (res$method_family == "bayesian") {
        cat("【贝叶斯网络荟萃分析结果】\n")
        cat("参考治疗：", res$result$ref_trt, "\n")
        cat("模型类型：", ifelse(res$result$linearModel == "fixed", "固定效应", "随机效应"), "\n")
        print(summary(res$result$model))
      } else {
        print(res$result)
      }
    })

    # ---------- 治疗排名（SUCRA）----------
    rank_result <- reactive({
      res <- nma_result_val()
      if (is.null(res) || is.null(res$result)) return(NULL)

      if (res$method_family == "bayesian") {
        ranking <- tryCatch({
          rank_probs <- gemtc::rank.probability(res$result$model)
          sucra_vals <- gemtc::sucra(rank_probs)
          list(rank_probs = rank_probs, sucra = sucra_vals)
        }, error = function(e) {
          showNotification(paste("贝叶斯排名计算失败：", e$message), type = "warning")
          return(NULL)
        })
        if (is.null(ranking)) return(NULL)
        sucra <- ranking$sucra
        trt_map <- res$result$treatments_df
        id_to_name <- setNames(trt_map$description, trt_map$id)
        rank_df <- data.frame(
          Treatment = id_to_name[names(sucra)],
          SUCRA = as.numeric(sucra),
          stringsAsFactors = FALSE
        ) %>% arrange(desc(SUCRA))
        return(rank_df)
      }

      # 频率学派排名
      setup_obj <- res$setup
      trt_names <- NULL
      if (!is.null(setup_obj$trt.id)) {
        trt_names <- names(setup_obj$trt.id)
      } else if (!is.null(setup_obj$data) && is.data.frame(setup_obj$data) && "trt" %in% names(setup_obj$data)) {
        trt_names <- sort(unique(setup_obj$data$trt))
      }
      current_treats <- all_treatments()
      if (!is.null(trt_names) && !is.null(current_treats) && !identical(sort(trt_names), sort(current_treats))) {
        trt_names <- current_treats
      } else if (is.null(trt_names) && !is.null(current_treats)) {
        trt_names <- current_treats
      }
      if (is.null(trt_names)) {
        showNotification("无法获取治疗名称列表，排名计算中止", type = "warning")
        return(NULL)
      }

      orig_method <- switch(input$nma_method,
                            freq_REML = "REML",
                            freq_fixed = "fixed",
                            freq_NH = "NH",
                            "REML")
      rank <- tryCatch({
        NMA::nmarank(res$result, method = orig_method)
      }, error = function(e) {
        tryCatch({
          NMA::nmarank(setup_obj, method = orig_method)
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
          if (!is.null(names(vec)) && all(names(vec) %in% trt_names)) {
            full_vec <- setNames(rep(NA, length(trt_names)), trt_names)
            full_vec[names(vec)] <- vec
            vec <- full_vec
          } else {
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
      if (is.data.frame(rank) && nrow(rank) > 0) DTOutput(session$ns("rank_table"))
      else verbatimTextOutput(session$ns("rank_text"))
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

    # ---------- 联赛表模块 ----------
    leagueTableServer("league_table",
                      nma_result = reactive({ nma_result_val() }),
                      rank_result = rank_result,
                      nma_method = reactive({ input$nma_method })
    )
  })
}
