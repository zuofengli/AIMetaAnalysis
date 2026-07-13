# modules/network_meta_module.R
# 完整的网络荟萃分析模块（排名表格和方向选择已移至 sucra_ranking_diagram_module.R）
# 注：治疗组列映射已在 data_selection_module 中完成，本模块不再重复

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
library(NMA)

source("modules/league_table_module.R")
source("modules/sucra_ranking_diagram_module.R")   # 独立排名图模块（含表格和方向选择）

# ========== 统一的治疗名称清洗函数 ==========
clean_treatment_unified <- function(x) {
  x <- as.character(x)
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
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
  } else {  # survival
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

# ========== 贝叶斯 NMA 核心函数 ==========
run_nma_bayesian <- function(nd, ref_trt, method_choice, raw_data=NULL) {
  if (method_choice == "bayes_fixed") {
    linearModel <- "fixed"
  } else {
    linearModel <- "random"
  }

  ref_trt <- clean_treatment_unified(ref_trt)

  if (nd$type == "binary") {
    nd$data$trt <- clean_treatment_unified(nd$data$trt)
    if (!all(c("d", "n") %in% names(nd$data))) {
      showNotification("二分类数据缺少 d (事件数) 或 n (样本量)", type = "error")
      return(NULL)
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
        !is.na(responders), !is.na(sampleSize),
        responders >= 0, sampleSize > 0,
        responders <= sampleSize
      )
    likelihood <- "binom"
    link <- "logit"

    treatments_present <- unique(data_ab$treatment)
    if (length(treatments_present) < 2) {
      showNotification("至少需要两个治疗组", type = "error")
      return(NULL)
    }
    if (!(ref_trt %in% treatments_present)) {
      ref_trt <- treatments_present[1]
      showNotification(paste("指定的参考治疗不存在，已自动使用:", ref_trt), type = "warning")
    }

    treatments_df <- data.frame(
      id = treatments_present,
      description = treatments_present,
      stringsAsFactors = FALSE
    )

    network <- tryCatch(
      gemtc::mtc.network(data.ab = data_ab, treatments = treatments_df),
      error = function(e) {
        showNotification(paste("网络构建失败：", e$message), type = "error")
        return(NULL)
      }
    )
    if (is.null(network)) return(NULL)

  } else if (nd$type == "survival") {
    if (is.null(nd$bayesian_data)) {
      showNotification("生存数据缺少贝叶斯所需对比数据 (treatment1, treatment2, diff, std.err)", type = "error")
      return(NULL)
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

    treatments_df <- data.frame(
      id = treatments_present,
      description = treatments_present,
      stringsAsFactors = FALSE
    )

    dat <- raw_data
    print(dat)

    if(any(is.na(dat$se_te))) {
      dat$se_te <- (log(dat$hr_uci) - log(dat$hr_lci)) / (2 * qnorm(0.975))
    }
    if(any(is.na(dat$te))) {
      dat$te <- log(dat$hr)
    }

    study_list <- list()
    for (i in 1:nrow(dat)) {
      study_id <- dat$study_display[i]
      t1 <- dat$treat1[i]
      t2 <- dat$treat2[i]
      te_val <- as.numeric(dat$te[i])
      se_val <- as.numeric(dat$se_te[i])

      baseline <- data.frame(
        study = study_id,
        treatment = t2,
        diff = NA_real_,
        std.err = NA_real_,
        stringsAsFactors = FALSE
      )
      experimental <- data.frame(
        study = study_id,
        treatment = t1,
        diff = te_val,
        std.err = se_val,
        stringsAsFactors = FALSE
      )
      study_list[[i]] <- rbind(baseline, experimental)
    }

    data_re <- do.call(rbind, study_list)
    data_re$diff <- as.numeric(data_re$diff)
    data_re$std.err <- as.numeric(data_re$std.err)
    rownames(data_re) <- NULL

    network <- tryCatch(
      gemtc::mtc.network(data.re = data_re),
      error = function(e) {
        showNotification(paste("网络构建失败 (data.re)：", e$message), type = "error")
        return(NULL)
      }
    )
    print(network)
    if (is.null(network)) return(NULL)

    likelihood <- "normal"
    link <- "identity"

  } else {
    showNotification(paste("不支持的数据类型:", nd$type), type = "error")
    return(NULL)
  }

  model <- tryCatch({
    if (linearModel == "random") {
      hy.prior <- gemtc::mtc.hy.prior("std.dev", "dunif", 0, 5)
      gemtc::mtc.model(
        network,
        type = "consistency",
        likelihood = likelihood,
        link = link,
        linearModel = "random",
        n.chain = 4,
        hy.prior = hy.prior
      )
    } else {
      gemtc::mtc.model(
        network,
        type = "consistency",
        likelihood = likelihood,
        link = link,
        linearModel = "fixed",
        n.chain = 4
      )
    }
  }, error = function(e) {
    showNotification(paste("模型创建失败：", e$message), type = "error")
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
      showNotification(paste("MCMC 运行失败：", e$message), type = "error")
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
    setup = list(data = if (nd$type == "binary") data_ab else data_re),
    type = nd$type,
    measure = if (nd$type == "binary") nd$measure else "HR",
    method_family = "bayesian"
  )
}

# ===================== UI 模块 =====================
networkMetaUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("网络荟萃分析 (Network Meta-Analysis)"),
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
    htmlOutput(ns("netplot_caption"),
               style = "text-align: center; font-style: italic; margin-top: -10px; margin-bottom: 15px; color: #555555;"),
    br(),
    conditionalPanel(
      condition = "input.nma_method == 'bayes_fixed' || input.nma_method == 'bayes_random'",
      ns = ns,
      h4("MCMC 诊断图 (Trace & Density)"),
      p("标准 gemtc 诊断图（包含 trace 和 density）。", style = "color: #31708f;"),
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
    # 排名图模块（包含表格和图形）
    sucraDiagramUI(ns("rank_diagram")),
    hr(),
    leagueTableUI(ns("league_table")),
    hr(),
    h4("原始输入数据预览"),
    DTOutput(ns("data_table"))
  )
}

# ===================== Server 模块 =====================
networkMetaServer <- function(id, data_type, outcome, subgroup, selected_levels, selected_studies, current_data, studies_info) {
  moduleServer(id, function(input, output, session) {

    # ---------- 所有治疗名称（用于参考治疗下拉框） ----------
    all_treatments <- reactive({
      req(selected_studies())
      # 从 current_data 中提取所有独特的治疗组
      dat <- current_data()
      if (is.null(dat) || nrow(dat) == 0) return(NULL)
      treat_vec <- unique(c(dat$treat1, dat$treat2))
      treat_vec <- treat_vec[!is.na(treat_vec)]
      treat_vec <- clean_treatment_unified(treat_vec)
      if (length(treat_vec) == 0) return(NULL)
      return(treat_vec)
    })

    available_ref_trts <- reactive({
      nd <- nma_data()
      req(nd)
      if (nd$type == "binary") {
        unique(nd$data$trt)
      } else {
        if (startsWith(input$nma_method, "bayes_") && !is.null(nd$bayesian_data)) {
          unique(c(nd$bayesian_data$treatment1, nd$bayesian_data$treatment2))
        } else {
          unique(nd$data$trt)
        }
      }
    })

    output$ref_trt_ui <- renderUI({
      trts <- available_ref_trts()
      if (is.null(trts) || length(trts) == 0) return(p("暂无可用治疗"))
      default_ref <- if ("Chemotherapy" %in% trts) "Chemotherapy" else trts[1]
      selectInput(session$ns("ref_trt"), "参考治疗", choices = trts, selected = default_ref)
    })

    output$measure_ui <- renderUI({
      if (data_type() == "binary") {
        is_bayesian <- startsWith(input$nma_method, "bayes")
        if (is_bayesian) {
          selectInput(session$ns("binary_measure"), "效应指标 (二分类)",
                      choices = c("风险比 (OR)" = "OR"),
                      selected = "OR")
        } else {
          selectInput(session$ns("binary_measure"), "效应指标 (二分类)",
                      choices = c("风险比 (OR)" = "OR", "风险差 (RD)" = "RD"),
                      selected = "OR")
        }
      } else {
        selectInput(session$ns("binary_measure"), "效应指标 (生存数据)",
                    choices = c("风险比 (HR)" = "HR"),
                    selected = "HR")
      }
    })

    nma_result_val <- reactiveVal(NULL)
    last_run_studies <- reactiveVal(NULL)

    observeEvent(
      list(selected_studies(), data_type(), input$binary_measure),
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

    output$data_type_hint <- renderUI({
      if (data_type() == "binary") {
        p("二分类数据：长格式 (study, trt, d, n)")
      } else {
        if (startsWith(input$nma_method, "bayes_")) {
          p("生存数据 (贝叶斯)：对比数据 (study, treat1, treat2, logHR, SE)")
        } else {
          p("生存数据 (频率学派)：trans.armdata 转换后的长格式 (study, trt, d, n)")
        }
      }
    })

    output$data_table <- renderDT({
      dat <- current_data()
      req(dat)
      if (nrow(dat) == 0) return(NULL)
      datatable(dat, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    # ---------- NMA 数据准备 ----------
    nma_data <- reactive({
      req(current_data())
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
      #browser()
      if (nrow(dat) < 2) {
        showNotification("至少需要 2 项研究的数据", type = "error")
        return(NULL)
      }

      # 确保 treat1/treat2 存在且非 NA（已在 data_selection_module 中处理好）
      if (!("treat1" %in% names(dat)) || !("treat2" %in% names(dat))) {
        showNotification("数据中缺少 treat1 或 treat2 列", type = "error")
        return(NULL)
      }
      dat <- dat %>% dplyr::filter(!is.na(treat1) & !is.na(treat2))

      if (nrow(dat) == 0) {
        showNotification("所有研究的治疗组信息缺失", type = "error")
        return(NULL)
      }

      #browser()

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
        #browser()
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

        trans_data <- tryCatch({
          NMA::trans.armdata(
            study = study_display, treat1 = treat1, treat2 = treat2,
            n1 = n1, n2 = n2, y = te, SE = se_te, measure = "logHR", data = dat
          )
        }, error = function(e) {
          showNotification(paste("trans.armdata 转换失败：", e$message), type = "error")
          NULL
        })
        if (is.null(trans_data)) return(NULL)
        trans_data$trt <- clean_treatment_unified(trans_data$trt)
        trans_data$study <- as.character(dat$study_display)   # 确保研究名称正确

        bayesian_data <- dat %>%
          transmute(
            study      = study_display,
            treatment1 = clean_treatment_unified(treat1),
            treatment2 = clean_treatment_unified(treat2),
            diff       = te,
            std.err    = se_te
          ) %>%
          dplyr::filter(
            !is.na(treatment1), !is.na(treatment2),
            !is.na(diff), !is.na(std.err),
            std.err > 0,
            is.finite(diff), is.finite(std.err),
            treatment1 != treatment2
          )

        if (nrow(bayesian_data) < 2) {
          showNotification("贝叶斯对比数据不足，需要至少2个有效研究", type = "error")
          return(NULL)
        }

        list(
          type = "survival",
          data = trans_data,
          bayesian_data = bayesian_data,
          measure = "HR"
        )
      }
    })

    output$pretable <- renderDT({
      nd <- nma_data()
      req(nd)
      if (nd$type == "survival" && startsWith(input$nma_method, "bayes_")) {
        df <- nd$bayesian_data
        col_labs <- c("研究", "实验组", "对照组", "logHR (mean)", "标准误 (std.err)")
        datatable(as.data.frame(df), options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE, colnames = col_labs)
      } else {
        df <- nd$data
        if (nd$type == "binary") {
          col_labs <- c("研究", "治疗组", "样本量 (n)", "事件数 (d)")
        } else {
          col_labs <- c("研究", "治疗组", "效应/事件 (d)", "样本量 (n)")
        }
        datatable(as.data.frame(df), options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE, colnames = col_labs)
      }
    })

    # ---------- 运行 NMA ----------
    observeEvent(input$run_nma, {

      nd <- nma_data()
      if (is.null(nd)) {
        nma_result_val(NULL)
        return()
      }

      current_subgroup <- subgroup()
      selected_levels <- selected_levels()

      if (!is.null(current_subgroup) && current_subgroup != "Overall") {
        if (is.null(selected_levels) || length(selected_levels) == 0) {
          showNotification(
            "请先选择一个亚组水平后再运行网络荟萃分析",
            type = "warning",
            duration = 5
          )
          return(NULL)
        }
        if (length(selected_levels) > 1) {
          showNotification(
            "网络荟萃分析目前仅支持一次运行一个亚组。请只保留一个“已选水平（显示顺序）”。",
            type = "error",
            duration = 8
          )
          return(NULL)
        }
      }

      ref_trt <- clean_treatment_unified(input$ref_trt)

      withProgress(message = '运行 NMA 分析...', value = 0.3, {
        final_res <- NULL
        if (startsWith(input$nma_method, "freq_")) {
          incProgress(0.6, detail = "使用频率学派方法...")
          final_res <- run_nma_frequentist(nd, ref_trt, input$nma_method)
        } else if (input$nma_method %in% c("bayes_fixed", "bayes_random")) {
          incProgress(0.6, detail = "运行贝叶斯模型（可能需要几分钟）...")
          final_res <- run_nma_bayesian(nd, ref_trt, input$nma_method, raw_data = current_data())
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

    output$netplot_caption <- renderUI({
      res <- nma_result_val()
      req(res)
      out <- outcome()
      if (is.null(out) || out == "") out <- "Selected Outcome"
      paste0("Fig. 1 Network graph of ", out)
    })

    # ---------- 贝叶斯诊断图 ----------
    output$bayes_diag_plots <- renderUI({
      res <- nma_result_val()
      req(res, res$method_family == "bayesian")
      samples <- res$result$model$samples
      if (is.null(samples)) return(p("MCMC 样本为空"))
      params <- colnames(samples[[1]])
      ns <- session$ns
      plot_list <- lapply(seq_along(params), function(i) {
        plotOutput(ns(paste0("bayes_param_", i)), height = "400px")
      })
      do.call(tagList, plot_list)
    })

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
            vals <- do.call(c, lapply(samples, function(chain) as.vector(chain[, param])))
            n_iters <- nrow(samples[[1]])
            n_chains <- length(samples)
            df <- data.frame(Iteration = rep(1:n_iters, n_chains),
                             Value = vals,
                             Chain = factor(rep(1:n_chains, each = n_iters)))
            par(mfrow = c(1, 2))
            plot(df$Iteration, df$Value, type = "n",
                 xlab = "Iteration", ylab = param, main = paste("Trace -", param))
            for (ch in levels(df$Chain)) {
              sub <- df[df$Chain == ch, ]
              lines(sub$Iteration, sub$Value, col = as.integer(ch))
            }
            legend("topright", legend = levels(df$Chain), col = 1:n_chains, lty = 1, cex = 0.8)
            plot(density(df$Value), main = paste("Density -", param), xlab = param)
          })
        })
      }
    }, priority = 100)

    output$bayes_params_info <- renderPrint({
      res <- nma_result_val()
      req(res, res$method_family == "bayesian")
      samples <- res$result$model$samples
      if (is.null(samples)) {
        cat("MCMC 样本为空，无法提取参数信息。\n")
        return()
      }
      all_params <- coda::varnames(samples)
      cat("模型参数总数:", length(all_params), "\n")
      cat("参数列表:\n")
      cat(paste(all_params, collapse = ", "), "\n")
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

    # ---------- 治疗排名（SUCRA） ----------
    # ---------- 治疗排名（SUCRA） ----------
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
        rank_probs <- ranking$rank_probs
        trt_map <- res$result$treatments_df
        id_to_name <- setNames(trt_map$description, trt_map$id)

        rank_df <- data.frame(
          Treatment = id_to_name[names(sucra)],
          SUCRA = as.numeric(sucra),
          stringsAsFactors = FALSE
        ) %>% arrange(desc(SUCRA))

        ## [修改] 获取参考治疗的原始 ID 或名称，并将其 SUCRA 强制设为 0
        ref_trt_id <- res$result$ref_trt
        ref_trt_name <- id_to_name[ref_trt_id]

        if (ref_trt_name %in% rank_df$Treatment) {
          rank_df$SUCRA[rank_df$Treatment == ref_trt_name] <- 0
        }

        return(rank_df %>% arrange(desc(SUCRA)))
      }

      # ---------- 频率学派排名 ----------
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

      rank_df <- NULL
      if (is.list(rank) && !is.data.frame(rank)) {
        if ("SUCRA" %in% names(rank)) rank_df <- make_rank_df(rank$SUCRA, "SUCRA")
        if ("Pbest" %in% names(rank)) rank_df <- make_rank_df(rank$Pbest, "P_best")
      } else if (is.data.frame(rank)) {
        if ("Treatment" %in% colnames(rank) && is.numeric(rank$Treatment))
          rank$Treatment <- trt_names[rank$Treatment]
        if ("trt" %in% colnames(rank) && is.numeric(rank$trt)) {
          rank$Treatment <- trt_names[rank$trt]
          rank$trt <- NULL
        }
        rank_df <- rank
        if ("SUCRA" %in% colnames(rank_df)) rank_df <- rank_df[order(-rank_df$SUCRA), ]
        if ("P_best" %in% colnames(rank_df)) rank_df <- rank_df[order(-rank_df$P_best), ]
      } else if (is.numeric(rank)) {
        rank_df <- make_rank_df(rank, "Score")
      }

      if (!is.null(rank_df)) {
        # --- 1. 防御性措施：确保 SUCRA 列存在且可计算 ---
        if (!"SUCRA" %in% names(rank_df)) {
          if ("P_best" %in% names(rank_df)) {
            rank_df$SUCRA <- rank_df$P_best
          } else if ("Score" %in% names(rank_df)) {
            rank_df$SUCRA <- rank_df$Score
          } else {
            rank_df$SUCRA <- 0  # 默认全设为0，防止缺失
          }
        }

        # 确保转换为数值，并将 NA 替换为 0，防止传入绘图层引发警告
        rank_df$SUCRA <- as.numeric(rank_df$SUCRA)
        rank_df$SUCRA[is.na(rank_df$SUCRA)] <- 0

        # --- 2. 将参考治疗的 SUCRA 强制设为 0 (最差) ---
        ref_trt <- clean_treatment_unified(input$ref_trt)
        if (ref_trt %in% rank_df$Treatment) {
          rank_df$SUCRA[rank_df$Treatment == ref_trt] <- 0
        }

        # 重新降序排序
        rank_df <- rank_df %>% arrange(desc(SUCRA))
      }

      return(rank_df)
    })

    # 返回完整排名信息（列表），供排名图模块（含 Rankogram）使用
    rank_full <- reactive({
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
        rank_probs <- ranking$rank_probs
        trt_map <- res$result$treatments_df
        id_to_name <- setNames(trt_map$description, trt_map$id)
        rank_df <- data.frame(
          Treatment = id_to_name[names(sucra)],
          SUCRA = as.numeric(sucra),
          stringsAsFactors = FALSE
        ) %>% arrange(desc(SUCRA))
        return(list(sucra = rank_df, rank_probs = rank_probs))
      } else {
        # 频率学派：只返回 sucra，rank_probs = NULL
        sucra_df <- rank_result()
        if (is.null(sucra_df)) return(NULL)
        return(list(sucra = sucra_df, rank_probs = NULL))
      }
    })

    # ---------- 联赛表模块 ----------
    leagueTableServer("league_table",
                      nma_result = reactive({ nma_result_val() }),
                      rank_result = rank_result,
                      nma_method = reactive({ input$nma_method })
    )

    # ---------- 独立排名图模块（支持 SUCRA 条形图/ Rankogram 和方向切换）----------
    sucraDiagramServer("rank_diagram", rank_result = rank_full, outcome = outcome)

  })
}
