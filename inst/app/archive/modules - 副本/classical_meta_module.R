# modules/classical_meta_module.R
# 经典荟萃分析模块：效应模型、森林图、漏斗图、发表偏倚检验、合并结果

library(shiny)
library(meta)
library(metafor)
library(ggplot2)

classicalMetaUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("3. 分析设置"),
    fluidRow(
      column(3, radioButtons(ns("model"), "效应模型",
                             choices = c("固定效应" = "fixed", "随机效应" = "random"),
                             selected = "fixed")),
      column(3, br(), actionButton(ns("run"), "运行荟萃分析", icon = icon("play"), class = "btn-primary"))
    ),
    br(),
    h4("4. 森林图"),
    plotOutput(ns("forest_plot"), height = "600px"),
    br(),
    h4("5. 漏斗图 (Funnel Plot)"),
    fluidRow(
      column(2, radioButtons(ns("funnel_pkg"), "绘图包",
                             choices = c("meta", "metafor", "metaviz"), selected = "meta", inline = TRUE)),
      column(2, checkboxInput(ns("show_studylab"), "显示研究标签", value = TRUE)),
      column(2, conditionalPanel(
        condition = paste0("input['", ns("funnel_pkg"), "'] == 'meta'"),
        checkboxInput(ns("funnel_contour"), "等高线增强", value = FALSE))),
      column(2, conditionalPanel(
        condition = paste0("input['", ns("funnel_pkg"), "'] == 'meta' && input['", ns("funnel_contour"), "'] == true"),
        textInput(ns("contour_levels"), "等高线水平 (逗号分隔)", value = "0.9,0.95,0.99")))
    ),
    fluidRow(
      column(2, numericInput(ns("xlim_min"), "X轴最小值", value = NA)),
      column(2, numericInput(ns("xlim_max"), "X轴最大值", value = NA)),
      column(2, numericInput(ns("ylim_min"), "Y轴最小值", value = NA)),
      column(2, numericInput(ns("ylim_max"), "Y轴最大值", value = NA))
    ),
    fluidRow(
      column(3, conditionalPanel(
        condition = paste0("input['", ns("funnel_pkg"), "'] == 'meta'"),
        selectInput(ns("metabias_method"), "发表偏倚检验方法",
                    choices = c("Egger (线性回归)" = "linreg",
                                "Begg (秩相关)" = "rank",
                                "Thompson (加权回归)" = "mm"),
                    selected = "linreg"))),
      column(3, conditionalPanel(
        condition = paste0("input['", ns("funnel_pkg"), "'] == 'metaviz'"),
        selectInput(ns("funnel_viz_type"), "图形类型",
                    choices = c("标准漏斗图" = "standard",
                                "等高线增强" = "contour",
                                "Egger's 回归线" = "egger"),
                    selected = "standard"))),
      column(3, downloadButton(ns("download_funnel"), "下载漏斗图 (PNG)"))
    ),
    plotOutput(ns("funnel_plot"), height = "500px"),
    br(),
    h5("发表偏倚检验 (Publication Bias Test)"),
    verbatimTextOutput(ns("funnel_test")),
    br(),
    h4("6. 合并结果"),
    verbatimTextOutput(ns("summary"))
  )
}

classicalMetaServer <- function(id, data_type, outcome, subgroup, study_group_var,
                                selected_studies, current_data) {
  moduleServer(id, function(input, output, session) {

    # ---------- 荟萃分析核心计算 ----------
    meta_result <- eventReactive(input$run, {
      req(selected_studies(), current_data())
      study_vec <- selected_studies()
      data <- current_data()

      has_subgroup <- "subgroup_level" %in% names(data)

      if (has_subgroup) {
        data <- dplyr::filter(data, unique_id %in% study_vec)
        if (length(unique(data$unique_id)) < 2) {
          showNotification("剩余研究不足两项，无法进行分析", type = "error")
          return(NULL)
        }
        data <- dplyr::filter(data, !is.na(subgroup_level))
        if (nrow(data) == 0) {
          showNotification("亚组水平数据缺失，无法分析", type = "error")
          return(NULL)
        }
        data <- data %>% mutate(studlab_modified = ifelse(is_synthetic,
                                                          paste0(study_display, "*"),
                                                          study_display))

        subgroup_name_final <- if (!is.null(subgroup()) && subgroup() != "Overall") {
          subgroup()
        } else if (!is.null(study_group_var()) && study_group_var() != "none") {
          study_group_var()
        } else NULL

        if (data_type() == "binary") {
          m <- metabin(event.e = event_ici, n.e = n_ici,
                       event.c = event_ctrl, n.c = n_ctrl,
                       studlab = studlab_modified,
                       data = data, sm = "OR", method = "MH",
                       common = (input$model == "fixed"),
                       random = (input$model == "random"),
                       subgroup = subgroup_level,
                       print.subgroup.name = FALSE,
                       subgroup.name = subgroup_name_final)
        } else {
          m <- metagen(TE = te, seTE = se_te,
                       studlab = studlab_modified,
                       data = data, sm = "HR", method.tau = "DL",
                       common = (input$model == "fixed"),
                       random = (input$model == "random"),
                       subgroup = subgroup_level,
                       print.subgroup.name = FALSE,
                       subgroup.name = subgroup_name_final)
        }
      } else {
        data <- dplyr::filter(data, unique_id %in% study_vec)
        if (nrow(data) < 2) {
          showNotification("剩余研究不足两项，无法进行分析", type = "error")
          return(NULL)
        }
        if ("is_synthetic" %in% names(data)) {
          data <- data %>% mutate(studlab_modified = ifelse(is_synthetic,
                                                            paste0(study_display, "*"),
                                                            study_display))
        } else {
          data$studlab_modified <- data$study_display
        }

        if (data_type() == "binary") {
          data$OR_true <- (data$event_ici * (data$n_ctrl - data$event_ctrl)) /
            ((data$n_ici - data$event_ici) * data$event_ctrl + 1e-10)
          data <- data[order(data$OR_true, decreasing = FALSE), ]
          data$OR_true <- NULL
          m <- metabin(event.e = event_ici, n.e = n_ici,
                       event.c = event_ctrl, n.c = n_ctrl,
                       studlab = studlab_modified, data = data,
                       sm = "OR", method = "MH",
                       common = (input$model == "fixed"),
                       random = (input$model == "random"))
        } else {
          data <- data[order(data$te), ]
          m <- metagen(TE = te, seTE = se_te,
                       studlab = studlab_modified, data = data,
                       sm = "HR", method.tau = "DL",
                       common = (input$model == "fixed"),
                       random = (input$model == "random"))
        }
      }
      return(m)
    })

    # ---------- 森林图 ----------
    output$forest_plot <- renderPlot({
      m <- meta_result()
      req(m)
      if (data_type() == "binary") {
        weight_col <- if (input$model == "fixed") "w.common" else "w.random"
        weight_lab <- if (input$model == "fixed") "Weight\n(common)" else "Weight\n(random)"
        leftcols <- c("studlab", "event.e", "n.e", "event.c", "n.c", weight_col)
        leftlabs <- c("Study", "ICI\nEvents", " \nTotal", "Control\nEvents",
                      "Control\nTotal", weight_lab)
        rightlabs <- "Odds Ratio\n95% CI"
      } else {
        weight_col <- if (input$model == "fixed") "w.common" else "w.random"
        weight_lab <- "Weight"
        leftcols <- c("studlab", "TE", "seTE", weight_col)
        leftlabs <- c("Study", "log[Hazard Ratio]", "SE", weight_lab)
        rightlabs <- "Hazard Ratio\nIV, Common, 95% CI"
      }
      if (!is.null(m$subgroup)) {
        forest(m, leftcols = leftcols, leftlabs = leftlabs,
               rightcols = c("effect.ci"), rightlabs = rightlabs,
               smlab = "", pooled.totals = (data_type() == "binary"),
               pooled.events = (data_type() == "binary"),
               comb.common = (input$model == "fixed"),
               comb.random = (input$model == "random"),
               test.overall = TRUE, test.subgroup = TRUE,
               mar = c(8, 4, 4, 2) + 0.1, hetstat = TRUE,
               overall.hetstat = TRUE, subgroup.hetstat = TRUE,
               print.subgroup.name = TRUE, fontsize = 9,
               squaresize = 0.6, col.square = "navy",
               col.diamond = "maroon", col.diamond.lines = "maroon",
               sort.subgroup = FALSE)
      } else {
        forest(m, leftcols = leftcols, leftlabs = leftlabs,
               rightcols = c("effect.ci"), rightlabs = rightlabs,
               smlab = "", pooled.totals = (data_type() == "binary"),
               pooled.events = (data_type() == "binary"),
               comb.common = (input$model == "fixed"),
               comb.random = (input$model == "random"),
               hetstat = TRUE, test.overall = TRUE, fontsize = 9,
               squaresize = 0.6, col.square = "navy",
               col.diamond = "maroon", col.diamond.lines = "maroon")
      }
    })

    # ---------- 漏斗图辅助函数 ----------
    parse_lim <- function(min_val, max_val, non_negative = FALSE) {
      min_val <- suppressWarnings(as.numeric(min_val))
      max_val <- suppressWarnings(as.numeric(max_val))
      if (is.na(min_val) || is.na(max_val) || min_val >= max_val) return(NULL)
      if (non_negative && min_val < 0) return(NULL)
      c(min_val, max_val)
    }

    funnel_plot_obj <- reactive({
      m <- meta_result()
      req(m)
      ylab <- if (data_type() == "binary") "Standard Error of log(Odds Ratio)" else "Standard Error of log(Hazard Ratio)"
      xlim <- parse_lim(input$xlim_min, input$xlim_max)
      ylim <- parse_lim(input$ylim_min, input$ylim_max, non_negative = TRUE)

      if (input$funnel_pkg == "meta") {
        contour <- input$funnel_contour
        levels <- if (contour) {
          levs <- tryCatch(as.numeric(strsplit(input$contour_levels, ",")[[1]]),
                           error = function(e) c(0.9, 0.95, 0.99))
          if (any(is.na(levs))) levs <- c(0.9, 0.95, 0.99)
          levs
        } else NULL
        list(type = "meta", m = m, contour = contour, levels = levels,
             show_lab = input$show_studylab, xlim = xlim, ylim = ylim, ylab = ylab)
      } else if (input$funnel_pkg == "metafor") {
        yi <- m$TE; vi <- m$seTE^2
        res <- rma(yi = yi, vi = vi, method = "DL", test = "z")
        res$slab <- m$studlab
        show_lab_val <- input$show_studylab
        if (show_lab_val && length(res$slab) != res$k) {
          showNotification("标签数量与研究数不匹配，已自动禁用标签显示。", type = "warning")
          show_lab_val <- FALSE
        }
        list(type = "metafor", res = res, show_lab = show_lab_val,
             xlim = xlim, ylim = ylim, ylab = ylab)
      } else if (input$funnel_pkg == "metaviz") {
        plot_data <- data.frame(yi = m$TE, vi = m$seTE^2, study = m$studlab)
        if (input$show_studylab) showNotification("metaviz 包不支持直接显示研究标签，请使用 meta 或 metafor。", type = "warning")
        list(type = "metaviz", data = plot_data, viz_type = input$funnel_viz_type,
             show_lab = FALSE, xlim = xlim, ylim = ylim, ylab = ylab)
      }
    })

    output$funnel_plot <- renderPlot({
      obj <- funnel_plot_obj()
      req(obj)
      if (obj$type == "meta") {
        if (obj$contour) {
          funnel(obj$m, contour = TRUE, contour.levels = obj$levels,
                 studlab = obj$show_lab, xlim = obj$xlim, ylim = obj$ylim, ylab = obj$ylab)
        } else {
          funnel(obj$m, studlab = obj$show_lab, xlim = obj$xlim, ylim = obj$ylim, ylab = obj$ylab)
        }
      } else if (obj$type == "metafor") {
        label_arg <- if (obj$show_lab) obj$res$slab else FALSE
        args <- list(x = obj$res, main = "Funnel Plot (metafor)", label = label_arg, ylab = obj$ylab)
        if (!is.null(obj$xlim)) args$xlim <- obj$xlim
        if (!is.null(obj$ylim)) args$ylim <- obj$ylim
        tryCatch(do.call(funnel, args), error = function(e) {
          showNotification(paste("metafor 漏斗图绘制失败：", e$message), type = "error")
          args$ylim <- NULL
          do.call(funnel, args)
        })
      } else if (obj$type == "metaviz") {
        viz_type <- obj$viz_type
        p <- if (viz_type == "standard") viz_funnel(obj$data, method = "FE")
        else if (viz_type == "contour") viz_funnel(obj$data, method = "FE", contours = TRUE)
        else if (viz_type == "egger") viz_funnel(obj$data, method = "FE", egger = TRUE)
        if (!is.null(obj$xlim)) p <- p + ggplot2::xlim(obj$xlim)
        if (!is.null(obj$ylim)) p <- p + ggplot2::ylim(obj$ylim)
        p <- p + labs(y = obj$ylab)
        print(p)
      }
    })

    output$funnel_test <- renderPrint({
      m <- meta_result()
      req(m)
      if (input$funnel_pkg == "meta") {
        method <- input$metabias_method
        if (m$k < 3) { cat("研究数量少于 3，无法进行发表偏倚检验。"); return() }
        tryCatch({
          test <- metabias(m, method.bias = method, k.min = 3)
          print(test)
          cat("\n注：p < 0.05 提示可能存在发表偏倚。")
        }, error = function(e) {
          cat("发表偏倚检验失败：", e$message, "\n可能原因：数据不满足检验假设。")
        }, warning = function(w) {
          cat("警告：", w$message, "\n")
          test <- suppressWarnings(metabias(m, method.bias = method, k.min = 3))
          if (!is.null(test)) { print(test); cat("\n注：p < 0.05 提示可能存在发表偏倚。") }
        })
      } else if (input$funnel_pkg == "metafor") {
        yi <- m$TE; vi <- m$seTE^2
        if (length(yi) < 3) { cat("研究数量少于 3，无法进行发表偏倚检验。"); return() }
        res <- rma(yi = yi, vi = vi, method = "DL")
        tryCatch({
          test <- regtest(res)
          print(test)
          cat("\n注：p < 0.05 提示可能存在发表偏倚。")
        }, error = function(e) { cat("metafor 发表偏倚检验失败：", e$message) })
      } else { cat("metaviz 包不直接提供检验，请切换至 meta 或 metafor。") }
    })

    output$download_funnel <- downloadHandler(
      filename = function() paste0("funnel_plot_", input$funnel_pkg, "_", Sys.Date(), ".png"),
      content = function(file) {
        obj <- funnel_plot_obj()
        req(obj)
        png(file, width = 10, height = 6, units = "in", res = 300)
        if (obj$type == "meta") {
          if (obj$contour) funnel(obj$m, contour = TRUE, contour.levels = obj$levels,
                                  studlab = obj$show_lab, xlim = obj$xlim, ylim = obj$ylim)
          else funnel(obj$m, studlab = obj$show_lab, xlim = obj$xlim, ylim = obj$ylim)
        } else if (obj$type == "metafor") {
          label_arg <- if (obj$show_lab) obj$res$slab else FALSE
          args <- list(x = obj$res, main = "Funnel Plot (metafor)", label = label_arg)
          if (!is.null(obj$xlim)) args$xlim <- obj$xlim
          if (!is.null(obj$ylim)) args$ylim <- obj$ylim
          tryCatch(do.call(funnel, args), error = function(e) { args$ylim <- NULL; do.call(funnel, args) })
        } else if (obj$type == "metaviz") {
          p <- switch(obj$viz_type,
                      standard = viz_funnel(obj$data, method = "FE"),
                      contour  = viz_funnel(obj$data, method = "FE", contours = TRUE),
                      egger    = viz_funnel(obj$data, method = "FE", egger = TRUE))
          if (!is.null(obj$xlim)) p <- p + ggplot2::xlim(obj$xlim)
          if (!is.null(obj$ylim)) p <- p + ggplot2::ylim(obj$ylim)
          print(p)
        }
        dev.off()
      }
    )

    output$summary <- renderPrint({
      m <- meta_result()
      req(m)
      if (!is.null(m$subgroup)) {
        cat("亚组分析结果\n==================================\n")
        print(summary(m))
      } else {
        if (data_type() == "binary") {
          if (input$model == "fixed") {
            cat("固定效应模型 (二分类数据, OR)\n")
            cat("合并 OR: ", round(exp(m$TE.common), 2),
                "\n95% CI: [", round(exp(m$lower.common), 2), ", ",
                round(exp(m$upper.common), 2), "]\n")
            cat("Z = ", round(m$zval.common, 2), ", p = ",
                format.pval(m$pval.common, digits = 4), "\n")
          } else {
            cat("随机效应模型 (二分类数据, OR)\n")
            cat("合并 OR: ", round(exp(m$TE.random), 2),
                "\n95% CI: [", round(exp(m$lower.random), 2), ", ",
                round(exp(m$upper.random), 2), "]\n")
            cat("Z = ", round(m$zval.random, 2), ", p = ",
                format.pval(m$pval.random, digits = 4), "\n")
          }
          cat("\n异质性:\nI² = ", round(m$I2, 1), "%\nτ² = ", round(m$tau2, 4),
              "\nQ = ", round(m$Q, 2), ", df = ", m$k - 1, ", p = ",
              format.pval(m$pval.Q, digits = 4), "\n")
        } else {
          if (input$model == "fixed") {
            cat("固定效应模型 (生存数据, HR)\n")
            cat("合并 HR: ", round(exp(m$TE.common), 2),
                "\n95% CI: [", round(exp(m$lower.common), 2), ", ",
                round(exp(m$upper.common), 2), "]\n")
            cat("Z = ", round(m$zval.common, 2), ", p = ",
                format.pval(m$pval.common, digits = 4), "\n")
          } else {
            cat("随机效应模型 (生存数据, HR)\n")
            cat("合并 HR: ", round(exp(m$TE.random), 2),
                "\n95% CI: [", round(exp(m$lower.random), 2), ", ",
                round(exp(m$upper.random), 2), "]\n")
            cat("Z = ", round(m$zval.random, 2), ", p = ",
                format.pval(m$pval.random, digits = 4), "\n")
          }
          cat("\n异质性:\nI² = ", round(m$I2, 1), "%\nτ² = ", round(m$tau2, 4),
              "\nQ = ", round(m$Q, 2), ", df = ", m$k - 1, ", p = ",
              format.pval(m$pval.Q, digits = 4), "\n")
        }
      }
    })

    # 返回荟萃分析结果和模型选择，供主 server 使用
    return(list(meta_result = meta_result, model = reactive(input$model)))
  })
}
