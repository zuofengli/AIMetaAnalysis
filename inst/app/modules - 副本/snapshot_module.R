# snapshot_module.R
# 快照捕获、管理、导出功能模块（修复版）

library(shiny)
library(meta)
library(dplyr)

# ---------- 初始化快照存储 ----------
init_snapshot_storage <- function() {
  reactiveVal(list())
}

# ---------- 构建快照对象 ----------
create_snapshot <- function(meta_obj, data_type, outcome, subgroup_name, model, user_insights) {
  # 提取效应量
  if (data_type == "binary") {
    if (model == "fixed") {
      eff <- list(
        measure = "OR",
        value = exp(meta_obj$TE.common),
        lower = exp(meta_obj$lower.common),
        upper = exp(meta_obj$upper.common),
        pval  = meta_obj$pval.common
      )
    } else {
      eff <- list(
        measure = "OR",
        value = exp(meta_obj$TE.random),
        lower = exp(meta_obj$lower.random),
        upper = exp(meta_obj$upper.random),
        pval  = meta_obj$pval.random
      )
    }
  } else {
    if (model == "fixed") {
      eff <- list(
        measure = "HR",
        value = exp(meta_obj$TE.common),
        lower = exp(meta_obj$lower.common),
        upper = exp(meta_obj$upper.common),
        pval  = meta_obj$pval.common
      )
    } else {
      eff <- list(
        measure = "HR",
        value = exp(meta_obj$TE.random),
        lower = exp(meta_obj$lower.random),
        upper = exp(meta_obj$upper.random),
        pval  = meta_obj$pval.random
      )
    }
  }

  # 异质性
  hetero <- list(
    I2     = meta_obj$I2,
    tau2   = meta_obj$tau2,
    Q      = meta_obj$Q,
    Q_df   = meta_obj$df.Q,
    Q_pval = meta_obj$pval.Q
  )

  # 研究层数据（用于精细报告）
  study_data <- data.frame(
    study   = meta_obj$studlab,
    TE      = meta_obj$TE,
    seTE    = meta_obj$seTE,
    stringsAsFactors = FALSE
  )
  if (data_type == "binary") {
    study_data$event_ici <- meta_obj$event.e
    study_data$n_ici     <- meta_obj$n.e
    study_data$event_ctrl<- meta_obj$event.c
    study_data$n_ctrl    <- meta_obj$n.c
    study_data$OR        <- exp(meta_obj$TE)
    study_data$lower     <- exp(meta_obj$lower)
    study_data$upper     <- exp(meta_obj$upper)
    study_data$weight    <- if (model == "fixed") meta_obj$w.common / sum(meta_obj$w.common) * 100 else meta_obj$w.random / sum(meta_obj$w.random) * 100
  } else {
    study_data$HR        <- exp(meta_obj$TE)
    study_data$lower     <- exp(meta_obj$lower)
    study_data$upper     <- exp(meta_obj$upper)
    study_data$weight    <- if (model == "fixed") meta_obj$w.common / sum(meta_obj$w.common) * 100 else meta_obj$w.random / sum(meta_obj$w.random) * 100
  }

  # 亚组信息
  subgroup_info <- NULL
  if (!is.null(meta_obj$subgroup)) {
    subgroup_info <- list(
      name   = meta_obj$subgroup.name,
      levels = unique(meta_obj$subgroup.levels)
    )
  }

  list(
    id            = format(Sys.time(), "%Y%m%d%H%M%S"),
    timestamp     = Sys.time(),
    outcome       = outcome,
    data_type     = data_type,
    model         = model,
    subgroup      = subgroup_info,
    pooled_effect = eff,
    heterogeneity = hetero,
    study_data    = study_data,
    user_insights = user_insights,
    figure_file   = NULL,
    caption       = NULL
  )
}

# ---------- 生成图形文件名和标题 ----------
generate_figure_metadata <- function(snapshot) {
  base <- paste0(
    "fig_", snapshot$outcome, "_",
    ifelse(is.null(snapshot$subgroup), "overall", snapshot$subgroup$name),
    "_", snapshot$id
  )
  snapshot$figure_file <- paste0(base, ".pdf")
  snapshot$caption <- paste0(
    "Forest plot of ", snapshot$outcome,
    if (!is.null(snapshot$subgroup)) paste(" by", snapshot$subgroup$name) else "",
    " (", snapshot$model, " effects model)."
  )
  snapshot
}

# ---------- 独立的绘图保存函数（不依赖 renderPlot）----------
save_forest_plot_to_pdf <- function(meta_obj, data_type, model, pdf_path) {
  # 确保目录存在
  dir.create(dirname(pdf_path), showWarnings = FALSE, recursive = TRUE)

  pdf(pdf_path, width = 12, height = 8)

  # 复制 app.R 中的森林图绘制代码
  if (data_type == "binary") {
    weight_col <- if (model == "fixed") "w.common" else "w.random"
    weight_lab <- if (model == "fixed") "Weight\n(common)" else "Weight\n(random)"
    leftcols <- c("studlab", "event.e", "n.e", "event.c", "n.c", weight_col)
    leftlabs <- c("Study", "ICI\nEvents", " \nTotal", "Control\nEvents", "Control\nTotal", weight_lab)
    rightlabs <- "Odds Ratio\n95% CI"
  } else {
    weight_col <- if (model == "fixed") "w.common" else "w.random"
    weight_lab <- "Weight"
    leftcols <- c("studlab", "TE", "seTE", weight_col)
    leftlabs <- c("Study", "log[Hazard Ratio]", "SE", weight_lab)
    rightcols <- c("effect.ci")
    rightlabs <- "Hazard Ratio\nIV, Common, 95% CI"
  }

  if (!is.null(meta_obj$subgroup)) {
    forest(meta_obj,
           leftcols = leftcols,
           leftlabs = leftlabs,
           rightcols = c("effect.ci"),
           rightlabs = rightlabs,
           smlab = "",
           pooled.totals = (data_type == "binary"),
           pooled.events = (data_type == "binary"),
           comb.common = (model == "fixed"),
           comb.random = (model == "random"),
           test.overall = TRUE,
           test.subgroup = TRUE,
           mar = c(8, 4, 4, 2) + 0.1,
           hetstat = TRUE,
           overall.hetstat = TRUE,
           subgroup.hetstat = TRUE,
           print.subgroup.name = TRUE,
           fontsize = 9,
           squaresize = 0.6,
           col.square = "navy",
           col.diamond = "maroon",
           col.diamond.lines = "maroon",
           sort.subgroup = FALSE)
  } else {
    forest(meta_obj,
           leftcols = leftcols,
           leftlabs = leftlabs,
           rightcols = c("effect.ci"),
           rightlabs = rightlabs,
           smlab = "",
           pooled.totals = (data_type == "binary"),
           pooled.events = (data_type == "binary"),
           comb.common = (model == "fixed"),
           comb.random = (model == "random"),
           hetstat = TRUE,
           test.overall = TRUE,
           fontsize = 9,
           squaresize = 0.6,
           col.square = "navy",
           col.diamond = "maroon",
           col.diamond.lines = "maroon")
  }

  dev.off()
  return(TRUE)
}

# ---------- Shiny 模块：UI 扩展 ----------
snapshotUI <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    h4("报告快照管理"),
    fluidRow(
      column(6,
             textAreaInput(ns("insights"), "分析备注 / Insights",
                           placeholder = "例如：与原始文献的差异、新发现等",
                           width = "100%", rows = 3)
      ),
      column(6,
             br(),
             actionButton(ns("capture"), "捕获当前图表", icon = icon("camera"), class = "btn-success"),
             br(), br(),
             downloadButton(ns("download_package"), "导出报告素材包 (ZIP)", class = "btn-primary")
      )
    ),
    br(),
    verbatimTextOutput(ns("snapshot_summary"))
  )
}

# ---------- Shiny 模块：Server 逻辑 ----------
snapshotServer <- function(id, meta_result, data_type, outcome, subgroup_name, model,
                           export_dir = "www/exports") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 创建导出目录
    if (!dir.exists(export_dir)) dir.create(export_dir, recursive = TRUE)

    # 存储所有快照
    snapshots <- reactiveVal(list())

    # 用户点击捕获
    observeEvent(input$capture, {
      m <- meta_result()
      if (is.null(m)) {
        showNotification("请先运行荟萃分析！", type = "error")
        return()
      }

      # 创建快照对象
      snap <- create_snapshot(
        meta_obj      = m,
        data_type     = data_type(),
        outcome       = outcome(),
        subgroup_name = subgroup_name(),
        model         = model(),
        user_insights = input$insights
      )

      # 生成文件名和标题
      snap <- generate_figure_metadata(snap)

      # 直接保存图形到 PDF（不依赖 renderPlot）
      pdf_path <- file.path(export_dir, snap$figure_file)
      success <- tryCatch({
        save_forest_plot_to_pdf(m, data_type(), model(), pdf_path)
        TRUE
      }, error = function(e) {
        showNotification(paste("保存图形失败：", e$message), type = "error", duration = 10)
        FALSE
      })

      if (!success) return()

      # 将快照加入列表
      current <- snapshots()
      current[[length(current) + 1]] <- snap
      snapshots(current)

      showNotification(paste("快照已保存：", snap$figure_file), type = "message")
    })

    # 显示快照摘要
    output$snapshot_summary <- renderPrint({
      snaps <- snapshots()
      if (length(snaps) == 0) {
        cat("暂无捕获的快照。")
      } else {
        cat("已捕获快照数量：", length(snaps), "\n\n")
        for (i in seq_along(snaps)) {
          s <- snaps[[i]]
          cat(i, ". ", s$caption, "\n", sep = "")
          cat("   合并效应量：", s$pooled_effect$measure, " = ", round(s$pooled_effect$value, 2),
              " [", round(s$pooled_effect$lower, 2), ", ", round(s$pooled_effect$upper, 2), "]\n", sep = "")
          cat("   异质性 I² = ", round(s$heterogeneity$I2, 1), "%\n", sep = "")
          if (!is.null(s$user_insights) && nchar(s$user_insights) > 0) {
            cat("   Insights: ", s$user_insights, "\n")
          }
          cat("\n")
        }
      }
    })

    # 下载素材包
    output$download_package <- downloadHandler(
      filename = function() {
        paste0("meta_report_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        snaps <- snapshots()
        if (length(snaps) == 0) {
          showNotification("没有可导出的快照！", type = "error")
          return(NULL)
        }

        tmpdir <- tempdir()
        pkg_dir <- file.path(tmpdir, "report_package")
        fig_dir <- file.path(pkg_dir, "figures")
        dir.create(fig_dir, recursive = TRUE)

        # 复制 PDF 图形
        copy_success <- TRUE
        for (s in snaps) {
          src <- file.path(export_dir, s$figure_file)
          if (file.exists(src)) {
            file.copy(src, file.path(fig_dir, s$figure_file), overwrite = TRUE)
          } else {
            showNotification(paste("警告：图形文件缺失", s$figure_file), type = "warning")
            copy_success <- FALSE
          }
        }

        if (!copy_success) {
          # 继续打包，但提醒用户
        }

        # 保存快照元数据为 RDS
        saveRDS(snaps, file = file.path(pkg_dir, "snapshots.rds"))

        # 生成人类可读的摘要 CSV
        summary_df <- do.call(rbind, lapply(snaps, function(s) {
          data.frame(
            ID = s$id,
            Outcome = s$outcome,
            Subgroup = ifelse(is.null(s$subgroup), "Overall", s$subgroup$name),
            Effect = paste0(round(s$pooled_effect$value, 2), " [",
                            round(s$pooled_effect$lower, 2), ", ",
                            round(s$pooled_effect$upper, 2), "]"),
            I2 = round(s$heterogeneity$I2, 1),
            Insights = ifelse(is.null(s$user_insights), "", s$user_insights)
          )
        }))
        write.csv(summary_df, file = file.path(pkg_dir, "snapshots_summary.csv"), row.names = FALSE)

        # 打包
        old_wd <- getwd()
        setwd(tmpdir)
        if (requireNamespace("zip", quietly = TRUE)) {
          zip::zipr(file, files = "report_package")
        } else {
          # 使用基础 R 的 zip 命令（Windows 可能需要 Rtools）
          utils::zip(file, files = "report_package")
        }
        setwd(old_wd)
      }
    )

    # 返回快照列表（供外部使用）
    return(snapshots)
  })
}
