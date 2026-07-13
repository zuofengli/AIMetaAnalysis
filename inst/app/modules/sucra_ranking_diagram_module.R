# modules/sucra_ranking_diagram_module.R
# 排名图模块（支持方向动态切换）
# 支持 SUCRA 条形图、Rankogram（按排名）、Rankogram（按治疗）
# 提供矢量图下载功能（PDF）

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DT)

sucraDiagramUI <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- 方向控制 + 当前结局 ----
    fluidRow(
      column(6,
             style = "background-color: #f9f9f9; padding: 10px; border-radius: 5px; border: 1px solid #ddd; margin-bottom: 15px;",
             strong("📌 当前结局："),
             textOutput(ns("current_outcome_display"), inline = TRUE),
             br(),
             radioButtons(ns("effect_direction"),
                          "效应量有利方向 (影响SUCRA排名)",
                          choices = c("数值越小越有利" = "lower_better",
                                      "数值越大越有利" = "higher_better"),
                          selected = "higher_better")
      ),
      column(6,
             style = "margin-top: 28px;",
             helpText("💡 SUCRA 值越高表示该方向上表现越好。",
                      br(),
                      "例如：若选择“数值越小越有利”，则效应量小的治疗 SUCRA 更高。",
                      br(),
                      "若选择“数值越大越有利”，则效应量大的治疗 SUCRA 更高。")
      )
    ),

    fluidRow(
      column(12,
             p(style = "background-color: #f0f8ff; padding: 10px; border-radius: 5px; border-left: 4px solid #4682b4;",
               strong("📖 解读提示："),
               "本排名图表严格遵循 NMA 标准解读原则：<b>排名 1 (Rank 1) 始终代表对患者最有利的治疗方案</b>。",
               "无论结局是有益指标（如 pCR）还是有害指标（如 EFS/死亡），排名靠前均代表临床获益最大。")
      )
    ),
    fluidRow(
      column(4,
             radioButtons(ns("plot_type"), "选择排名图类型",
                          choices = c(
                            "SUCRA 条形图" = "sucra",
                            "Rankogram（按排名）" = "rankogram",
                            "Rankogram（按治疗）" = "rankogram_flip"
                          ),
                          selected = "sucra", inline = TRUE)
      ),
      column(4,
             actionButton(ns("show_rank_diagram"),
                          "绘制/更新图形",
                          icon = icon("chart-bar"),
                          class = "btn-info")
      ),
      column(4,
             downloadButton(ns("download_rank_diagram"),
                            "下载矢量图 (PDF)",
                            class = "btn-success")
      )
    ),
    h4("治疗排名 (SUCRA)"),
    DTOutput(ns("rank_table")),
    br(),
    plotOutput(ns("ranking_diagram"), height = "500px"),
    htmlOutput(ns("rank_caption"),
               style = "text-align: center; font-style: italic; margin-top: -10px; margin-bottom: 20px; color: #555555;"),
    br()
  )
}

sucraDiagramServer <- function(id, rank_result, outcome, nma_info = NULL,
                               subgroup = NULL, selected_levels = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ---- 显示当前结局（安全版） ----
    output$current_outcome_display <- renderText({
      if (is.null(outcome()) || outcome() == "") "未选择结局" else outcome()
    })

    # ---- 获取亚组信息 ----
    subgroup_val <- reactive({
      if (!is.null(subgroup) && is.function(subgroup)) subgroup() else subgroup
    })
    selected_levels_val <- reactive({
      if (!is.null(selected_levels) && is.function(selected_levels)) selected_levels() else selected_levels
    })

    # ---- 显示当前结局（安全版） ----
    output$current_outcome_display <- renderText({
      if (is.null(outcome()) || outcome() == "") "未选择结局" else outcome()
    })



    # ---- 根据方向调整排名数据 ----
    adjusted_rank_data <- reactive({
      rank_list <- rank_result()
      if (is.null(rank_list)) return(NULL)

      # 提取 SUCRA 数据框
      sucra_df <- if (is.list(rank_list) && !is.data.frame(rank_list)) {
        rank_list$sucra
      } else {
        rank_list
      }
      if (is.null(sucra_df) || nrow(sucra_df) == 0) return(NULL)
      if (!"SUCRA" %in% names(sucra_df)) {
        if (ncol(sucra_df) >= 2) names(sucra_df)[2] <- "SUCRA"
        else return(NULL)
      }

      # 提取 rank_probs（贝叶斯可能有）
      rank_probs <- if (is.list(rank_list) && !is.data.frame(rank_list)) {
        rank_list$rank_probs
      } else {
        NULL
      }

      # 复制数据，避免修改原始
      adjusted_sucra <- sucra_df
      adjusted_probs <- rank_probs

      # 根据方向调整
      if (input$effect_direction == "lower_better") {
        # 反转 SUCRA
        adjusted_sucra$SUCRA <- 1 - adjusted_sucra$SUCRA
        # 如果有 rank_probs，镜像反转列（Rank 1 ↔ Rank K）
        if (!is.null(adjusted_probs) && is.matrix(adjusted_probs)) {
          adjusted_probs <- adjusted_probs[, ncol(adjusted_probs):1, drop = FALSE]
        }
      }
      # 无论方向，按 SUCRA 降序排列
      adjusted_sucra <- adjusted_sucra[order(-adjusted_sucra$SUCRA), ]

      list(sucra = adjusted_sucra, rank_probs = adjusted_probs)
    })

    # ---- 存储绘图类型（供下载使用） ----
    current_plot_type <- reactiveVal("sucra")

    # ---------- 绘图函数（均返回 ggplot 对象） ----------

    # 1. SUCRA 条形图
    plot_sucra <- function(sucra_df) {
      if (is.null(sucra_df) || nrow(sucra_df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "无 SUCRA 数据") +
                 theme_void())
      }

      # 确保列名正确
      if (!"Treatment" %in% names(sucra_df) || !"SUCRA" %in% names(sucra_df)) {
        if (ncol(sucra_df) >= 2) {
          names(sucra_df)[1:2] <- c("Treatment", "SUCRA")
        } else {
          return(ggplot() +
                   annotate("text", x = 0.5, y = 0.5, label = "SUCRA 数据格式错误") +
                   theme_void())
        }
      }

      plot_df <- sucra_df[, c("Treatment", "SUCRA")]
      plot_df$SUCRA <- as.numeric(plot_df$SUCRA)
      plot_df <- plot_df[!is.na(plot_df$SUCRA), ]

      if (nrow(plot_df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "SUCRA 均为 NA") +
                 theme_void())
      }

      # 按 SUCRA 升序排列因子（SUCRA 越高排在越上方）
      plot_df$Treatment <- factor(plot_df$Treatment,
                                  levels = plot_df$Treatment[order(plot_df$SUCRA)])

      max_sucra <- max(plot_df$SUCRA, na.rm = TRUE)
      if (max_sucra == 0) max_sucra <- 1

      p <- ggplot(plot_df, aes(x = SUCRA, y = Treatment)) +
        geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
        geom_text(aes(label = round(SUCRA, 3)), hjust = -0.2, size = 4) +
        labs(x = "SUCRA", y = "Treatment") +
        scale_x_continuous(limits = c(0, max_sucra * 1.1)) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(
            size = 13, face = "plain", color = "black", margin = margin(r = 5)
          )
        )
      return(p)
    }

    # 2. Rankogram（按排名分组）
    plot_rankogram <- function(rank_probs) {
      if (is.null(rank_probs)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "无排名概率数据\n（仅贝叶斯模型支持）") +
                 theme_void())
      }
      if (!is.matrix(rank_probs) && !is.data.frame(rank_probs)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "排名概率数据格式错误") +
                 theme_void())
      }

      rp <- unclass(rank_probs)
      rp <- matrix(as.numeric(rp), nrow = nrow(rank_probs), ncol = ncol(rank_probs),
                   dimnames = dimnames(rank_probs))

      df <- as.data.frame(rp)
      df$Treatment <- rownames(df)
      if (is.null(rownames(df)) || all(rownames(df) == "")) {
        rownames(df) <- paste0("Trt", seq_len(nrow(df)))
        df$Treatment <- rownames(df)
      }
      rank_cols <- setdiff(names(df), "Treatment")
      rank_nums <- seq_along(rank_cols)

      df_long <- tryCatch({
        tidyr::pivot_longer(df, cols = all_of(rank_cols),
                            names_to = "RankCol", values_to = "Probability")
      }, error = function(e) {
        warning("pivot_longer 失败: ", e$message)
        NULL
      })
      if (is.null(df_long) || nrow(df_long) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "数据转换失败") +
                 theme_void())
      }

      df_long$RankNum <- rank_nums[match(df_long$RankCol, rank_cols)]
      df_long$RankCol <- NULL

      avg_rank <- tapply(df_long$RankNum * df_long$Probability,
                         df_long$Treatment, sum, na.rm = TRUE)
      trt_order <- names(sort(avg_rank))
      df_long$Treatment <- factor(df_long$Treatment, levels = trt_order)

      p <- ggplot(df_long, aes(x = factor(RankNum), y = Probability, fill = Treatment)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
        labs(x = "排名 (Rank)", y = "概率", fill = "治疗") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.position = "bottom",
              legend.box = "vertical") +
        guides(fill = guide_legend(nrow = ceiling(length(unique(df_long$Treatment)) / 4)))
      return(p)
    }

    # 3. Rankogram（按治疗分组）
    plot_rankogram_flip <- function(rank_probs) {
      if (is.null(rank_probs)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "无排名概率数据\n（仅贝叶斯模型支持）") +
                 theme_void())
      }
      if (!is.matrix(rank_probs) && !is.data.frame(rank_probs)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "排名概率数据格式错误") +
                 theme_void())
      }

      rp <- unclass(rank_probs)
      rp <- matrix(as.numeric(rp), nrow = nrow(rank_probs), ncol = ncol(rank_probs),
                   dimnames = dimnames(rank_probs))
      df <- as.data.frame(rp)
      df$Treatment <- rownames(df)
      rank_cols <- setdiff(names(df), "Treatment")
      rank_nums <- seq_along(rank_cols)

      df_long <- tryCatch({
        tidyr::pivot_longer(df, cols = all_of(rank_cols),
                            names_to = "RankCol", values_to = "Probability")
      }, error = function(e) {
        warning("pivot_longer 失败: ", e$message)
        NULL
      })
      if (is.null(df_long) || nrow(df_long) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "数据转换失败") +
                 theme_void())
      }
      df_long$RankNum <- rank_nums[match(df_long$RankCol, rank_cols)]
      df_long$RankCol <- NULL

      p <- ggplot(df_long, aes(x = Treatment, y = Probability, fill = factor(RankNum))) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
        labs(x = "治疗方案", y = "概率", fill = "排名") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
      return(p)
    }

    # ---------- 主图形输出（基于调整后的数据，自动响应方向变化） ----------
    output$ranking_diagram <- renderPlot({
      rank_list <- adjusted_rank_data()
      if (is.null(rank_list)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5, label = "无排名数据，请先运行 NMA") +
                 theme_void())
      }

      sucra_df <- rank_list$sucra
      rank_probs <- rank_list$rank_probs

      plot_type <- input$plot_type
      current_plot_type(plot_type)  # 记录当前类型供下载使用

      if (plot_type == "sucra") {
        p <- plot_sucra(sucra_df)
      } else if (plot_type %in% c("rankogram", "rankogram_flip")) {
        # 检查是否有 rank_probs
        if (is.null(rank_probs)) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "当前结果不支持 Rankogram\n（仅贝叶斯模型提供）") +
            theme_void()
        } else {
          if (plot_type == "rankogram") {
            p <- plot_rankogram(rank_probs)
          } else {
            p <- plot_rankogram_flip(rank_probs)
          }
        }
      } else {
        p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "未知图形类型") + theme_void()
      }
      print(p)
    })

    # ---------- SUCRA 排名表格（基于调整后的数据） ----------
    output$rank_table <- renderDT({
      rank_list <- adjusted_rank_data()
      if (is.null(rank_list)) {
        return(datatable(data.frame(提示 = "暂无排名数据，请先运行 NMA"),
                         options = list(dom = 't')))
      }
      df <- rank_list$sucra
      if (is.null(df) || nrow(df) == 0) {
        return(datatable(data.frame(提示 = "暂无排名数据"),
                         options = list(dom = 't')))
      }
      datatable(df, options = list(pageLength = 10, dom = 't', autoWidth = TRUE),
                rownames = FALSE) %>%
        formatRound(columns = "SUCRA", digits = 3)
    })

    # ---------- 动态图注 ----------
    output$rank_caption <- renderUI({
      rank_list <- adjusted_rank_data()
      if (is.null(rank_list)) return(NULL)
      out <- outcome()
      if (is.null(out) || out == "") out <- "Selected Outcome"

      fig_num <- switch(input$plot_type,
                        "sucra" = "3a",
                        "rankogram" = "3b",
                        "rankogram_flip" = "3c")
      fig_title <- switch(input$plot_type,
                          "sucra" = "SUCRA ranking diagram",
                          "rankogram" = "Rankogram (Rank 1 = Best treatment)",
                          "rankogram_flip" = "Rank probability grouped by treatment")

      paste0("Fig ", fig_num, ". ", fig_title, " for ", out)
    })

    # ---------- “绘制/更新”按钮（由于响应式自动更新，此处仅作提示） ----------
    observeEvent(input$show_rank_diagram, {
      showNotification("图形已自动根据当前设置更新", type = "message", duration = 2)
    })

    # ---------- 下载矢量图 (PDF) ----------
    output$download_rank_diagram <- downloadHandler(
      filename = function() {
        # 构造文件名：{method_family}-{outcome_clean}-{model_type}-Ranking_diagram_日期.pdf
        out <- outcome()
        if (is.null(out) || out == "") out <- "outcome"
        out_clean <- gsub("[^A-Za-z0-9]", "_", out)

        # ---- 新增：获取亚组信息 ----
        sub <- subgroup_val()
        sel <- selected_levels_val()
        sub_suffix <- ""
        if (!is.null(sub) && sub != "Overall" && length(sel) == 1 && !is.null(sel) && sel != "") {
          sub_suffix <- paste0("_", gsub("[^A-Za-z0-9]", "_", sel[1]))
        } else if (!is.null(sub) && sub != "Overall" && length(sel) > 1) {
          sub_suffix <- "_multi"
        }

        # 从 nma_info 获取方法信息（若未提供则使用默认）
        if (!is.null(nma_info)) {
          info <- nma_info()
          method_family <- info$method_family %||% "unknown"
          model_type <- info$model_type %||% "unknown"
        } else {
          method_family <- "unknown"
          model_type <- "unknown"
        }

        # 组合文件名：方法-结局[亚组]-模型-Ranking_diagram_日期.pdf
        paste0(method_family, "-", out_clean, "-",sub,"-", sub_suffix, "-", model_type,
               "-Ranking_diagram_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        rank_list <- adjusted_rank_data()
        if (is.null(rank_list)) return()

        sucra_df <- rank_list$sucra
        rank_probs <- rank_list$rank_probs

        if (is.null(sucra_df) || nrow(sucra_df) == 0) return()

        plot_type <- current_plot_type()  # 使用最近绘制的类型

        p <- switch(plot_type,
                    "sucra" = plot_sucra(sucra_df),
                    "rankogram" = {
                      if (is.null(rank_probs)) {
                        ggplot() + annotate("text", x=0.5, y=0.5, label="Rankogram 不可用") + theme_void()
                      } else {
                        plot_rankogram(rank_probs)
                      }
                    },
                    "rankogram_flip" = {
                      if (is.null(rank_probs)) {
                        ggplot() + annotate("text", x=0.5, y=0.5, label="Rankogram 不可用") + theme_void()
                      } else {
                        plot_rankogram_flip(rank_probs)
                      }
                    },
                    {
                      # 默认回退
                      plot_sucra(sucra_df)
                    }
        )

        if (is.null(p)) return()
        ggsave(file, plot = p, device = "pdf", width = 10, height = 6, units = "in")
      }
    )

  })
}
