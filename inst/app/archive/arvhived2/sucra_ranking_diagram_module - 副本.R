# modules/sucra_ranking_diagram_module.R
# 排名图模块（无方向选择，Rank 1 永远代表最佳治疗方案）

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DT)

sucraDiagramUI <- function(id) {
  ns <- NS(id)
  tagList(
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
      )
    ),
    h4("治疗排名 (SUCRA)"),
    DTOutput(ns("rank_table")),
    br(),
    plotOutput(ns("ranking_diagram"), height = "500px"),

    # ===== 新增：渲染图注的 HTML 容器 =====
    htmlOutput(ns("rank_caption"),
               style = "text-align: center; font-style: italic; margin-top: -10px; margin-bottom: 20px; color: #555555;"),
    br()
  )
}

sucraDiagramServer <- function(id, rank_result, outcome) {
  moduleServer(id, function(input, output, session) {

    # 用于存储当前的 SUCRA 表格数据
    current_sucra_df <- reactiveVal(NULL)

    # ---------- 绘制 SUCRA 条形图 ----------
    plot_sucra <- function(sucra_df) {
      if (is.null(sucra_df) || nrow(sucra_df) == 0) {
        plot(1, type = "n", axes = FALSE, main = "无 SUCRA 数据")
        text(1, 1, "请先成功运行 NMA 并获得 SUCRA 排名")
        return()
      }

      # 确保列名正确
      if (!"Treatment" %in% names(sucra_df) || !"SUCRA" %in% names(sucra_df)) {
        if (ncol(sucra_df) >= 2) {
          names(sucra_df)[1:2] <- c("Treatment", "SUCRA")
        } else {
          plot(1, type = "n", axes = FALSE, main = "SUCRA 数据格式错误")
          text(1, 1, "需要包含 Treatment 和 SUCRA 列")
          return()
        }
      }

      plot_df <- sucra_df[, c("Treatment", "SUCRA")]
      plot_df$SUCRA <- as.numeric(plot_df$SUCRA)
      plot_df <- plot_df[!is.na(plot_df$SUCRA), ]

      if (nrow(plot_df) == 0) {
        plot(1, type = "n", axes = FALSE, main = "SUCRA 均为 NA")
        return()
      }

      # 按 SUCRA 升序排列因子（SUCRA 越高排在越上方，代表越好）
      plot_df$Treatment <- factor(plot_df$Treatment,
                                  levels = plot_df$Treatment[order(plot_df$SUCRA)])

      # 【修改】：删除了原本写在图里面的 title = "..."，改由外部 HTML 渲染 Caption
      p <- ggplot(plot_df, aes(x = SUCRA, y = Treatment)) +
        geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
        geom_text(aes(label = round(SUCRA, 3)), hjust = -0.2, size = 4) +
        labs(x = "SUCRA", y = "Treatment") +
        xlim(0, max(plot_df$SUCRA) * 1.1) +
        #theme_minimal(base_size = 12) +
        #theme(panel.grid.major.y = element_blank())
        theme_minimal(base_size = 12) +
        theme(
          # 去掉 Y 轴的横向网格线
          panel.grid.major.y = element_blank(),

          # ================= 【新增】调整左侧标签字体 =================
          axis.text.y = element_text(
            size = 13,           # 字体大小（根据您需求的偏好可设为 8~10 或更大）
            face = "plain",     # 字体样式：plain(常规), bold(加粗), italic(斜体)
            color = "black",    # 文字颜色
            margin = margin(r = 5) # 标签到图形的右侧间距（单位：pt）
          )
          # ============================================================
        )



      print(p)
    }

    # ---------- 绘制 Rankogram（按排名分组）----------
    plot_rankogram <- function(rank_probs) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        plot(1, type = "n", axes = FALSE, main = "请安装 ggplot2")
        text(1, 1, "install.packages('ggplot2')")
        return()
      }
      if (is.null(rank_probs)) {
        plot(1, type = "n", axes = FALSE, main = "无排名概率数据")
        text(1, 1, "当前 NMA 结果不包含排名概率矩阵\n（仅贝叶斯模型支持）")
        return()
      }
      if (!is.matrix(rank_probs) && !is.data.frame(rank_probs)) {
        plot(1, type = "n", axes = FALSE, main = "排名概率数据格式错误")
        text(1, 1, "rank_probs 不是矩阵或数据框")
        return()
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
        plot(1, type = "n", axes = FALSE, main = "数据转换失败")
        return()
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
      print(p)
    }

    # ---------- 绘制 Rankogram（按治疗分组）----------
    plot_rankogram_flip <- function(df_long) {
      p <- ggplot(df_long, aes(x = Treatment, y = Probability, fill = factor(RankNum))) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
        labs(x = "治疗方案", y = "概率", fill = "排名") +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
      print(p)
    }

    # ---------- 主事件：更新 ----------
    observeEvent(input$show_rank_diagram, {
      rank_list <- rank_result()

      if (is.null(rank_list)) {
        current_sucra_df(NULL)
        output$ranking_diagram <- renderPlot({
          plot(1, type = "n", axes = FALSE, main = "无排名数据")
          text(1, 1, "请先运行 NMA 分析")
        })
        return()
      }

      # 1. 提取 SUCRA 数据框
      sucra_df <- if (is.list(rank_list) && !is.data.frame(rank_list)) {
        rank_list$sucra
      } else {
        rank_list
      }

      # 2. 处理数据（绝对不做任何人为翻转，保留数学原值）
      if (!is.null(sucra_df) && nrow(sucra_df) > 0) {
        if (!"SUCRA" %in% names(sucra_df)) {
          if (ncol(sucra_df) >= 2) {
            names(sucra_df)[2] <- "SUCRA"
          } else {
            current_sucra_df(NULL)
            output$ranking_diagram <- renderPlot({
              plot(1, type = "n", axes = FALSE, main = "SUCRA 数据列缺失")
            })
            return()
          }
        }
        # 按 SUCRA 降序排列（最佳在前）
        sucra_df <- sucra_df[order(-sucra_df$SUCRA), ]
        current_sucra_df(sucra_df)
      } else {
        current_sucra_df(NULL)
      }

      # 3. 绘图
      output$ranking_diagram <- renderPlot({
        if (input$plot_type == "sucra") {
          plot_sucra(sucra_df)
        } else {
          # Rankogram 部分
          rank_probs <- if (is.list(rank_list) && !is.data.frame(rank_list)) {
            rank_list$rank_probs
          } else {
            NULL
          }
          if (is.null(rank_probs)) {
            plot(1, type = "n", axes = FALSE, main = "无排名概率数据")
            return()
          }
          if (input$plot_type == "rankogram") {
            plot_rankogram(rank_probs)
          } else if (input$plot_type == "rankogram_flip") {
            # 生成 df_long (去除所有翻转逻辑)
            rp <- unclass(rank_probs)
            rp <- matrix(as.numeric(rp), nrow = nrow(rank_probs), ncol = ncol(rank_probs),
                         dimnames = dimnames(rank_probs))
            df <- as.data.frame(rp)
            df$Treatment <- rownames(df)
            rank_cols <- setdiff(names(df), "Treatment")
            rank_nums <- seq_along(rank_cols)
            df_long <- tidyr::pivot_longer(df, cols = all_of(rank_cols),
                                           names_to = "RankCol", values_to = "Probability")
            df_long$RankNum <- rank_nums[match(df_long$RankCol, rank_cols)]
            df_long$RankCol <- NULL
            plot_rankogram_flip(df_long)
          }
        }
      })
    })

    # ===== 新增：动态渲染图注 Caption =====
    output$rank_caption <- renderUI({
      # 确保只有在有数据时才渲染图注
      req(current_sucra_df())

      # 获取传进来的结局指标
      out <- outcome()
      if (is.null(out) || out == "") out <- "Selected Outcome"

      # 根据选择的图表类型，自动匹配图号 (3a, 3b, 3c) 和相应的文字描述
      fig_num <- switch(input$plot_type,
                        "sucra" = "3a",
                        "rankogram" = "3b",
                        "rankogram_flip" = "3c")
      fig_title <- switch(input$plot_type,
                          "sucra" = "SUCRA ranking diagram",
                          "rankogram" = "Rankogram (Rank 1 = Best treatment)",
                          "rankogram_flip" = "Rank probability grouped by treatment")

      # 拼接最终的图注文字
      paste0("Fig ", fig_num, ". ", fig_title, " for ", out)
    })

    # ---------- 渲染 SUCRA 排名表格 ----------
    output$rank_table <- renderDT({
      df <- current_sucra_df()
      if (is.null(df) || nrow(df) == 0) {
        return(datatable(data.frame(提示 = "暂无排名数据，请点击“绘制/更新图形”"),
                         options = list(dom = 't')))
      }
      datatable(df, options = list(pageLength = 10, dom = 't', autoWidth = TRUE),
                rownames = FALSE) %>%
        formatRound(columns = "SUCRA", digits = 3)
    })

  })
}
