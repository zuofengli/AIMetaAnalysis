# ==================== 改进版 Shiny 荟萃分析工具 ====================
# 功能：
#   - 从 clinical_studies_data.xlsx 读取研究信息和二分类数据
#   - 筛选研究并选择多项研究进行固定/随机效应荟萃分析
#   - 显示森林图和统计摘要
# 依赖包：shiny, DT, meta, grid, readxl, dplyr

library(shiny)
library(DT)
library(meta)
library(grid)
library(readxl)
library(dplyr)

# ---------- 1. 读取数据 ----------
get_data_file <- function() {
  pkg_file <- system.file("extdata", "clinical_studies_data.xlsx", package = "AIMetaAnalysis")
  if (pkg_file != "") return(pkg_file)
  rel_file <- file.path("extdata", "clinical_studies_data.xlsx")
  if (file.exists(rel_file)) return(rel_file)
  if (file.exists("clinical_studies_data.xlsx")) return("clinical_studies_data.xlsx")
  stop("无法找到 clinical_studies_data.xlsx 文件。请确保文件位于 extdata/ 目录下或当前工作目录中。")
}

data_file <- get_data_file()

# 读取各工作表
studies_info <- read_excel(data_file, sheet = "studies_info")
binary_data  <- read_excel(data_file, sheet = "binary_data")

# ---------- 2. 处理 binary_data 生成 pCR 数据 ----------
# 过滤 removed 列为 YES 的行
binary_data <- binary_data %>% filter(is.na(removed) | removed != "YES")

# 筛选 pCR 相关数据
pcr_raw <- binary_data %>%
  filter(
    outcome == "PathologicalCompleteRespons",
    subgroup == "Overall",
    period == "Overall"
  )

if (nrow(pcr_raw) == 0) {
  stop("binary_data 中未找到符合条件的 pCR 数据（outcome='PathologicalCompleteRespons', subcategory='Overall', period='Overall'）。")
}

# ========== 关键修改：添加调试输出，并明确对照组识别规则 ==========
# 请根据您的实际数据调整下面的对照组识别逻辑！
# 当前规则：treatment 以 "_ctrl" 结尾的为对照组，其余为实验组。
# 如果您的数据中对照组为 "Chemotherapy"，请将规则改为 treatment == "Chemotherapy"
cat("开始处理 pCR 数据...\n")
cat("原始 pcr_raw 行数:", nrow(pcr_raw), "\n")
unique_trials <- unique(pcr_raw$trial)
cat("唯一研究:", paste(unique_trials, collapse = ", "), "\n")

pcr_list <- list()
skipped_studies <- c()

for (s in unique_trials) {
  study_rows <- pcr_raw[pcr_raw$trial == s, ]

  # 对照组：treatment 以 "_ctrl" 结尾（请根据实际数据修改）
  ctrl_idx <- grepl("_ctrl$", study_rows$treatment)
  # 实验组：其余所有
  exp_idx  <- !ctrl_idx

  cat("研究", s, ": 对照组匹配数 =", sum(ctrl_idx), ", 实验组匹配数 =", sum(exp_idx), "\n")

  if (sum(ctrl_idx) != 1) {
    warning(paste("研究", s, "的对照组行数不为1（实际为", sum(ctrl_idx), "），已跳过。"))
    skipped_studies <- c(skipped_studies, s)
    next
  }
  if (sum(exp_idx) != 1) {
    warning(paste("研究", s, "的实验组行数不为1（实际为", sum(exp_idx), "），已跳过。"))
    skipped_studies <- c(skipped_studies, s)
    next
  }

  ctrl_rows <- study_rows[ctrl_idx, ]
  exp_rows  <- study_rows[exp_idx, ]

  pcr_list[[s]] <- data.frame(
    study      = s,
    event_ici  = exp_rows$d,
    n_ici      = exp_rows$n,
    event_ctrl = ctrl_rows$d,
    n_ctrl     = ctrl_rows$n,
    stringsAsFactors = FALSE
  )
}

if (length(pcr_list) == 0) {
  stop("没有可用的研究用于荟萃分析。跳过的研究: ", paste(skipped_studies, collapse = ", "),
       "\n可能原因：对照组 treatment 没有以 '_ctrl' 结尾，或每个研究的实验组/对照组行数不等于1。",
       "\n请检查数据并调整对照组识别规则（例如改为 treatment == 'Chemotherapy'）。")
}

pcr_data <- do.call(rbind, pcr_list)
# 确保列名和顺序正确
pcr_data <- pcr_data[, c("study", "event_ici", "n_ici", "event_ctrl", "n_ctrl")]
print(pcr_data)
cat("成功提取", nrow(pcr_data), "个研究的 pCR 数据。\n")

# ---------- 3. 处理 studies_info，统一研究名称 ----------
if ("trial" %in% names(studies_info)) {
  studies_info$study_name <- studies_info$trial
} else {
  studies_info$study_name <- studies_info$study
}

studies_info$year <- as.integer(studies_info$year)

if (any(duplicated(studies_info$study_name))) {
  warning("studies_info 中存在重复的研究名称，将保留第一条记录。")
  studies_info <- studies_info[!duplicated(studies_info$study_name), ]
}

# ---------- 4. UI ----------
ui <- fluidPage(
  titlePanel("NSCLC 围手术期免疫治疗 pCR 荟萃分析"),

  wellPanel(
    h4("1. 筛选临床研究"),
    fluidRow(
      column(4,
             selectInput("phase_filter", "试验分期",
                         choices = c("全部", unique(studies_info$phase)),
                         selected = "全部")
      ),
      column(4,
             sliderInput("year_filter", "发表年份范围",
                         min = min(studies_info$year, na.rm = TRUE),
                         max = max(studies_info$year, na.rm = TRUE),
                         value = range(studies_info$year, na.rm = TRUE),
                         step = 1, sep = "")
      )
    ),
    br(),
    h4("2. 选择研究（点击行可多选）"),
    DTOutput("study_table"),
    br(),
    textOutput("data_availability")
  ),

  wellPanel(
    h4("3. 分析设置"),
    fluidRow(
      column(3,
             radioButtons("model", "效应模型",
                          choices = c("固定效应" = "fixed",
                                      "随机效应" = "random"),
                          selected = "fixed")
      ),
      column(3,
             br(),
             actionButton("run", "运行荟萃分析", icon = icon("play"),
                          class = "btn-primary")
      )
    ),
    br(),
    h4("4. 森林图"),
    plotOutput("forest_plot", height = "600px"),
    br(),
    h4("5. 合并结果"),
    verbatimTextOutput("summary")
  )
)

# ---------- 5. Server ----------
server <- function(input, output, session) {

  filtered_studies <- reactive({
    df <- studies_info
    if (input$phase_filter != "全部") {
      df <- df[df$phase == input$phase_filter, ]
    }
    df <- df[df$year >= input$year_filter[1] & df$year <= input$year_filter[2], ]
    df
  })

  output$data_availability <- renderText({
    avail <- filtered_studies()$study_name %in% pcr_data$study
    count <- sum(avail)
    paste("当前筛选结果中，", count, "项研究有可用的 pCR 数据。")
  })

  output$study_table <- renderDT({
    display_df <- filtered_studies()[, c("study_name", "phase", "year")]
    datatable(display_df,
              selection = 'multiple',
              options = list(pageLength = 10, autoWidth = TRUE, dom = 'ftp'),
              rownames = FALSE,
              colnames = c("研究名称", "分期", "年份")
    )
  })

  selected_studies <- reactive({
    rows <- input$study_table_rows_selected
    if (is.null(rows)) return(NULL)
    filtered_studies()[rows, "study_name"]
  })

  meta_result <- eventReactive(input$run, {
    req(selected_studies())
    if (nrow(selected_studies()) < 2) {
      showNotification("请至少选择两项研究进行分析", type = "error")
      return(NULL)
    }
    print(selected_studies())
    data <- pcr_data[pcr_data$study %in% selected_studies(), ]
    print(data)


    # 提取研究名称向量
    study_vec <- selected_studies()$study_name   # 或 selected_studies()[[1]]

    # 从 pcr_data 中筛选
    data <- pcr_data[pcr_data$study %in% study_vec, ]

    # 检查缺失的研究
    missing_studies <- setdiff(study_vec, data$study)
    if (length(missing_studies) > 0) {
      showNotification(paste("以下研究没有 pCR 数据，将被忽略：",
                             paste(missing_studies, collapse = ", ")),
                       type = "warning")
    }

    if (nrow(data) < 2) {
      showNotification("剩余研究不足两项，无法进行分析", type = "error")
      return(NULL)
    }

    # 按粗略 OR 排序（避免零事件问题）
    data$OR_true <- (data$event_ici * (data$n_ctrl - data$event_ctrl)) /
      ((data$n_ici - data$event_ici) * data$event_ctrl + 1e-10)
    data <- data[order(data$OR_true, decreasing = FALSE), ]
    data$OR_true <- NULL

    metabin(event.e = event_ici, n.e = n_ici,
            event.c = event_ctrl, n.c = n_ctrl,
            studlab = study,
            data = data,
            sm = "OR",
            method = "MH",
            fixed = (input$model == "fixed"),
            random = (input$model == "random"))
  })

  output$forest_plot <- renderPlot({
    m <- meta_result()
    req(m)

    forest(m,
           leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", "w.fixed"),
           leftlabs = c("Study", "ICI\nEvents", " \nTotal", "Control\nEvents", "Control\nTotal", "Weight\n(fixed)"),
           rightcols = c("effect.ci"),
           rightlabs = "Odds Ratio for pCR",
           smlab = "",
           pooled.totals = TRUE,
           pooled.events = TRUE,
           comb.fixed = (input$model == "fixed"),
           comb.random = (input$model == "random"),
           hetstat = TRUE,
           test.overall = TRUE,
           fontsize = 9,
           squaresize = 0.6,
           col.square = "navy",
           col.diamond = "maroon",
           col.diamond.lines = "maroon"
    )

    # 使用 m$k 获取研究数量，计算动态偏移
    offset <- (m$k - 6) * 0.02   # 可根据实际图形微调系数
    # 确保 offset 非负（当研究数少于5时不向上移动）
    offset <- max(offset, 0)

    if (input$model == "fixed") {
      z_val <- round(m$zval.fixed, 2)
      p_val <- m$pval.fixed
    } else {
      z_val <- round(m$zval.random, 2)
      p_val <- m$pval.random
    }

    p_text <- ifelse(p_val < 0.0001, "<0.0001", paste0("= ", round(p_val, 4)))
    I2_val <- round(m$I2, 1)
    tau2_val <- round(m$tau2, 4)
    p_Q <- m$pval.Q
    df_val <- m$k - 1
    p_Q_text <- ifelse(p_Q < 0.0001, "<0.0001", paste0("= ", round(p_Q, 4)))

    effect_text <- paste0("Test for overall effect (",
                          ifelse(input$model == "fixed", "Fixed effect", "Random effects"),
                          "): Z = ", z_val, " (", p_text, ")")

    # 添加文本，y 坐标减去 offset
    #grid.text(effect_text, x = 0.29, y = 0.33 - offset, just = "center",
    #          gp = gpar(fontsize = 8))
  })

  output$summary <- renderPrint({
    m <- meta_result()
    req(m)

    if (input$model == "fixed") {
      cat("固定效应模型\n")
      cat("合并 OR: ", round(exp(m$TE.fixed), 2), "\n")
      cat("95% CI: [", round(exp(m$lower.fixed), 2), ", ", round(exp(m$upper.fixed), 2), "]\n")
      cat("Z = ", round(m$zval.fixed, 2), ", p = ", format.pval(m$pval.fixed, digits = 4), "\n")
    } else {
      cat("随机效应模型\n")
      cat("合并 OR: ", round(exp(m$TE.random), 2), "\n")
      cat("95% CI: [", round(exp(m$lower.random), 2), ", ", round(exp(m$upper.random), 2), "]\n")
      cat("Z = ", round(m$zval.random, 2), ", p = ", format.pval(m$pval.random, digits = 4), "\n")
    }

    cat("\n异质性:\n")
    cat("I² = ", round(m$I2, 1), "%\n")
    cat("τ² = ", round(m$tau2, 4), "\n")
    cat("Q = ", round(m$Q, 2), ", df = ", m$k - 1, ", p = ", format.pval(m$pval.Q, digits = 4), "\n")
  })
}

# ---------- 6. 运行应用 ----------
shinyApp(ui = ui, server = server)
