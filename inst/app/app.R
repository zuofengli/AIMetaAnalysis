# ==================== 改进版 Shiny 荟萃分析工具 ====================
# 功能：
#   - 从 clinical_studies_data.xlsx 读取研究信息和二分类数据
#   - 筛选研究，动态选择结局指标和亚组，并选择多项研究进行固定/随机效应荟萃分析
#   - 支持亚组分析（当选择具体亚组变量时，自动按所有水平分组）
#   - 显示森林图和统计摘要
# 依赖包：shiny, DT, meta, grid, readxl, dplyr, tidyr, stringr

library(shiny)
library(DT)
library(meta)
library(grid)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

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

# ---------- 2. 处理 binary_data，过滤 removed 列 ----------
binary_data <- binary_data %>% filter(is.na(removed) | removed != "YES")

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
  titlePanel("NSCLC 围手术期免疫治疗荟萃分析"),

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


    fluidRow(
      column(2, actionButton("select_all", "全选", icon = icon("check-square"))),
      column(2, actionButton("clear_selection", "清除", icon = icon("times")))
    ),

    DTOutput("study_table"),
    br(),
    textOutput("data_availability")
  ),

  # ---------- 结局指标与亚组选择 ----------
  wellPanel(
    h4("2.5 选择结局指标与亚组"),
    fluidRow(
      column(6, uiOutput("outcome_ui")),
      column(6, uiOutput("subgroup_ui"))  # 亚组变量选择（含"无亚组"选项，默认Overall）
    )
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

  # 创建表格代理
  proxy <- dataTableProxy('study_table')


  # 全选按钮
  observeEvent(input$select_all, {
    req(filtered_studies())
    selectRows(proxy, 1:nrow(filtered_studies()))
  })

  # 清除选择按钮
  observeEvent(input$clear_selection, {
    selectRows(proxy, NULL)
  })

  # 筛选研究（基于分期和年份）
  filtered_studies <- reactive({
    df <- studies_info
    if (input$phase_filter != "全部") {
      df <- df[df$phase == input$phase_filter, ]
    }
    df <- df[df$year >= input$year_filter[1] & df$year <= input$year_filter[2], ]
    df
  })

  # 显示研究表格
  output$study_table <- renderDT({
    display_df <- filtered_studies()[, c("study_name", "phase", "year")]
    datatable(display_df,
              selection = 'multiple',
              options = list(pageLength = 10, autoWidth = TRUE, dom = 'ftp'),
              rownames = FALSE,
              colnames = c("研究名称", "分期", "年份")
    )
  })

  # 获取用户选中的研究名称
  selected_studies <- reactive({
    rows <- input$study_table_rows_selected
    if (is.null(rows)) return(NULL)
    filtered_studies()[rows, "study_name", drop = FALSE]
  })

  # ---------- 动态生成结局指标和亚组的下拉菜单 ----------
  output$outcome_ui <- renderUI({
    outcomes <- unique(binary_data$outcome)
    outcomes <- outcomes[!is.na(outcomes)]
    selectInput("outcome", "结局指标",
                choices = outcomes,
                selected = outcomes[1])
  })

  # 当结局指标改变时，更新亚组变量选项，并默认选中 "Overall"（如果存在）
  observeEvent(input$outcome, {
    req(input$outcome)
    subgroups <- binary_data %>%
      filter(outcome == input$outcome) %>%
      pull(subgroup) %>%
      unique() %>%
      na.omit()
    # 添加"无亚组"选项（值空）
    subgroup_choices <- c("无亚组" = "", subgroups)
    # 如果 subgroups 包含 "Overall"，则默认选中它；否则选空
    default_selected <- if ("Overall" %in% subgroups) "Overall" else ""
    updateSelectInput(session, "subgroup",
                      choices = subgroup_choices,
                      selected = default_selected)
  })

  output$subgroup_ui <- renderUI({
    selectInput("subgroup", "亚组变量", choices = NULL)
  })

  # ---------- 根据选择的结局和亚组变量提取可用数据 ----------
  current_data <- reactive({
    req(input$outcome)

    # 基础过滤：结局指标
    filtered <- binary_data %>%
      filter(outcome == input$outcome)

    # ========== 亚组分析分支：用户选择了具体的亚组变量（非空、非"Overall"） ==========
    if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {

      # 过滤出当前亚组变量的数据
      filtered <- filtered %>% filter(subgroup == input$subgroup)

      # 获取该亚组下的所有水平（来自 subgroup_option 列）
      level_data <- filtered %>%
        filter(!is.na(subgroup_option)) %>%
        mutate(level = subgroup_option)

      if (nrow(level_data) == 0) {
        showNotification("当前选择的亚组变量下没有可用水平数据", type = "warning")
        return(NULL)
      }

      # 识别试验组和对照组
      level_data <- level_data %>%
        mutate(arm = if_else(grepl("_ctrl$", treatment), "ctrl", "ici"))

      # 按研究、水平、臂汇总
      study_levels <- level_data %>%
        group_by(trial, level, arm) %>%
        summarise(d = sum(d, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(id_cols = c(trial, level), names_from = arm, values_from = c(d, n)) %>%
        filter(!is.na(d_ici) & !is.na(n_ici) & !is.na(d_ctrl) & !is.na(n_ctrl))

      if (nrow(study_levels) == 0) {
        showNotification("没有研究同时包含试验组和对照组数据", type = "warning")
        return(NULL)
      }

      # ---------- 新增：计算每个研究的 OR 近似值，并按亚组内 OR 升序排列 ----------
      study_levels <- study_levels %>%
        mutate(OR_approx = (d_ici * (n_ctrl - d_ctrl)) /
                 ((n_ici - d_ici) * d_ctrl + 1e-10)) %>%   # 加极小值避免除零
        arrange(level, OR_approx) %>%                                 # 先按亚组水平，再按 OR 排序
        select(-OR_approx)                                            # 移除辅助列

      # 重命名并添加 subgroup_level 列
      result <- study_levels %>%
        rename(study = trial, event_ici = d_ici, n_ici = n_ici,
               event_ctrl = d_ctrl, n_ctrl = n_ctrl, subgroup_level = level) %>%
        select(study, event_ici, n_ici, event_ctrl, n_ctrl, subgroup_level)

      return(result)
    }

    # ========== 无亚组模式（包括"无亚组"和选中"Overall"） ==========
    else {
      # 只使用 subgroup == "Overall" 的行
      filtered_overall <- filtered %>% filter(subgroup == "Overall")

      if (nrow(filtered_overall) == 0) {
        showNotification("没有找到总体（Overall）数据", type = "warning")
        return(NULL)
      }

      unique_trials <- unique(filtered_overall$trial)
      data_list <- list()
      skipped <- c()

      for (s in unique_trials) {
        study_rows <- filtered_overall[filtered_overall$trial == s, ]

        # 对照组识别规则：treatment 以 "_ctrl" 结尾
        ctrl_idx <- grepl("_ctrl$", study_rows$treatment)
        exp_idx  <- !ctrl_idx

        if (sum(ctrl_idx) != 1 || sum(exp_idx) != 1) {
          skipped <- c(skipped, s)
          next
        }

        ctrl_rows <- study_rows[ctrl_idx, ]
        exp_rows  <- study_rows[exp_idx, ]

        data_list[[s]] <- data.frame(
          study      = s,
          event_ici  = exp_rows$d,
          n_ici      = exp_rows$n,
          event_ctrl = ctrl_rows$d,
          n_ctrl     = ctrl_rows$n,
          stringsAsFactors = FALSE
        )
      }

      if (length(data_list) == 0) {
        showNotification("当前结局下没有可用的总体数据", type = "warning")
        return(NULL)
      }

      result <- do.call(rbind, data_list)

      # ---------- 无亚组模式原有的排序：按 OR 从小到大 ----------
      result$OR_true <- (result$event_ici * (result$n_ctrl - result$event_ctrl)) /
        ((result$n_ici - result$event_ici) * result$event_ctrl + 1e-10)
      result <- result[order(result$OR_true, decreasing = FALSE), ]
      result$OR_true <- NULL

      result[, c("study", "event_ici", "n_ici", "event_ctrl", "n_ctrl")]
    }
  })

  # 更新数据可用性提示
  output$data_availability <- renderText({
    avail_studies <- filtered_studies()$study_name
    current <- current_data()
    if (is.null(current)) {
      count <- 0
    } else {
      if ("subgroup_level" %in% names(current)) {
        count <- length(unique(current$study))
      } else {
        count <- sum(avail_studies %in% current$study)
      }
    }
    paste("当前筛选结果中，", count, "项研究有可用的数据（基于所选结局和亚组）。")
  })

  # 荟萃分析（点击按钮后执行）
  meta_result <- eventReactive(input$run, {
    req(selected_studies(), current_data())

    study_df <- selected_studies()
    study_vec <- study_df$study_name

    data <- current_data()

    if ("subgroup_level" %in% names(data)) {
      # 亚组数据
      data <- data[data$study %in% study_vec, ]
      missing_studies <- setdiff(study_vec, unique(data$study))
      if (length(missing_studies) > 0) {
        showNotification(paste("以下研究在当前亚组下没有数据，将被忽略：",
                               paste(missing_studies, collapse = ", ")),
                         type = "warning")
      }
      if (length(unique(data$study)) < 2) {
        showNotification("剩余研究不足两项，无法进行分析", type = "error")
        return(NULL)
      }
      data <- data[order(data$subgroup_level), ]

      m <- metabin(event.e = event_ici, n.e = n_ici,
                   event.c = event_ctrl, n.c = n_ctrl,
                   studlab = study,
                   data = data,
                   sm = "OR",
                   method = "MH",
                   fixed = (input$model == "fixed"),
                   random = (input$model == "random"),
                   byvar = subgroup_level,
                   print.byvar = FALSE,
                   bylab = input$subgroup)

    } else {
      # 无亚组数据
      data <- data[data$study %in% study_vec, ]
      missing_studies <- setdiff(study_vec, data$study)
      if (length(missing_studies) > 0) {
        showNotification(paste("以下研究在当前结局下没有数据，将被忽略：",
                               paste(missing_studies, collapse = ", ")),
                         type = "warning")
      }
      if (nrow(data) < 2) {
        showNotification("剩余研究不足两项，无法进行分析", type = "error")
        return(NULL)
      }
      # 按粗略 OR 排序
      data$OR_true <- (data$event_ici * (data$n_ctrl - data$event_ctrl)) /
        ((data$n_ici - data$event_ici) * data$event_ctrl + 1e-10)
      data <- data[order(data$OR_true, decreasing = FALSE), ]
      data$OR_true <- NULL

      m <- metabin(event.e = event_ici, n.e = n_ici,
                   event.c = event_ctrl, n.c = n_ctrl,
                   studlab = study,
                   data = data,
                   sm = "OR",
                   method = "MH",
                   fixed = (input$model == "fixed"),
                   random = (input$model == "random"))
    }

    m
  })

  # 森林图
  output$forest_plot <- renderPlot({
    m <- meta_result()
    req(m)

    # 根据用户选择的模型确定权重列及其标签
    weight_col <- if (input$model == "fixed") "w.fixed" else "w.random"
    weight_lab <- if (input$model == "fixed") "Weight\n(fixed)" else "Weight\n(random)"


    #左侧显示的列（添加 z 和 pval）
    leftcols <- c("studlab", "event.e", "n.e", "event.c", "n.c", weight_col)
    leftlabs <- c("Study", "ICI\nEvents", " \nTotal", "Control\nEvents", "Control\nTotal",  weight_lab)

    # 当前荟萃分析是亚组分析，即数据按某个分类变量（如不同的 PD-L1 表达水平、病理类型等）分层，森林图需要按亚组分别显示合并结果。
    if (!is.null(m$byvar)) {
      forest(m,
             #leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", weight_col),
             #leftlabs = c("Study", "ICI\nEvents", " \nTotal", "Control\nEvents", "Control\nTotal", weight_lab),
             leftcols = leftcols,
             leftlabs = leftlabs,
             rightcols = c("effect.ci"),
             rightlabs = "Odds Ratio",
             smlab = "",
             pooled.totals = TRUE,
             pooled.events = TRUE,
             comb.fixed = (input$model == "fixed"),
             comb.random = (input$model == "random"),


             test.overall = TRUE,
             test.subgroup = TRUE,
             mar = c(8, 4, 4, 2) + 0.1,  # 适当增加边距
             hetstat = TRUE,
             overall.hetstat = TRUE,      # 显示整体异质性
             subgroup.hetstat = TRUE,
             print.byvar = TRUE,          # 打印亚组标签

             fontsize = 9,
             squaresize = 0.6,
             col.square = "navy",
             col.diamond = "maroon",
             col.diamond.lines = "maroon",
             bysort = TRUE,
             print.subgroup.name = TRUE)
    } else {
      forest(m,
             #leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c", weight_col),
             #leftlabs = c("Study", "ICI\nEvents", " \nTotal", "Control\nEvents", "Control\nTotal", weight_lab),
             leftcols = leftcols,
             leftlabs = leftlabs,
             rightcols = c("effect.ci"),
             rightlabs = "Odds Ratio",
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
             col.diamond.lines = "maroon")
    }
  })

  # 汇总结果
  output$summary <- renderPrint({
    m <- meta_result()
    req(m)

    if (!is.null(m$byvar)) {
      cat("亚组分析结果\n")
      cat("==================================\n")
      print(summary(m))
    } else {
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
    }
  })
}

# ---------- 6. 运行应用 ----------
shinyApp(ui = ui, server = server)
