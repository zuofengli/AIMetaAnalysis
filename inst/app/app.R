# ==================== 整合版 Shiny 荟萃分析工具（支持二分类与生存数据） ====================
# 功能：
#   - 从 clinical_studies_data.xlsx 读取研究信息、二分类数据和生存数据
#   - 筛选研究，动态选择数据类型、结局指标和亚组，并选择多项研究进行荟萃分析
#   - 支持亚组分析（当选择具体亚组变量时，自动按所有水平分组）
#   - 通过左右列表自由选择亚组水平并调整顺序
#   - 显示森林图和统计摘要
#   - 生存数据必须提供 te 和 se_te 列，缺失时提示用户并排除该研究
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
survival_data <- read_excel(data_file, sheet = "survival")

# ---------- 统一年份列名 ----------
if ("pub_year" %in% names(studies_info)) {
  studies_info <- studies_info %>% rename(year = pub_year)
}
if ("pub_year" %in% names(binary_data)) {
  binary_data <- binary_data %>% rename(year = pub_year)
}
if ("pub_year" %in% names(survival_data)) {
  survival_data <- survival_data %>% rename(year = pub_year)
}

# ---------- 安全转换数值列（抑制强制转换警告） ----------
binary_numeric_cols <- intersect(c("d", "n", "n?"), names(binary_data))
if (length(binary_numeric_cols) > 0) {
  suppressWarnings({
    binary_data <- binary_data %>% mutate(across(all_of(binary_numeric_cols), as.numeric))
  })
}

survival_numeric_cols <- intersect(c("te", "se_te", "n1", "n2", "z_value", "p_value", "hr", "lower_ci", "upper_ci"), names(survival_data))
if (length(survival_numeric_cols) > 0) {
  suppressWarnings({
    survival_data <- survival_data %>% mutate(across(all_of(survival_numeric_cols), as.numeric))
  })
}

# ---------- 统一研究标识列 ----------
# 研究信息中的研究名称列
if ("trial" %in% names(studies_info)) {
  studies_info$study_name <- studies_info$trial
} else {
  studies_info$study_name <- studies_info$study
}
studies_info$year <- as.integer(studies_info$year)

# 二分类数据：确保有 study 列
if ("trial" %in% names(binary_data)) {
  binary_data$study <- binary_data$trial
} else if (!"study" %in% names(binary_data)) {
  stop("二分类数据中必须包含研究标识列（trial 或 study）")
}

# 生存数据：确保有 study 列
if ("trial" %in% names(survival_data)) {
  survival_data$study <- survival_data$trial
} else if (!"study" %in% names(survival_data)) {
  stop("生存数据中必须包含研究标识列（trial 或 study）")
}

# ---------- 处理 year 列：直接使用自身数据，不做匹配 ----------
# 如果 binary_data 中没有 year 列，则创建一列全为 NA
if (!"year" %in% names(binary_data)) {
  binary_data$year <- NA_integer_
} else {
  binary_data$year <- as.numeric(binary_data$year)
}

# 如果 survival_data 中没有 year 列，则创建一列全为 NA
if (!"year" %in% names(survival_data)) {
  survival_data$year <- NA_integer_
} else {
  survival_data$year <- as.numeric(survival_data$year)
}

# ---------- 生成唯一标识 study_id 和显示名称 ----------
# studies_info
studies_info <- studies_info %>%
  mutate(study_id = paste(study_name, year, sep = "_"),
         study_display = paste0(study_name, " (", year, ")"))

# binary_data
binary_data <- binary_data %>%
  mutate(study_id = paste(study, year, sep = "_"),
         study_display = paste0(study, " (", year, ")"))

# survival_data
survival_data <- survival_data %>%
  mutate(study_id = paste(study, year, sep = "_"),
         study_display = paste0(study, " (", year, ")"))

# ---------- 过滤 removed 列 ----------
if ("removed" %in% names(binary_data)) {
  binary_data <- binary_data %>% filter(is.na(removed) | removed != "YES")
}
if ("removed" %in% names(survival_data)) {
  survival_data <- survival_data %>% filter(is.na(removed) | removed != "YES")
}

# ---------- 生存数据预处理：统一亚组列名 ----------
if ("group" %in% names(survival_data) && "groupOption" %in% names(survival_data)) {
  survival_data <- survival_data %>%
    rename(subgroup = group, subgroup_option = groupOption)
}
if (!"subgroup" %in% names(survival_data)) {
  survival_data$subgroup <- NA
}
if (!"subgroup_option" %in% names(survival_data)) {
  survival_data$subgroup_option <- NA
}

# 再次确保数值列转换（已有警告抑制）
survival_data <- survival_data %>%
  mutate(te = as.numeric(te), se_te = as.numeric(se_te),
         n1 = as.numeric(n1), n2 = as.numeric(n2))

# 检查生存数据是否缺少 te 或 se_te 列
if (!("te" %in% names(survival_data)) || !("se_te" %in% names(survival_data))) {
  stop("生存数据中必须包含 'te' 和 'se_te' 列。请检查数据文件。")
}

# ---------- 2. UI ----------
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

  # ---------- 数据类型与结局指标选择 ----------
  wellPanel(
    h4("2.5 选择数据类型与结局指标"),
    fluidRow(
      column(4,
             radioButtons("data_type", "数据类型",
                          choices = c("二分类数据" = "binary",
                                      "生存数据 (HR)" = "survival"),
                          selected = "binary")
      ),
      column(4, uiOutput("outcome_ui")),
      column(4, uiOutput("subgroup_ui"))
    ),
    # 亚组顺序构建控件（仅当选择具体亚组时显示）
    conditionalPanel(
      condition = "input.subgroup != '' && input.subgroup != 'Overall'",
      wellPanel(
        h5("选择亚组水平并调整顺序"),
        fluidRow(
          column(4,
                 h6("可用水平"),
                 selectInput("left", NULL, choices = NULL, size = 8, selectize = FALSE)
          ),
          column(1,
                 br(),
                 actionButton("add", "加入 ->", width = "100%"),
                 br(), br(),
                 actionButton("remove", "<- 移除", width = "100%"),
                 br(), br(),
                 actionButton("up", "上升", width = "100%"),
                 br(), br(),
                 actionButton("down", "下降", width = "100%")
          ),
          column(4,
                 h6("已选水平（显示顺序）"),
                 selectInput("right", NULL, choices = NULL, size = 8, selectize = FALSE)
          ),
          column(2,
                 br(),
                 actionButton("reset_all", "重置为全部", icon = icon("undo"), width = "100%")
          )
        )
      )
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

# ---------- 3. Server ----------
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

  # 显示研究表格（显示 study_display，内部使用 study_id）
  output$study_table <- renderDT({
    display_df <- filtered_studies()[, c("study_display", "phase", "year")]
    datatable(display_df,
              selection = 'multiple',
              options = list(pageLength = 10, autoWidth = TRUE, dom = 'ftp'),
              rownames = FALSE,
              colnames = c("研究名称", "分期", "年份")
    )
  })

  # 获取用户选中的研究 study_id 列表
  selected_studies <- reactive({
    rows <- input$study_table_rows_selected
    if (is.null(rows)) return(NULL)
    filtered_studies()[rows, "study_id", drop = TRUE]
  })

  # ---------- 动态生成结局指标和亚组的下拉菜单（根据数据类型） ----------
  # 结局指标 UI
  output$outcome_ui <- renderUI({
    if (input$data_type == "binary") {
      outcomes <- unique(binary_data$outcome)
      outcomes <- outcomes[!is.na(outcomes)]
      selectInput("outcome", "结局指标",
                  choices = outcomes,
                  selected = outcomes[1])
    } else {
      outcomes <- unique(survival_data$outcome)
      outcomes <- outcomes[!is.na(outcomes)]
      selectInput("outcome", "结局指标",
                  choices = outcomes,
                  selected = outcomes[1])
    }
  })

  # 当结局指标或数据类型改变时，更新亚组变量选项
  observeEvent(c(input$outcome, input$data_type), {
    req(input$outcome)
    if (input$data_type == "binary") {
      subgroups <- binary_data %>%
        filter(outcome == input$outcome) %>%
        pull(subgroup) %>%
        unique() %>%
        na.omit()
    } else {
      subgroups <- survival_data %>%
        filter(outcome == input$outcome) %>%
        pull(subgroup) %>%
        unique() %>%
        na.omit()
    }
    subgroup_choices <- c("无亚组" = "", subgroups)
    default_selected <- if ("Overall" %in% subgroups) "Overall" else ""
    updateSelectInput(session, "subgroup",
                      choices = subgroup_choices,
                      selected = default_selected)
  })

  output$subgroup_ui <- renderUI({
    selectInput("subgroup", "亚组变量", choices = NULL)
  })

  # ---------- 亚组顺序构建逻辑（左右列表） ----------
  selected_order <- reactiveVal(character(0))

  all_levels <- reactive({
    req(input$outcome, input$subgroup, input$subgroup != "", input$subgroup != "Overall")
    if (input$data_type == "binary") {
      levels <- binary_data %>%
        filter(outcome == input$outcome, subgroup == input$subgroup, !is.na(subgroup_option)) %>%
        pull(subgroup_option) %>%
        unique()
    } else {
      levels <- survival_data %>%
        filter(outcome == input$outcome, subgroup == input$subgroup, !is.na(subgroup_option)) %>%
        pull(subgroup_option) %>%
        unique()
    }
    levels
  })

  observeEvent(c(input$outcome, input$subgroup, input$data_type), {
    req(all_levels())
    levels <- all_levels()
    selected_order(levels)
    updateSelectInput(session, "left", choices = setdiff(levels, selected_order()))
    updateSelectInput(session, "right", choices = selected_order(), selected = character(0))
  })

  observe({
    req(all_levels())
    levels <- all_levels()
    left_choices <- setdiff(levels, selected_order())
    updateSelectInput(session, "left", choices = left_choices)
  })

  observe({
    updateSelectInput(session, "right", choices = selected_order())
  })

  observeEvent(input$add, {
    req(input$left, input$left != "")
    selected <- input$left
    current <- selected_order()
    if (!(selected %in% current)) {
      new_order <- c(current, selected)
      selected_order(new_order)
      updateSelectInput(session, "right", selected = selected)
    }
  })

  observeEvent(input$remove, {
    req(input$right, input$right != "")
    selected <- input$right
    current <- selected_order()
    new_order <- current[current != selected]
    selected_order(new_order)
    updateSelectInput(session, "right", selected = character(0))
  })

  observeEvent(input$up, {
    req(input$right, input$right != "")
    selected <- input$right
    current <- selected_order()
    idx <- which(current == selected)
    if (length(idx) == 1 && idx > 1) {
      new_order <- current
      new_order[idx] <- current[idx - 1]
      new_order[idx - 1] <- selected
      selected_order(new_order)
      updateSelectInput(session, "right", selected = selected)
    }
  })

  observeEvent(input$down, {
    req(input$right, input$right != "")
    selected <- input$right
    current <- selected_order()
    idx <- which(current == selected)
    if (length(idx) == 1 && idx < length(current)) {
      new_order <- current
      new_order[idx] <- current[idx + 1]
      new_order[idx + 1] <- selected
      selected_order(new_order)
      updateSelectInput(session, "right", selected = selected)
    }
  })

  observeEvent(input$reset_all, {
    req(all_levels())
    levels <- all_levels()
    selected_order(levels)
    updateSelectInput(session, "right", selected = character(0))
  })

  # ---------- 根据选择的数据类型、结局和亚组变量提取可用数据 ----------
  current_data <- reactive({
    req(input$outcome)

    if (input$data_type == "binary") {
      # ========== 二分类数据 ==========
      filtered <- binary_data %>%
        filter(outcome == input$outcome)

      # 亚组分析分支
      if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
        filtered <- filtered %>% filter(subgroup == input$subgroup)
        level_data <- filtered %>%
          filter(!is.na(subgroup_option)) %>%
          mutate(level = subgroup_option)

        if (nrow(level_data) == 0) {
          showNotification("当前选择的亚组变量下没有可用水平数据", type = "warning")
          return(NULL)
        }

        level_data <- level_data %>%
          mutate(arm = if_else(grepl("_ctrl$", treatment), "ctrl", "ici"))

        # 按 study_id、level、arm 聚合
        study_levels <- level_data %>%
          group_by(study_id, level, arm) %>%
          summarise(d = sum(d, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(id_cols = c(study_id, level), names_from = arm, values_from = c(d, n)) %>%
          filter(!is.na(d_ici) & !is.na(n_ici) & !is.na(d_ctrl) & !is.na(n_ctrl))

        if (nrow(study_levels) == 0) {
          showNotification("没有研究同时包含试验组和对照组数据", type = "warning")
          return(NULL)
        }

        # 添加显示名称（从原始数据中取第一个 study_display，但 study_display 相同）
        study_levels <- study_levels %>%
          left_join(level_data %>% select(study_id, study_display) %>% distinct(study_id, .keep_all = TRUE), by = "study_id") %>%
          mutate(OR_approx = (d_ici * (n_ctrl - d_ctrl)) /
                   ((n_ici - d_ici) * d_ctrl + 1e-10)) %>%
          arrange(level, OR_approx) %>%
          select(-OR_approx)

        result <- study_levels %>%
          rename(event_ici = d_ici, n_ici = n_ici,
                 event_ctrl = d_ctrl, n_ctrl = n_ctrl, subgroup_level = level) %>%
          select(study_id, study_display, event_ici, n_ici, event_ctrl, n_ctrl, subgroup_level)

        order_vec <- selected_order()
        if (length(order_vec) == 0) {
          showNotification("请至少选择一个亚组水平", type = "warning")
          return(NULL)
        }
        result <- result %>% filter(subgroup_level %in% order_vec)
        if (nrow(result) == 0) {
          showNotification("当前没有研究属于您选择的亚组水平，请调整选择", type = "warning")
          return(NULL)
        }
        result$subgroup_level <- factor(result$subgroup_level, levels = order_vec)
        result <- result[order(result$subgroup_level), ]
        return(result)
      } else {
        # 无亚组模式（Overall）
        filtered_overall <- filtered %>% filter(subgroup == "Overall")
        if (nrow(filtered_overall) == 0) {
          showNotification("没有找到总体（Overall）数据", type = "warning")
          return(NULL)
        }

        unique_studies <- unique(filtered_overall$study_id)
        data_list <- list()
        for (s in unique_studies) {
          study_rows <- filtered_overall[filtered_overall$study_id == s, ]
          ctrl_idx <- grepl("_ctrl$", study_rows$treatment)
          exp_idx  <- !ctrl_idx
          if (sum(ctrl_idx) != 1 || sum(exp_idx) != 1) next
          ctrl_rows <- study_rows[ctrl_idx, ]
          exp_rows  <- study_rows[exp_idx, ]
          data_list[[s]] <- data.frame(
            study_id      = s,
            study_display = study_rows$study_display[1],
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
        result$OR_true <- (result$event_ici * (result$n_ctrl - result$event_ctrl)) /
          ((result$n_ici - result$event_ici) * result$event_ctrl + 1e-10)
        result <- result[order(result$OR_true, decreasing = FALSE), ]
        result$OR_true <- NULL
        return(result[, c("study_id", "study_display", "event_ici", "n_ici", "event_ctrl", "n_ctrl")])
      }
    } else {
      # ========== 生存数据 ==========
      filtered <- survival_data %>%
        filter(outcome == input$outcome) %>%
        filter(!is.na(te) & !is.na(se_te))

      if (nrow(filtered) == 0) {
        showNotification("没有可用的生存数据（所有研究的 te 或 se_te 均缺失）", type = "error")
        return(NULL)
      }

      # 亚组分析分支
      if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
        if (!("subgroup" %in% names(filtered)) || all(is.na(filtered$subgroup))) {
          showNotification("当前数据中没有亚组变量列，无法进行亚组分析", type = "warning")
          return(NULL)
        }
        filtered <- filtered %>% filter(subgroup == input$subgroup)

        if (!("subgroup_option" %in% names(filtered)) || all(is.na(filtered$subgroup_option))) {
          showNotification("当前数据中没有亚组水平列（subgroup_option），无法进行亚组分析", type = "warning")
          return(NULL)
        }

        level_data <- filtered %>%
          filter(!is.na(subgroup_option)) %>%
          mutate(level = subgroup_option)

        if (nrow(level_data) == 0) {
          showNotification("当前选择的亚组变量下没有可用水平数据", type = "warning")
          return(NULL)
        }

        # 按 study_id、level 汇总
        study_levels <- level_data %>%
          group_by(study_id, level) %>%
          summarise(te = mean(te, na.rm = TRUE),
                    se_te = mean(se_te, na.rm = TRUE),
                    n1 = sum(n1, na.rm = TRUE),
                    n2 = sum(n2, na.rm = TRUE),
                    study_display = first(study_display),
                    .groups = "drop") %>%
          filter(!is.na(te) & !is.na(se_te))

        if (nrow(study_levels) == 0) {
          showNotification("没有可用的生存数据（所有研究的 te 或 se_te 均缺失）", type = "error")
          return(NULL)
        }

        result <- study_levels %>%
          rename(subgroup_level = level) %>%
          select(study_id, study_display, te, se_te, n1, n2, subgroup_level)

        order_vec <- selected_order()
        if (length(order_vec) == 0) {
          showNotification("请至少选择一个亚组水平", type = "warning")
          return(NULL)
        }
        result <- result %>% filter(subgroup_level %in% order_vec)
        if (nrow(result) == 0) {
          showNotification("当前没有研究属于您选择的亚组水平，请调整选择", type = "warning")
          return(NULL)
        }
        result$subgroup_level <- factor(result$subgroup_level, levels = order_vec)
        result <- result[order(result$subgroup_level), ]
        return(result)
      } else {
        # 无亚组模式（Overall）
        if ("subgroup" %in% names(filtered) && any(filtered$subgroup == "Overall", na.rm = TRUE)) {
          filtered_overall <- filtered %>% filter(subgroup == "Overall")
        } else {
          filtered_overall <- filtered
        }

        unique_studies <- unique(filtered_overall$study_id)
        data_list <- list()
        for (s in unique_studies) {
          study_rows <- filtered_overall[filtered_overall$study_id == s, ]
          if (nrow(study_rows) > 1) {
            study_rows <- study_rows %>%
              summarise(te = mean(te, na.rm = TRUE),
                        se_te = mean(se_te, na.rm = TRUE),
                        n1 = sum(n1, na.rm = TRUE),
                        n2 = sum(n2, na.rm = TRUE),
                        study_display = first(study_display),
                        .groups = "drop")
          }
          if (is.na(study_rows$te) || is.na(study_rows$se_te)) next
          data_list[[s]] <- data.frame(
            study_id = s,
            study_display = study_rows$study_display,
            te = study_rows$te,
            se_te = study_rows$se_te,
            n1 = study_rows$n1,
            n2 = study_rows$n2,
            stringsAsFactors = FALSE
          )
        }
        if (length(data_list) == 0) {
          showNotification("当前结局下没有可用的总体数据（所有研究的 te 或 se_te 均缺失）", type = "error")
          return(NULL)
        }
        result <- do.call(rbind, data_list)
        result <- result[order(result$te), ]
        return(result[, c("study_id", "study_display", "te", "se_te", "n1", "n2")])
      }
    }
  })

  # 更新数据可用性提示
  output$data_availability <- renderText({
    avail_studies <- filtered_studies()$study_id
    current <- current_data()
    if (is.null(current)) {
      count <- 0
    } else {
      count <- length(unique(current$study_id))
    }
    paste("当前筛选结果中，", count, "项研究有可用的数据（基于所选数据类型、结局和亚组）。")
  })

  # 荟萃分析（点击按钮后执行）
  meta_result <- eventReactive(input$run, {
    req(selected_studies(), current_data())

    study_vec <- selected_studies()
    data <- current_data()

    if ("subgroup_level" %in% names(data)) {
      # 亚组分析
      data <- data[data$study_id %in% study_vec, ]
      missing_studies <- setdiff(study_vec, unique(data$study_id))
      if (length(missing_studies) > 0) {
        showNotification(paste("以下研究在当前亚组下没有数据，将被忽略：",
                               paste(missing_studies, collapse = ", ")),
                         type = "warning")
      }
      if (length(unique(data$study_id)) < 2) {
        showNotification("剩余研究不足两项，无法进行分析", type = "error")
        return(NULL)
      }

      # 按用户指定的亚组顺序排序（已经因子化）
      data <- data[order(data$subgroup_level), ]

      if (input$data_type == "binary") {
        m <- metabin(event.e = event_ici, n.e = n_ici,
                     event.c = event_ctrl, n.c = n_ctrl,
                     studlab = study_display,   # 使用显示名称
                     data = data,
                     sm = "OR",
                     method = "MH",
                     common = (input$model == "fixed"),
                     random = (input$model == "random"),
                     subgroup = subgroup_level,
                     print.subgroup.name = FALSE,
                     subgroup.name = input$subgroup)
      } else {
        m <- metagen(TE = te, seTE = se_te,
                     studlab = study_display,   # 使用显示名称
                     data = data,
                     sm = "HR",
                     method.tau = "DL",
                     common = (input$model == "fixed"),
                     random = (input$model == "random"),
                     subgroup = subgroup_level,
                     print.subgroup.name = FALSE,
                     subgroup.name = input$subgroup)
      }
    } else {
      # 无亚组分析
      data <- data[data$study_id %in% study_vec, ]
      missing_studies <- setdiff(study_vec, data$study_id)
      if (length(missing_studies) > 0) {
        showNotification(paste("以下研究在当前结局下没有数据，将被忽略：",
                               paste(missing_studies, collapse = ", ")),
                         type = "warning")
      }
      if (nrow(data) < 2) {
        showNotification("剩余研究不足两项，无法进行分析", type = "error")
        return(NULL)
      }

      if (input$data_type == "binary") {
        # 按粗略 OR 排序
        data$OR_true <- (data$event_ici * (data$n_ctrl - data$event_ctrl)) /
          ((data$n_ici - data$event_ici) * data$event_ctrl + 1e-10)
        data <- data[order(data$OR_true, decreasing = FALSE), ]
        data$OR_true <- NULL
        m <- metabin(event.e = event_ici, n.e = n_ici,
                     event.c = event_ctrl, n.c = n_ctrl,
                     studlab = study_display,
                     data = data,
                     sm = "OR",
                     method = "MH",
                     common = (input$model == "fixed"),
                     random = (input$model == "random"))
      } else {
        # 按 te 从小到大排序
        data <- data[order(data$te), ]
        m <- metagen(TE = te, seTE = se_te,
                     studlab = study_display,
                     data = data,
                     sm = "HR",
                     method.tau = "DL",
                     common = (input$model == "fixed"),
                     random = (input$model == "random"))
      }
    }
    m
  })

  # 森林图
  output$forest_plot <- renderPlot({
    m <- meta_result()
    req(m)

    if (input$data_type == "binary") {
      weight_col <- if (input$model == "fixed") "w.common" else "w.random"
      weight_lab <- if (input$model == "fixed") "Weight\n(common)" else "Weight\n(random)"
      leftcols <- c("studlab", "event.e", "n.e", "event.c", "n.c", weight_col)
      leftlabs <- c("Study", "ICI\nEvents", " \nTotal", "Control\nEvents", "Control\nTotal", weight_lab)
      effect_lab <- "Odds Ratio"
      rightlabs <- "Odds Ratio\n95% CI"
    } else {
      weight_col <- if (input$model == "fixed") "w.common" else "w.random"
      weight_lab <- "Weight"
      leftcols <- c("studlab", "TE", "seTE", weight_col)
      leftlabs <- c("Study", "log[Hazard Ratio]", "SE", weight_lab)
      rightcols <- c("effect.ci")
      rightlabs <- "Hazard Ratio\nIV, Common, 95% CI"
      effect_lab <- "Hazard Ratio"
    }

    if (!is.null(m$subgroup)) {
      forest(m,
             leftcols = leftcols,
             leftlabs = leftlabs,
             rightcols = c("effect.ci"),
             rightlabs = rightlabs,
             smlab = "",
             pooled.totals = (input$data_type == "binary"),
             pooled.events = (input$data_type == "binary"),
             comb.common = (input$model == "fixed"),
             comb.random = (input$model == "random"),
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
      forest(m,
             leftcols = leftcols,
             leftlabs = leftlabs,
             rightcols = c("effect.ci"),
             rightlabs = rightlabs,
             smlab = "",
             pooled.totals = (input$data_type == "binary"),
             pooled.events = (input$data_type == "binary"),
             comb.common = (input$model == "fixed"),
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

    if (!is.null(m$subgroup)) {
      cat("亚组分析结果\n")
      cat("==================================\n")
      print(summary(m))
    } else {
      if (input$data_type == "binary") {
        if (input$model == "fixed") {
          cat("固定效应模型 (二分类数据, OR)\n")
          cat("合并 OR: ", round(exp(m$TE.common), 2), "\n")
          cat("95% CI: [", round(exp(m$lower.common), 2), ", ", round(exp(m$upper.common), 2), "]\n")
          cat("Z = ", round(m$zval.common, 2), ", p = ", format.pval(m$pval.common, digits = 4), "\n")
        } else {
          cat("随机效应模型 (二分类数据, OR)\n")
          cat("合并 OR: ", round(exp(m$TE.random), 2), "\n")
          cat("95% CI: [", round(exp(m$lower.random), 2), ", ", round(exp(m$upper.random), 2), "]\n")
          cat("Z = ", round(m$zval.random, 2), ", p = ", format.pval(m$pval.random, digits = 4), "\n")
        }
        cat("\n异质性:\n")
        cat("I² = ", round(m$I2, 1), "%\n")
        cat("τ² = ", round(m$tau2, 4), "\n")
        cat("Q = ", round(m$Q, 2), ", df = ", m$k - 1, ", p = ", format.pval(m$pval.Q, digits = 4), "\n")
      } else {
        if (input$model == "fixed") {
          cat("固定效应模型 (生存数据, HR)\n")
          cat("合并 HR: ", round(exp(m$TE.common), 2), "\n")
          cat("95% CI: [", round(exp(m$lower.common), 2), ", ", round(exp(m$upper.common), 2), "]\n")
          cat("Z = ", round(m$zval.common, 2), ", p = ", format.pval(m$pval.common, digits = 4), "\n")
        } else {
          cat("随机效应模型 (生存数据, HR)\n")
          cat("合并 HR: ", round(exp(m$TE.random), 2), "\n")
          cat("95% CI: [", round(exp(m$lower.random), 2), ", ", round(exp(m$upper.random), 2), "]\n")
          cat("Z = ", round(m$zval.random, 2), ", p = ", format.pval(m$pval.random, digits = 4), "\n")
        }
        cat("\n异质性:\n")
        cat("I² = ", round(m$I2, 1), "%\n")
        cat("τ² = ", round(m$tau2, 4), "\n")
        cat("Q = ", round(m$Q, 2), ", df = ", m$k - 1, ", p = ", format.pval(m$pval.Q, digits = 4), "\n")
      }
    }
  })
}

# ---------- 4. 运行应用 ----------
shinyApp(ui = ui, server = server)
