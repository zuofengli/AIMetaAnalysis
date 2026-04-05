# ==================== app.R ====================
# 功能：NSCLC 围手术期免疫治疗荟萃分析
#       支持二分类与生存数据，支持亚组水平合成（仅生存数据）
#       分析仅包含用户右侧选择的亚组水平
#       合成数据可覆盖现有水平，并在森林图中以星号标记

library(shiny)
library(shinyjs)
library(DT)
library(meta)
library(grid)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# 加载合成模块
source("synthetic_module.R")

# ---------- 数据读取 ----------
get_data_file <- function() {
  pkg_file <- system.file("extdata", "clinical_studies_data.xlsx", package = "AIMetaAnalysis")
  if (pkg_file != "") return(pkg_file)
  rel_file <- file.path("extdata", "clinical_studies_data.xlsx")
  if (file.exists(rel_file)) return(rel_file)
  if (file.exists("clinical_studies_data.xlsx")) return("clinical_studies_data.xlsx")
  stop("无法找到 clinical_studies_data.xlsx 文件。请确保文件位于 extdata/ 目录下或当前工作目录中。")
}

data_file <- get_data_file()

studies_info <- read_excel(data_file, sheet = "studies_info")
binary_data  <- read_excel(data_file, sheet = "binary_data")
survival_data <- read_excel(data_file, sheet = "survival")

# 统一年份列名
if ("pub_year" %in% names(studies_info)) studies_info <- studies_info %>% rename(year = pub_year)
if ("pub_year" %in% names(binary_data)) binary_data <- binary_data %>% rename(year = pub_year)
if ("pub_year" %in% names(survival_data)) survival_data <- survival_data %>% rename(year = pub_year)

# 数值转换
binary_numeric_cols <- intersect(c("d", "n", "n?"), names(binary_data))
if (length(binary_numeric_cols) > 0) {
  suppressWarnings(binary_data <- binary_data %>% mutate(across(all_of(binary_numeric_cols), as.numeric)))
}
survival_numeric_cols <- intersect(c("te", "se_te", "n1", "n2", "z_value", "p_value", "hr", "lower_ci", "upper_ci"), names(survival_data))
if (length(survival_numeric_cols) > 0) {
  suppressWarnings(survival_data <- survival_data %>% mutate(across(all_of(survival_numeric_cols), as.numeric)))
}

# 统一研究标识列
if ("trial" %in% names(studies_info)) studies_info$study_name <- studies_info$trial else studies_info$study_name <- studies_info$study
studies_info$year <- as.integer(studies_info$year)

if ("trial" %in% names(binary_data)) binary_data$study <- binary_data$trial else if (!"study" %in% names(binary_data)) stop("二分类数据中必须包含研究标识列（trial 或 study）")
if ("trial" %in% names(survival_data)) survival_data$study <- survival_data$trial else if (!"study" %in% names(survival_data)) stop("生存数据中必须包含研究标识列（trial 或 study）")

# 处理 year 列
if (!"year" %in% names(binary_data)) binary_data$year <- NA_integer_ else binary_data$year <- as.numeric(binary_data$year)
if (!"year" %in% names(survival_data)) survival_data$year <- NA_integer_ else survival_data$year <- as.numeric(survival_data$year)

# 确保 stratified 列存在
if (!"stratified" %in% names(survival_data)) survival_data$stratified <- NA_character_

# 生成唯一标识
studies_info <- studies_info %>% mutate(study_id = paste(study_name, year, sep = "_"), study_display = paste0(study_name, " (", year, ")"))
binary_data <- binary_data %>% mutate(study_id = paste(study, year, sep = "_"), study_display = paste0(study, " (", year, ")"))
survival_data <- survival_data %>% mutate(study_id = paste(study, year, sep = "_"), study_display = paste0(study, " (", year, ")"))

# 过滤 removed 列
if ("removed" %in% names(binary_data)) binary_data <- binary_data %>% filter(is.na(removed) | removed != "YES")
if ("removed" %in% names(survival_data)) survival_data <- survival_data %>% filter(is.na(removed) | removed != "YES")

# 生存数据亚组列名统一
if ("group" %in% names(survival_data) && "groupOption" %in% names(survival_data)) {
  survival_data <- survival_data %>% rename(subgroup = group, subgroup_option = groupOption)
}
if (!"subgroup" %in% names(survival_data)) survival_data$subgroup <- NA
if (!"subgroup_option" %in% names(survival_data)) survival_data$subgroup_option <- NA
survival_data <- survival_data %>% mutate(subgroup = str_trim(subgroup), subgroup_option = str_trim(subgroup_option))
survival_data <- survival_data %>% mutate(te = as.numeric(te), se_te = as.numeric(se_te), n1 = as.numeric(n1), n2 = as.numeric(n2))

if (!("te" %in% names(survival_data)) || !("se_te" %in% names(survival_data))) stop("生存数据中必须包含 'te' 和 'se_te' 列。")

# ---------- UI ----------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("NSCLC 围手术期免疫治疗荟萃分析"),

  wellPanel(
    h4("1. 筛选临床研究"),
    fluidRow(
      column(4, selectInput("phase_filter", "试验分期", choices = c("全部", unique(studies_info$phase)), selected = "全部")),
      column(4, sliderInput("year_filter", "发表年份范围", min = min(studies_info$year, na.rm = TRUE), max = max(studies_info$year, na.rm = TRUE), value = range(studies_info$year, na.rm = TRUE), step = 1, sep = ""))
    ),
    br(),
    h4("2. 选择研究（点击行可多选）"),
    fluidRow(column(2, actionButton("select_all", "全选", icon = icon("check-square"))),
             column(2, actionButton("clear_selection", "清除", icon = icon("times")))),
    DTOutput("study_table"),
    br(),
    textOutput("data_availability")
  ),

  wellPanel(
    h4("2.5 选择数据类型与结局指标"),
    fluidRow(
      column(4, radioButtons("data_type", "数据类型", choices = c("二分类数据" = "binary", "生存数据 (HR)" = "survival"), selected = "binary")),
      column(4, uiOutput("outcome_ui")),
      column(4, uiOutput("subgroup_ui")),
      column(4, conditionalPanel(condition = "input.data_type == 'survival'", checkboxInput("stratified_filter", "优先使用分层分析数据 (stratified = Yes)", value = TRUE)))
    ),
    conditionalPanel(
      condition = "input.subgroup != '' && input.subgroup != 'Overall'",
      wellPanel(
        h5("选择亚组水平并调整顺序"),
        fluidRow(
          column(4, h6("可用水平"), selectInput("left", NULL, choices = NULL, size = 8, selectize = FALSE)),
          column(1, br(), actionButton("add", "加入 ->", width = "100%"), br(), br(), actionButton("remove", "<- 移除", width = "100%"), br(), br(), actionButton("up", "上升", width = "100%"), br(), br(), actionButton("down", "下降", width = "100%")),
          column(4, h6("已选水平（显示顺序）"), selectInput("right", NULL, choices = NULL, size = 8, selectize = FALSE)),
          column(2, br(), actionButton("reset_all", "重置为全部", icon = icon("undo"), width = "100%"), br(), br(), syntheticUI("synth"))
        ),
        hr(),
        h5("已合成的亚组水平"),
        DTOutput("synthetic_levels_table"),
        actionButton("delete_synth_level", "删除选中的合成水平", icon = icon("trash"), class = "btn-danger")
      )
    )
  ),

  wellPanel(
    h4("3. 分析设置"),
    fluidRow(column(3, radioButtons("model", "效应模型", choices = c("固定效应" = "fixed", "随机效应" = "random"), selected = "fixed")),
             column(3, br(), actionButton("run", "运行荟萃分析", icon = icon("play"), class = "btn-primary"))),
    br(),
    h4("4. 森林图"),
    plotOutput("forest_plot", height = "600px"),
    br(),
    h4("5. 合并结果"),
    verbatimTextOutput("summary")
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  proxy <- dataTableProxy('study_table')
  observeEvent(input$select_all, { req(filtered_studies()); selectRows(proxy, 1:nrow(filtered_studies())) })
  observeEvent(input$clear_selection, { selectRows(proxy, NULL) })

  filtered_studies <- reactive({
    df <- studies_info
    if (input$phase_filter != "全部") df <- df[df$phase == input$phase_filter, ]
    df <- df[df$year >= input$year_filter[1] & df$year <= input$year_filter[2], ]
    df
  })

  output$study_table <- renderDT({
    display_df <- filtered_studies()[, c("study_display", "phase", "year")]
    datatable(display_df, selection = 'multiple', options = list(pageLength = 10, autoWidth = TRUE, dom = 'ftp'), rownames = FALSE, colnames = c("研究名称", "分期", "年份"))
  })

  selected_studies <- reactive({
    rows <- input$study_table_rows_selected
    if (is.null(rows)) return(NULL)
    filtered_studies()[rows, "study_id", drop = TRUE]
  })

  # 结局指标 UI
  output$outcome_ui <- renderUI({
    if (input$data_type == "binary") {
      outcomes <- unique(binary_data$outcome)
      outcomes <- outcomes[!is.na(outcomes)]
      selectInput("outcome", "结局指标", choices = outcomes, selected = outcomes[1])
    } else {
      outcomes <- unique(survival_data$outcome)
      outcomes <- outcomes[!is.na(outcomes)]
      selectInput("outcome", "结局指标", choices = outcomes, selected = outcomes[1])
    }
  })

  # 亚组变量更新
  observeEvent(c(input$outcome, input$data_type), {
    req(input$outcome)
    if (input$data_type == "binary") {
      subgroups <- binary_data %>% filter(outcome == input$outcome) %>% pull(subgroup) %>% unique() %>% na.omit()
    } else {
      subgroups <- survival_data %>% filter(outcome == input$outcome) %>% pull(subgroup) %>% unique() %>% na.omit()
    }
    subgroup_choices <- c("无亚组" = "", subgroups)
    default_selected <- if ("Overall" %in% subgroups) "Overall" else ""
    updateSelectInput(session, "subgroup", choices = subgroup_choices, selected = default_selected)
  })

  output$subgroup_ui <- renderUI({ selectInput("subgroup", "亚组变量", choices = NULL) })

  # 亚组顺序
  selected_order <- reactiveVal(character(0))
  all_levels <- reactive({
    req(input$outcome, input$subgroup, input$subgroup != "", input$subgroup != "Overall")
    if (input$data_type == "binary") {
      levels <- binary_data %>% filter(outcome == input$outcome, subgroup == input$subgroup, !is.na(subgroup_option)) %>% pull(subgroup_option) %>% unique()
    } else {
      levels <- survival_data %>% filter(outcome == input$outcome, subgroup == input$subgroup, !is.na(subgroup_option)) %>% pull(subgroup_option) %>% unique()
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

  observe({ updateSelectInput(session, "left", choices = setdiff(all_levels(), selected_order())) })
  observe({ updateSelectInput(session, "right", choices = selected_order()) })

  observeEvent(input$add, { req(input$left); current <- selected_order(); new_order <- c(current, input$left); selected_order(new_order); updateSelectInput(session, "right", selected = input$left) })
  observeEvent(input$remove, { req(input$right); current <- selected_order(); new_order <- current[current != input$right]; selected_order(new_order); updateSelectInput(session, "right", selected = character(0)) })
  observeEvent(input$up, {
    req(input$right); current <- selected_order(); idx <- which(current == input$right)
    if (length(idx) == 1 && idx > 1) { new_order <- current; new_order[idx] <- current[idx-1]; new_order[idx-1] <- input$right; selected_order(new_order); updateSelectInput(session, "right", selected = input$right) }
  })
  observeEvent(input$down, {
    req(input$right); current <- selected_order(); idx <- which(current == input$right)
    if (length(idx) == 1 && idx < length(current)) { new_order <- current; new_order[idx] <- current[idx+1]; new_order[idx+1] <- input$right; selected_order(new_order); updateSelectInput(session, "right", selected = input$right) }
  })
  observeEvent(input$reset_all, { req(all_levels()); selected_order(all_levels()); updateSelectInput(session, "right", selected = character(0)) })

  # 存储合成数据
  synthetic_data <- reactiveVal(NULL)

  # 优先选择 stratified 记录的函数
  select_study_record_priority <- function(df, stratified_filter) {
    if (stratified_filter) {
      candidates <- df %>% filter(stratified == "Yes")
      if (nrow(candidates) == 0) candidates <- df
    } else {
      candidates <- df %>% filter(is.na(stratified) | stratified != "Yes")
      if (nrow(candidates) == 0) candidates <- df
    }
    candidates %>% slice(1)
  }

  # 当前数据（基础数据，不含合成）
  current_data_base <- reactive({
    req(input$outcome)
    if (input$data_type == "binary") {
      filtered <- binary_data %>% filter(outcome == input$outcome)
      if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
        filtered <- filtered %>% filter(tolower(subgroup) == tolower(input$subgroup))
        level_data <- filtered %>% filter(!is.na(subgroup_option)) %>% mutate(level = subgroup_option)
        if (nrow(level_data) == 0) return(NULL)
        level_data <- level_data %>% mutate(arm = if_else(grepl("_ctrl$", treatment), "ctrl", "ici"))
        study_levels <- level_data %>% group_by(study_id, level, arm) %>% summarise(d = sum(d, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop") %>% pivot_wider(id_cols = c(study_id, level), names_from = arm, values_from = c(d, n)) %>% filter(!is.na(d_ici) & !is.na(n_ici) & !is.na(d_ctrl) & !is.na(n_ctrl))
        if (nrow(study_levels) == 0) return(NULL)
        study_levels <- study_levels %>% left_join(level_data %>% select(study_id, study_display) %>% distinct(study_id, .keep_all = TRUE), by = "study_id") %>% mutate(OR_approx = (d_ici * (n_ctrl - d_ctrl)) / ((n_ici - d_ici) * d_ctrl + 1e-10)) %>% arrange(level, OR_approx) %>% select(-OR_approx)
        result <- study_levels %>% rename(event_ici = d_ici, n_ici = n_ici, event_ctrl = d_ctrl, n_ctrl = n_ctrl, subgroup_level = level) %>% select(study_id, study_display, event_ici, n_ici, event_ctrl, n_ctrl, subgroup_level)
        return(result)
      } else {
        filtered_overall <- filtered %>% filter(subgroup == "Overall")
        if (nrow(filtered_overall) == 0) return(NULL)
        unique_studies <- unique(filtered_overall$study_id)
        data_list <- list()
        for (s in unique_studies) {
          study_rows <- filtered_overall[filtered_overall$study_id == s, ]
          ctrl_idx <- grepl("_ctrl$", study_rows$treatment)
          exp_idx <- !ctrl_idx
          if (sum(ctrl_idx) != 1 || sum(exp_idx) != 1) next
          ctrl_rows <- study_rows[ctrl_idx, ]; exp_rows <- study_rows[exp_idx, ]
          data_list[[s]] <- data.frame(study_id = s, study_display = study_rows$study_display[1], event_ici = exp_rows$d, n_ici = exp_rows$n, event_ctrl = ctrl_rows$d, n_ctrl = ctrl_rows$n, stringsAsFactors = FALSE)
        }
        if (length(data_list) == 0) return(NULL)
        result <- do.call(rbind, data_list)
        result$OR_true <- (result$event_ici * (result$n_ctrl - result$event_ctrl)) / ((result$n_ici - result$event_ici) * result$event_ctrl + 1e-10)
        result <- result[order(result$OR_true, decreasing = FALSE), ]; result$OR_true <- NULL
        return(result[, c("study_id", "study_display", "event_ici", "n_ici", "event_ctrl", "n_ctrl")])
      }
    } else { # survival
      filtered <- survival_data %>% filter(outcome == input$outcome) %>% filter(!is.na(te) & !is.na(se_te))
      if (nrow(filtered) == 0) return(NULL)
      if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
        if (!("subgroup" %in% names(filtered)) || all(is.na(filtered$subgroup))) return(NULL)
        filtered <- filtered %>% filter(tolower(subgroup) == tolower(input$subgroup))
        if (!("subgroup_option" %in% names(filtered)) || all(is.na(filtered$subgroup_option))) return(NULL)
        grouped <- filtered %>% filter(!is.na(subgroup_option)) %>% group_by(study_id, subgroup_option)
        selected_rows <- grouped %>% group_modify(~ select_study_record_priority(.x, input$stratified_filter)) %>% ungroup()
        if (nrow(selected_rows) == 0) return(NULL)
        result <- selected_rows %>% rename(subgroup_level = subgroup_option) %>% select(study_id, study_display, te, se_te, n1, n2, subgroup_level)
        return(result)
      } else {
        if ("subgroup" %in% names(filtered) && any(filtered$subgroup == "Overall", na.rm = TRUE)) {
          filtered_overall <- filtered %>% filter(subgroup == "Overall")
        } else { filtered_overall <- filtered }
        grouped <- filtered_overall %>% group_by(study_id)
        selected_rows <- grouped %>% group_modify(~ select_study_record_priority(.x, input$stratified_filter)) %>% ungroup()
        if (nrow(selected_rows) == 0) return(NULL)
        result <- selected_rows %>% select(study_id, study_display, te, se_te, n1, n2)
        result <- result[order(result$te), ]
        return(result)
      }
    }
  })

  # 最终数据（包含合成数据，并仅保留用户选择的水平，添加 is_synthetic 列）
  current_data <- reactive({
    base <- current_data_base()
    if (is.null(base)) return(NULL)

    # 亚组分析模式：合并合成数据并过滤用户选择的水平
    if (input$data_type == "survival" && !is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
      synth_df <- synthetic_data()
      if (!is.null(synth_df) && nrow(synth_df) > 0) {
        base <- base %>% mutate(is_synthetic = FALSE)
        synth_df <- synth_df %>% mutate(is_synthetic = TRUE)
        base <- bind_rows(base, synth_df)
        # 去重：对于同一研究、同一亚组水平，如果既有原始又有合成，保留合成数据
        base <- base %>%
          group_by(study_id, subgroup_level) %>%
          arrange(is_synthetic) %>%  # FALSE 在前，TRUE 在后，slice(n()) 取最后一行（合成）
          slice(n()) %>%
          ungroup()
      } else {
        base <- base %>% mutate(is_synthetic = FALSE)
      }
      # 仅保留用户在“已选水平”中选择的水平
      order_vec <- selected_order()
      if (length(order_vec) > 0) {
        base <- base %>% filter(subgroup_level %in% order_vec)
      } else {
        base <- base %>% filter(FALSE)
      }
      # 因子化并按顺序排序
      base$subgroup_level <- factor(base$subgroup_level, levels = order_vec)
      base <- base[order(base$subgroup_level), ]
    } else if (input$data_type == "survival") {
      # 无亚组模式，但为了一致性，也添加 is_synthetic 列（始终 FALSE）
      base <- base %>% mutate(is_synthetic = FALSE)
    } else {
      # 二分类数据暂时不支持合成，添加 is_synthetic 列（始终 FALSE）
      base <- base %>% mutate(is_synthetic = FALSE)
    }

    # 移除 subgroup_level 为 NA 的行（避免 meta 报错）
    if (!is.null(base) && "subgroup_level" %in% names(base)) {
      base <- base %>% filter(!is.na(subgroup_level))
    }
    base
  })

  # 合成模块调用
  synth_module <- syntheticServer(
    id = "synth",
    available_levels = reactive({ all_levels() }),
    data_type = reactive({ input$data_type }),
    base_data = reactive({ current_data_base() }),  # 传入未含合成数据的基础数据，避免递归
    selected_order_reactive = selected_order,
    synthetic_data_reactive = synthetic_data
  )

  # 显示已合成的亚组水平表格
  output$synthetic_levels_table <- renderDT({
    req(synthetic_data())
    df <- synthetic_data()
    if (nrow(df) == 0) return(NULL)
    synth_levels <- unique(df$subgroup_level)
    display_df <- data.frame(
      合成水平 = synth_levels,
      研究数 = sapply(synth_levels, function(lvl) nrow(df %>% filter(subgroup_level == lvl))),
      stringsAsFactors = FALSE
    )
    datatable(display_df,
              selection = 'single',
              options = list(dom = 't', pageLength = 10,
                             columnDefs = list(list(className = 'dt-center', targets = '_all'))),
              rownames = FALSE)
  })

  # 删除选中的合成水平
  observeEvent(input$delete_synth_level, {
    req(input$synthetic_levels_table_rows_selected)
    selected_row <- input$synthetic_levels_table_rows_selected
    synth_df <- synthetic_data()
    req(synth_df)
    levels_unique <- unique(synth_df$subgroup_level)
    level_to_remove <- levels_unique[selected_row]

    # 从 synthetic_data 中删除该水平
    new_synth <- synth_df %>% filter(subgroup_level != level_to_remove)
    if (nrow(new_synth) == 0) new_synth <- NULL
    synthetic_data(new_synth)

    # 从 selected_order 中删除该水平
    current_order <- selected_order()
    new_order <- current_order[current_order != level_to_remove]
    selected_order(new_order)

    # 同步更新右侧列表显示
    updateSelectInput(session, "right", choices = new_order)

    showNotification(paste0("已删除合成水平：", level_to_remove), type = "warning")
  })

  # 当结局或亚组改变时，清除合成数据
  observeEvent(c(input$outcome, input$subgroup, input$data_type), {
    synthetic_data(NULL)
  })

  output$data_availability <- renderText({
    current <- current_data()
    if (is.null(current)) count <- 0 else count <- length(unique(current$study_id))
    paste("当前筛选结果中，", count, "项研究有可用的数据（基于所选数据类型、结局和亚组）。")
  })

  meta_result <- eventReactive(input$run, {
    req(selected_studies(), current_data())
    study_vec <- selected_studies()
    data <- current_data()

    if ("subgroup_level" %in% names(data)) {
      data <- data[data$study_id %in% study_vec, ]
      if (length(unique(data$study_id)) < 2) {
        showNotification("剩余研究不足两项，无法进行分析", type = "error")
        return(NULL)
      }
      # 确保 subgroup_level 为因子，且 levels 与 selected_order 一致
      order_vec <- selected_order()
      if (length(order_vec) == 0) {
        showNotification("请至少选择一个亚组水平", type = "warning")
        return(NULL)
      }
      data$subgroup_level <- factor(data$subgroup_level, levels = order_vec)
      # 删除 subgroup_level 为 NA 的行
      data <- data %>% filter(!is.na(subgroup_level))
      if (nrow(data) == 0) {
        showNotification("亚组水平数据缺失，无法分析", type = "error")
        return(NULL)
      }
      data <- data[order(data$subgroup_level), ]

      # 为合成数据的研究标签添加星号
      data <- data %>%
        mutate(studlab_modified = ifelse(is_synthetic,
                                         paste0(study_display, "*"),
                                         study_display))

      if (input$data_type == "binary") {
        m <- metabin(event.e = event_ici, n.e = n_ici,
                     event.c = event_ctrl, n.c = n_ctrl,
                     studlab = studlab_modified,
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
                     studlab = studlab_modified,
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
      # 无亚组模式
      data <- data[data$study_id %in% study_vec, ]
      if (nrow(data) < 2) {
        showNotification("剩余研究不足两项，无法进行分析", type = "error")
        return(NULL)
      }
      # 为合成数据的研究标签添加星号（注意：无亚组时，合成数据可能不存在，但保险起见）
      if ("is_synthetic" %in% names(data)) {
        data <- data %>%
          mutate(studlab_modified = ifelse(is_synthetic,
                                           paste0(study_display, "*"),
                                           study_display))
      } else {
        data$studlab_modified <- data$study_display
      }

      if (input$data_type == "binary") {
        data$OR_true <- (data$event_ici * (data$n_ctrl - data$event_ctrl)) /
          ((data$n_ici - data$event_ici) * data$event_ctrl + 1e-10)
        data <- data[order(data$OR_true, decreasing = FALSE), ]; data$OR_true <- NULL
        m <- metabin(event.e = event_ici, n.e = n_ici,
                     event.c = event_ctrl, n.c = n_ctrl,
                     studlab = studlab_modified,
                     data = data,
                     sm = "OR",
                     method = "MH",
                     common = (input$model == "fixed"),
                     random = (input$model == "random"))
      } else {
        data <- data[order(data$te), ]
        m <- metagen(TE = te, seTE = se_te,
                     studlab = studlab_modified,
                     data = data,
                     sm = "HR",
                     method.tau = "DL",
                     common = (input$model == "fixed"),
                     random = (input$model == "random"))
      }
    }
    m
  })

  output$forest_plot <- renderPlot({
    m <- meta_result(); req(m)
    if (input$data_type == "binary") {
      weight_col <- if (input$model == "fixed") "w.common" else "w.random"
      weight_lab <- if (input$model == "fixed") "Weight\n(common)" else "Weight\n(random)"
      leftcols <- c("studlab", "event.e", "n.e", "event.c", "n.c", weight_col)
      leftlabs <- c("Study", "ICI\nEvents", " \nTotal", "Control\nEvents", "Control\nTotal", weight_lab)
      rightlabs <- "Odds Ratio\n95% CI"
    } else {
      weight_col <- if (input$model == "fixed") "w.common" else "w.random"
      weight_lab <- "Weight"
      leftcols <- c("studlab", "TE", "seTE", weight_col)
      leftlabs <- c("Study", "log[Hazard Ratio]", "SE", weight_lab)
      rightcols <- c("effect.ci")
      rightlabs <- "Hazard Ratio\nIV, Common, 95% CI"
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

  output$summary <- renderPrint({
    m <- meta_result(); req(m)
    if (!is.null(m$subgroup)) {
      cat("亚组分析结果\n==================================\n")
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

shinyApp(ui = ui, server = server)
