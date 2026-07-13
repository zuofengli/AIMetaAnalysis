# ==================== app.R ====================
# 功能：NSCLC 围手术期免疫治疗荟萃分析
#       漏斗图支持 meta / metafor / metaviz，增加 X/Y 轴范围控制
#       修复 metafor ylim 报错问题
#       新增：亚组为 Overall 时可选择 phase 或 regimen_type 进行分组分析
#       新增：快照捕获与报告素材导出功能（非侵入式扩展）

# prompt补充
# 在 R + Shiny + dplyr 中：
# 始终使用 dplyr::filter()（避免被 stats::filter 覆盖），避免在非 dplyr 上下文使用 .data$，优先使用裸列名（col 而不是 .data$col）。

library(shiny)
library(shinyjs)
library(DT)
library(meta)
library(grid)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(metafor)
if (!requireNamespace("metaviz", quietly = TRUE)) {
  warning("metaviz 包未安装，将无法使用 metaviz 绘图选项。")
} else {
  library(metaviz)
}

# ---------- 引入快照模块 ----------
source("snapshot_module.R")
source("synthetic_module.R")

.current_snapshot_to_save <- NULL

# ---------- 数据读取 ----------
get_data_file <- function() {
  pkg_file <- system.file("extdata", "clinical_studies_data.xlsx", package = "AIMetaAnalysis")
  if (pkg_file != "") return(pkg_file)
  rel_file <- file.path("extdata", "clinical_studies_data.xlsx")
  if (file.exists(rel_file)) return(rel_file)
  if (file.exists("clinical_studies_data.xlsx")) return("clinical_studies_data.xlsx")
  stop("无法找到 clinical_studies_data.xlsx 文件。")
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

if (!"year" %in% names(binary_data)) binary_data$year <- NA_integer_ else binary_data$year <- as.numeric(binary_data$year)
if (!"year" %in% names(survival_data)) survival_data$year <- NA_integer_ else survival_data$year <- as.numeric(survival_data$year)

if (!"stratified" %in% names(survival_data)) survival_data$stratified <- NA_character_

studies_info <- studies_info %>% mutate(study_id = paste(study_name, year, sep = "_"), study_display = paste0(study_name, " (", year, ")"))
binary_data <- binary_data %>% mutate(study_id = paste(study, year, sep = "_"), study_display = paste0(study, " (", year, ")"))
survival_data <- survival_data %>% mutate(study_id = paste(study, year, sep = "_"), study_display = paste0(study, " (", year, ")"))

# 移除标记为剔除的研究（使用 filter 中的 .data 安全引用）
if ("removed" %in% names(binary_data)) {
  binary_data <- binary_data %>% dplyr::filter(is.na(.data$removed) | .data$removed != "YES")
}
if ("removed" %in% names(survival_data)) {
  survival_data <- survival_data %>% dplyr::filter(is.na(.data$removed) | .data$removed != "YES")
}

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
    fluidRow(column(2, actionButton("select_all", "全选", icon = icon("check-square"))), column(2, actionButton("clear_selection", "清除", icon = icon("times")))),
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
      condition = "input.subgroup == 'Overall'",
      fluidRow(
        column(4, selectInput("study_group_var", "临床研究分组变量", choices = c("不分组" = "none", "试验分期 (phase)" = "phase", "治疗方案类型 (regimen_type)" = "regimen_type"), selected = "none"))
      )
    ),
    conditionalPanel(
      condition = "(input.subgroup != '' && input.subgroup != 'Overall') || (input.subgroup == 'Overall' && input.study_group_var != 'none')",
      wellPanel(
        h5("选择亚组水平并调整顺序"),
        fluidRow(
          column(4, h6("可用水平"), selectInput("left", NULL, choices = NULL, size = 8, selectize = FALSE)),
          column(1, br(), actionButton("add", "加入 ->", width = "100%"), br(), br(), actionButton("remove", "<- 移除", width = "100%"), br(), br(), actionButton("up", "上升", width = "100%"), br(), br(), actionButton("down", "下降", width = "100%")),
          column(4, h6("已选水平（显示顺序）"), selectInput("right", NULL, choices = NULL, size = 8, selectize = FALSE)),
          column(2, br(), actionButton("reset_all", "重置为全部", icon = icon("undo"), width = "100%"), br(), br(), conditionalPanel(condition = "input.subgroup != '' && input.subgroup != 'Overall'", syntheticUI("synth")))
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
    fluidRow(column(3, radioButtons("model", "效应模型", choices = c("固定效应" = "fixed", "随机效应" = "random"), selected = "fixed")), column(3, br(), actionButton("run", "运行荟萃分析", icon = icon("play"), class = "btn-primary"))),
    br(),
    h4("4. 森林图"),
    plotOutput("forest_plot", height = "600px"),
    br(),
    h4("5. 漏斗图 (Funnel Plot)"),
    fluidRow(
      column(2, radioButtons("funnel_pkg", "绘图包", choices = c("meta", "metafor", "metaviz"), selected = "meta", inline = TRUE)),
      column(2, checkboxInput("show_studylab", "显示研究标签", value = TRUE)),
      column(2, conditionalPanel(condition = "input.funnel_pkg == 'meta'", checkboxInput("funnel_contour", "等高线增强", value = FALSE))),
      column(2, conditionalPanel(condition = "input.funnel_pkg == 'meta' && input.funnel_contour == true", textInput("contour_levels", "等高线水平 (逗号分隔)", value = "0.9,0.95,0.99")))
    ),
    fluidRow(
      column(2, numericInput("xlim_min", "X轴最小值", value = NA)),
      column(2, numericInput("xlim_max", "X轴最大值", value = NA)),
      column(2, numericInput("ylim_min", "Y轴最小值", value = NA)),
      column(2, numericInput("ylim_max", "Y轴最大值", value = NA))
    ),
    fluidRow(
      column(3, conditionalPanel(condition = "input.funnel_pkg == 'meta'", selectInput("metabias_method", "发表偏倚检验方法", choices = c("Egger (线性回归)" = "linreg", "Begg (秩相关)" = "rank", "Thompson (加权回归)" = "mm"), selected = "linreg"))),
      column(3, conditionalPanel(condition = "input.funnel_pkg == 'metaviz'", selectInput("funnel_viz_type", "图形类型", choices = c("标准漏斗图" = "standard", "等高线增强" = "contour", "Egger's 回归线" = "egger"), selected = "standard"))),
      column(3, downloadButton("download_funnel", "下载漏斗图 (PNG)"))
    ),
    plotOutput("funnel_plot", height = "500px"),
    br(),
    h5("发表偏倚检验 (Publication Bias Test)"),
    verbatimTextOutput("funnel_test"),
    br(),
    h4("6. 合并结果"),
    verbatimTextOutput("summary"),
    br(),
    snapshotUI("snapshot_module")
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

  observeEvent(c(input$outcome, input$data_type), {
    req(input$outcome)
    if (input$data_type == "binary") {
      subgroups <- binary_data %>% dplyr::filter(.data$outcome == input$outcome) %>% pull(subgroup) %>% unique() %>% na.omit()
    } else {
      subgroups <- survival_data %>% dplyr::filter(.data$outcome == input$outcome) %>% pull(subgroup) %>% unique() %>% na.omit()
    }
    subgroup_choices <- c("无亚组" = "", subgroups)
    default_selected <- if ("Overall" %in% subgroups) "Overall" else ""
    updateSelectInput(session, "subgroup", choices = subgroup_choices, selected = default_selected)
  })

  output$subgroup_ui <- renderUI({ selectInput("subgroup", "亚组变量", choices = NULL) })

  selected_order <- reactiveVal(character(0))

  all_levels <- reactive({
    req(input$outcome)
    if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
      if (input$data_type == "binary") {
        levels <- binary_data %>%
          dplyr::filter(.data$outcome == input$outcome, .data$subgroup == input$subgroup, !is.na(.data$subgroup_option)) %>%
          pull(subgroup_option) %>% unique()
      } else {
        levels <- survival_data %>%
          dplyr::filter(.data$outcome == input$outcome, .data$subgroup == input$subgroup, !is.na(.data$subgroup_option)) %>%
          pull(subgroup_option) %>% unique()
      }
      return(levels)
    }
    if (!is.null(input$subgroup) && input$subgroup == "Overall" && !is.null(input$study_group_var) && input$study_group_var != "none") {
      if (input$study_group_var == "phase") {
        levels <- studies_info$phase %>% unique() %>% na.omit()
      } else if (input$study_group_var == "regimen_type") {
        levels <- studies_info$regimen_type %>% unique() %>% na.omit()
      } else {
        levels <- character(0)
      }
      return(sort(levels))
    }
    character(0)
  })

  observeEvent(all_levels(), {
    levs <- all_levels()
    if (length(levs) > 0) {
      selected_order(levs)
      updateSelectInput(session, "left", choices = setdiff(levs, selected_order()))
      updateSelectInput(session, "right", choices = selected_order(), selected = character(0))
    } else {
      selected_order(character(0))
    }
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

  synthetic_data <- reactiveVal(NULL)

  select_study_record_priority <- function(df, stratified_filter) {
    if (stratified_filter) {
      candidates <- df %>% dplyr::filter(.data$stratified == "Yes")
      if (nrow(candidates) == 0) candidates <- df
    } else {
      candidates <- df %>% dplyr::filter(is.na(.data$stratified) | .data$stratified != "Yes")
      if (nrow(candidates) == 0) candidates <- df
    }
    candidates %>% slice(1)
  }

  current_data_base <- reactive({
    req(input$outcome)
    if (input$data_type == "binary") {
      filtered <- binary_data %>% dplyr::filter(.data$outcome == input$outcome)
      if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
        filtered <- filtered %>% dplyr::filter(tolower(.data$subgroup) == tolower(input$subgroup))
        level_data <- filtered %>% dplyr::filter(!is.na(.data$subgroup_option)) %>% mutate(level = .data$subgroup_option)
        if (nrow(level_data) == 0) return(NULL)
        level_data <- level_data %>% mutate(arm = if_else(grepl("_ctrl$", .data$treatment), "ctrl", "ici"))
        study_levels <- level_data %>%
          group_by(.data$study_id, .data$level, .data$arm) %>%
          summarise(d = sum(.data$d, na.rm = TRUE), n = sum(.data$n, na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(id_cols = c(study_id, level), names_from = arm, values_from = c(d, n)) %>%
          dplyr::filter(!is.na(.data$d_ici) & !is.na(.data$n_ici) & !is.na(.data$d_ctrl) & !is.na(.data$n_ctrl))
        if (nrow(study_levels) == 0) return(NULL)
        study_levels <- study_levels %>%
          left_join(level_data %>% select(study_id, study_display) %>% distinct(study_id, .keep_all = TRUE), by = "study_id") %>%
          mutate(OR_approx = (d_ici * (n_ctrl - d_ctrl)) / ((n_ici - d_ici) * d_ctrl + 1e-10)) %>%
          arrange(.data$level, .data$OR_approx) %>%
          select(-.data$OR_approx)
        result <- study_levels %>%
          rename(event_ici = d_ici, n_ici = n_ici, event_ctrl = d_ctrl, n_ctrl = n_ctrl, subgroup_level = level) %>%
          select(study_id, study_display, event_ici, n_ici, event_ctrl, n_ctrl, subgroup_level)
        return(result)
      } else {
        filtered_overall <- filtered %>% dplyr::filter(.data$subgroup == "Overall")
        if (nrow(filtered_overall) == 0) return(NULL)
        unique_studies <- unique(filtered_overall$study_id)
        data_list <- list()
        for (s in unique_studies) {
          study_rows <- filtered_overall[filtered_overall$study_id == s, ]
          ctrl_idx <- grepl("_ctrl$", study_rows$treatment)
          exp_idx <- !ctrl_idx
          if (sum(ctrl_idx) != 1 || sum(exp_idx) != 1) next
          ctrl_rows <- study_rows[ctrl_idx, ]; exp_rows <- study_rows[exp_idx, ]
          data_list[[s]] <- data.frame(study_id = s, study_display = study_rows$study_display[1],
                                       event_ici = exp_rows$d, n_ici = exp_rows$n,
                                       event_ctrl = ctrl_rows$d, n_ctrl = ctrl_rows$n,
                                       stringsAsFactors = FALSE)
        }
        if (length(data_list) == 0) return(NULL)
        result <- do.call(rbind, data_list)
        result$OR_true <- (result$event_ici * (result$n_ctrl - result$event_ctrl)) / ((result$n_ici - result$event_ici) * result$event_ctrl + 1e-10)
        result <- result[order(result$OR_true, decreasing = FALSE), ]; result$OR_true <- NULL

        if (!is.null(input$study_group_var) && input$study_group_var != "none") {
          group_info <- studies_info %>% select(study_id, phase, regimen_type)
          result <- result %>% left_join(group_info, by = "study_id")
          if (input$study_group_var == "phase") {
            result$subgroup_level <- result$phase
          } else if (input$study_group_var == "regimen_type") {
            result$subgroup_level <- result$regimen_type
          }
          result <- result %>% dplyr::filter(!is.na(.data$subgroup_level))
        }
        return(result[, c("study_id", "study_display", "event_ici", "n_ici", "event_ctrl", "n_ctrl", if("subgroup_level" %in% names(result)) "subgroup_level" else NULL)])
      }
    } else {
      filtered <- survival_data %>% dplyr::filter(.data$outcome == input$outcome) %>% dplyr::filter(!is.na(.data$te) & !is.na(.data$se_te))
      if (nrow(filtered) == 0) return(NULL)
      if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
        if (!("subgroup" %in% names(filtered)) || all(is.na(filtered$subgroup))) return(NULL)
        filtered <- filtered %>% filter(tolower(.data$subgroup) == tolower(input$subgroup))
        if (!("subgroup_option" %in% names(filtered)) || all(is.na(filtered$subgroup_option))) return(NULL)
        grouped <- filtered %>% dplyr::filter(!is.na(.data$subgroup_option)) %>% group_by(.data$study_id, .data$subgroup_option)
        selected_rows <- grouped %>% group_modify(~ select_study_record_priority(.x, input$stratified_filter)) %>% ungroup()
        if (nrow(selected_rows) == 0) return(NULL)
        result <- selected_rows %>% rename(subgroup_level = subgroup_option) %>% select(study_id, study_display, te, se_te, n1, n2, subgroup_level)
        return(result)
      } else {
        if ("subgroup" %in% names(filtered) && any(filtered$subgroup == "Overall", na.rm = TRUE)) {
          filtered_overall <- filtered %>% dplyr::filter(.data$subgroup == "Overall")
        } else { filtered_overall <- filtered }
        grouped <- filtered_overall %>% group_by(.data$study_id)
        selected_rows <- grouped %>% group_modify(~ select_study_record_priority(.x, input$stratified_filter)) %>% ungroup()
        if (nrow(selected_rows) == 0) return(NULL)
        result <- selected_rows %>% select(study_id, study_display, te, se_te, n1, n2)

        if (!is.null(input$study_group_var) && input$study_group_var != "none") {
          group_info <- studies_info %>% select(study_id, phase, regimen_type)
          result <- result %>% left_join(group_info, by = "study_id")
          if (input$study_group_var == "phase") {
            result$subgroup_level <- result$phase
          } else if (input$study_group_var == "regimen_type") {
            result$subgroup_level <- result$regimen_type
          }
          result <- result %>% dplyr::filter(!is.na(.data$subgroup_level))
        }
        return(result)
      }
    }
  })

  current_data <- reactive({
    base <- current_data_base()
    if (is.null(base)) return(NULL)

    has_subgroup <- "subgroup_level" %in% names(base)

    if (input$data_type == "survival" && !is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
      synth_df <- synthetic_data()
      if (!is.null(synth_df) && nrow(synth_df) > 0) {
        base <- base %>% mutate(is_synthetic = FALSE)
        synth_df <- synth_df %>% mutate(is_synthetic = TRUE)
        base <- bind_rows(base, synth_df)
        base <- base %>% group_by(.data$study_id, .data$subgroup_level) %>% arrange(.data$is_synthetic) %>% slice(n()) %>% ungroup()
      } else {
        base <- base %>% mutate(is_synthetic = FALSE)
      }
    } else {
      base <- base %>% mutate(is_synthetic = FALSE)
    }

    if (has_subgroup) {
      order_vec <- selected_order()
      if (length(order_vec) == 0) {
        showNotification("请至少选择一个分组水平", type = "warning")
        return(NULL)
      }
      base <- base %>% dplyr::filter(.data$subgroup_level %in% order_vec)
      base$subgroup_level <- factor(base$subgroup_level, levels = order_vec)
      base <- base %>% arrange(.data$subgroup_level)
    }
    base
  })

  synth_module <- syntheticServer(
    id = "synth",
    available_levels = reactive({ all_levels() }),
    data_type = reactive({ input$data_type }),
    base_data = reactive({ current_data_base() }),
    selected_order_reactive = selected_order,
    synthetic_data_reactive = synthetic_data
  )

  output$synthetic_levels_table <- renderDT({
    req(synthetic_data())
    df <- synthetic_data()
    if (nrow(df) == 0) return(NULL)
    synth_levels <- unique(df$subgroup_level)
    display_df <- data.frame(
      合成水平 = synth_levels,
      研究数 = sapply(synth_levels, function(lvl) nrow(df %>% dplyr::filter(.data$subgroup_level == lvl))),
      stringsAsFactors = FALSE
    )
    datatable(display_df, selection = 'single', options = list(dom = 't', pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = '_all'))), rownames = FALSE)
  })

  observeEvent(input$delete_synth_level, {
    req(input$synthetic_levels_table_rows_selected)
    selected_row <- input$synthetic_levels_table_rows_selected
    synth_df <- synthetic_data()
    req(synth_df)
    levels_unique <- unique(synth_df$subgroup_level)
    level_to_remove <- levels_unique[selected_row]
    new_synth <- synth_df %>% dplyr::filter(.data$subgroup_level != level_to_remove)
    if (nrow(new_synth) == 0) new_synth <- NULL
    synthetic_data(new_synth)
    current_order <- selected_order()
    new_order <- current_order[current_order != level_to_remove]
    selected_order(new_order)
    updateSelectInput(session, "right", choices = new_order)
    showNotification(paste0("已删除合成水平：", level_to_remove), type = "warning")
  })

  observeEvent(c(input$outcome, input$subgroup, input$data_type, input$study_group_var), { synthetic_data(NULL) })

  output$data_availability <- renderText({
    current <- current_data()
    if (is.null(current)) count <- 0 else count <- length(unique(current$study_id))
    paste("当前筛选结果中，", count, "项研究有可用的数据。")
  })

  meta_result <- eventReactive(input$run, {
    req(selected_studies(), current_data())
    study_vec <- selected_studies()
    data <- current_data()

    has_subgroup <- "subgroup_level" %in% names(data)

    if (has_subgroup) {
      data <- data[data$study_id %in% study_vec, ]
      if (length(unique(data$study_id)) < 2) {
        showNotification("剩余研究不足两项，无法进行分析", type = "error")
        return(NULL)
      }
      data <- data %>% dplyr::filter(!is.na(.data$subgroup_level))
      if (nrow(data) == 0) {
        showNotification("亚组水平数据缺失，无法分析", type = "error")
        return(NULL)
      }
      data <- data %>% mutate(studlab_modified = ifelse(.data$is_synthetic, paste0(.data$study_display, "*"), .data$study_display))

      subgroup_name_final <- if (!is.null(input$subgroup) && input$subgroup != "Overall") {
        input$subgroup
      } else if (!is.null(input$study_group_var) && input$study_group_var != "none") {
        input$study_group_var
      } else NULL

      if (input$data_type == "binary") {
        m <- metabin(event.e = event_ici, n.e = n_ici,
                     event.c = event_ctrl, n.c = n_ctrl,
                     studlab = studlab_modified,
                     data = data, sm = "OR", method = "MH",
                     common = (input$model == "fixed"), random = (input$model == "random"),
                     subgroup = subgroup_level, print.subgroup.name = FALSE, subgroup.name = subgroup_name_final)
      } else {
        m <- metagen(TE = te, seTE = se_te, studlab = studlab_modified,
                     data = data, sm = "HR", method.tau = "DL",
                     common = (input$model == "fixed"), random = (input$model == "random"),
                     subgroup = subgroup_level, print.subgroup.name = FALSE, subgroup.name = subgroup_name_final)
      }
    } else {
      data <- data[data$study_id %in% study_vec, ]
      if (nrow(data) < 2) {
        showNotification("剩余研究不足两项，无法进行分析", type = "error")
        return(NULL)
      }
      if ("is_synthetic" %in% names(data)) {
        data <- data %>% mutate(studlab_modified = ifelse(.data$is_synthetic, paste0(.data$study_display, "*"), .data$study_display))
      } else {
        data$studlab_modified <- data$study_display
      }

      if (input$data_type == "binary") {
        data$OR_true <- (data$event_ici * (data$n_ctrl - data$event_ctrl)) / ((data$n_ici - data$event_ici) * data$event_ctrl + 1e-10)
        data <- data[order(data$OR_true, decreasing = FALSE), ]; data$OR_true <- NULL
        m <- metabin(event.e = event_ici, n.e = n_ici, event.c = event_ctrl, n.c = n_ctrl,
                     studlab = studlab_modified, data = data, sm = "OR", method = "MH",
                     common = (input$model == "fixed"), random = (input$model == "random"))
      } else {
        data <- data[order(data$te), ]
        m <- metagen(TE = te, seTE = se_te, studlab = studlab_modified,
                     data = data, sm = "HR", method.tau = "DL",
                     common = (input$model == "fixed"), random = (input$model == "random"))
      }
    }
    m
  })

  capture_trigger <- reactiveVal(FALSE)

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
      forest(m, leftcols = leftcols, leftlabs = leftlabs, rightcols = c("effect.ci"), rightlabs = rightlabs,
             smlab = "", pooled.totals = (input$data_type == "binary"), pooled.events = (input$data_type == "binary"),
             comb.common = (input$model == "fixed"), comb.random = (input$model == "random"),
             test.overall = TRUE, test.subgroup = TRUE, mar = c(8, 4, 4, 2) + 0.1, hetstat = TRUE,
             overall.hetstat = TRUE, subgroup.hetstat = TRUE, print.subgroup.name = TRUE, fontsize = 9,
             squaresize = 0.6, col.square = "navy", col.diamond = "maroon", col.diamond.lines = "maroon", sort.subgroup = FALSE)
    } else {
      forest(m, leftcols = leftcols, leftlabs = leftlabs, rightcols = c("effect.ci"), rightlabs = rightlabs,
             smlab = "", pooled.totals = (input$data_type == "binary"), pooled.events = (input$data_type == "binary"),
             comb.common = (input$model == "fixed"), comb.random = (input$model == "random"),
             hetstat = TRUE, test.overall = TRUE, fontsize = 9, squaresize = 0.6,
             col.square = "navy", col.diamond = "maroon", col.diamond.lines = "maroon")
    }
  })

  snapshots <- snapshotServer(
    id = "snapshot_module",
    meta_result = meta_result,
    data_type = reactive(input$data_type),
    outcome = reactive(input$outcome),
    subgroup_name = reactive({ if (is.null(input$subgroup) || input$subgroup == "") "Overall" else input$subgroup }),
    model = reactive(input$model)
  )

  # ---------- 漏斗图等相关函数保持不变 ----------
  parse_lim <- function(min_val, max_val, non_negative = FALSE) {
    min_val <- suppressWarnings(as.numeric(min_val))
    max_val <- suppressWarnings(as.numeric(max_val))
    if (is.na(min_val) || is.na(max_val) || min_val >= max_val) return(NULL)
    if (non_negative && min_val < 0) return(NULL)
    c(min_val, max_val)
  }

  funnel_plot_obj <- reactive({
    m <- meta_result(); req(m)
    pkg <- input$funnel_pkg; show_lab <- input$show_studylab
    ylab_label <- if (input$data_type == "binary") "Standard Error of log(Odds Ratio)" else "Standard Error of log(Hazard Ratio)"
    xlim <- parse_lim(input$xlim_min, input$xlim_max, non_negative = FALSE)
    ylim <- parse_lim(input$ylim_min, input$ylim_max, non_negative = TRUE)

    if (pkg == "meta") {
      contour <- input$funnel_contour
      levels <- if (contour) {
        levs <- tryCatch(as.numeric(strsplit(input$contour_levels, ",")[[1]]), error = function(e) c(0.9, 0.95, 0.99))
        if (any(is.na(levs))) levs <- c(0.9, 0.95, 0.99)
        levs
      } else NULL
      list(type = "meta", m = m, contour = contour, levels = levels, show_lab = show_lab, xlim = xlim, ylim = ylim, ylab = ylab_label)
    } else if (pkg == "metafor") {
      yi <- m$TE; vi <- m$seTE^2
      res <- rma(yi = yi, vi = vi, method = "DL", test = "z")
      res$slab <- m$studlab
      if (show_lab && length(res$slab) != res$k) { showNotification("标签数量与研究数不匹配，已自动禁用标签显示。", type = "warning"); show_lab <- FALSE }
      list(type = "metafor", res = res, show_lab = show_lab, xlim = xlim, ylim = ylim, ylab = ylab_label)
    } else if (pkg == "metaviz") {
      plot_data <- data.frame(yi = m$TE, vi = m$seTE^2, study = m$studlab)
      if (show_lab) showNotification("metaviz 包不支持直接显示研究标签，请使用 meta 或 metafor。", type = "warning")
      list(type = "metaviz", data = plot_data, viz_type = input$funnel_viz_type, show_lab = FALSE, xlim = xlim, ylim = ylim, ylab = ylab_label)
    }
  })

  output$funnel_plot <- renderPlot({
    obj <- funnel_plot_obj(); req(obj)
    if (obj$type == "meta") {
      if (obj$contour) {
        funnel(obj$m, contour = TRUE, contour.levels = obj$levels, studlab = obj$show_lab, xlim = obj$xlim, ylim = obj$ylim, ylab = obj$ylab)
      } else {
        funnel(obj$m, studlab = obj$show_lab, xlim = obj$xlim, ylim = obj$ylim, ylab = obj$ylab)
      }
    } else if (obj$type == "metafor") {
      label_arg <- if (obj$show_lab) obj$res$slab else FALSE
      args <- list(x = obj$res, main = "Funnel Plot (metafor)", label = label_arg, ylab = obj$ylab)
      if (!is.null(obj$xlim)) args$xlim <- obj$xlim
      if (!is.null(obj$ylim)) args$ylim <- obj$ylim
      tryCatch({
        do.call(funnel, args)
      }, error = function(e) {
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
      p
    }
  })

  output$funnel_test <- renderPrint({
    m <- meta_result(); req(m)
    if (input$funnel_pkg == "meta") {
      method <- input$metabias_method
      if (m$k < 3) { cat("研究数量少于 3，无法进行发表偏倚检验。"); return() }
      tryCatch({
        test <- metabias(m, method.bias = method, k.min = 3)
        print(test)
        cat("\n注：p < 0.05 提示可能存在发表偏倚。")
      }, error = function(e) { cat("发表偏倚检验失败：", e$message, "\n可能原因：数据不满足检验假设。") },
      warning = function(w) { cat("警告：", w$message, "\n"); test <- suppressWarnings(metabias(m, method.bias = method, k.min = 3)); if (!is.null(test)) { print(test); cat("\n注：p < 0.05 提示可能存在发表偏倚。") } })
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
      obj <- funnel_plot_obj(); req(obj)
      png(file, width = 10, height = 6, units = "in", res = 300)
      if (obj$type == "meta") {
        if (obj$contour) funnel(obj$m, contour = TRUE, contour.levels = obj$levels, studlab = obj$show_lab, xlim = obj$xlim, ylim = obj$ylim)
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
    m <- meta_result(); req(m)
    if (!is.null(m$subgroup)) {
      cat("亚组分析结果\n==================================\n")
      print(summary(m))
    } else {
      if (input$data_type == "binary") {
        if (input$model == "fixed") {
          cat("固定效应模型 (二分类数据, OR)\n")
          cat("合并 OR: ", round(exp(m$TE.common), 2), "\n95% CI: [", round(exp(m$lower.common), 2), ", ", round(exp(m$upper.common), 2), "]\n")
          cat("Z = ", round(m$zval.common, 2), ", p = ", format.pval(m$pval.common, digits = 4), "\n")
        } else {
          cat("随机效应模型 (二分类数据, OR)\n")
          cat("合并 OR: ", round(exp(m$TE.random), 2), "\n95% CI: [", round(exp(m$lower.random), 2), ", ", round(exp(m$upper.random), 2), "]\n")
          cat("Z = ", round(m$zval.random, 2), ", p = ", format.pval(m$pval.random, digits = 4), "\n")
        }
        cat("\n异质性:\nI² = ", round(m$I2, 1), "%\nτ² = ", round(m$tau2, 4), "\nQ = ", round(m$Q, 2), ", df = ", m$k - 1, ", p = ", format.pval(m$pval.Q, digits = 4), "\n")
      } else {
        if (input$model == "fixed") {
          cat("固定效应模型 (生存数据, HR)\n")
          cat("合并 HR: ", round(exp(m$TE.common), 2), "\n95% CI: [", round(exp(m$lower.common), 2), ", ", round(exp(m$upper.common), 2), "]\n")
          cat("Z = ", round(m$zval.common, 2), ", p = ", format.pval(m$pval.common, digits = 4), "\n")
        } else {
          cat("随机效应模型 (生存数据, HR)\n")
          cat("合并 HR: ", round(exp(m$TE.random), 2), "\n95% CI: [", round(exp(m$lower.random), 2), ", ", round(exp(m$upper.random), 2), "]\n")
          cat("Z = ", round(m$zval.random, 2), ", p = ", format.pval(m$pval.random, digits = 4), "\n")
        }
        cat("\n异质性:\nI² = ", round(m$I2, 1), "%\nτ² = ", round(m$tau2, 4), "\nQ = ", round(m$Q, 2), ", df = ", m$k - 1, ", p = ", format.pval(m$pval.Q, digits = 4), "\n")
      }
    }
  })
}

shinyApp(ui = ui, server = server)
