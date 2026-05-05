# modules/data_selection_module.R
# 数据选择与准备模块：数据类型、结局、亚组、分组变量、亚组水平顺序、合成数据

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)

source("modules/synthetic_module.R")   # 确保合成模块在同一个作用域

dataSelectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("2.5 选择数据类型与结局指标"),
    fluidRow(
      column(4, radioButtons(ns("data_type"), "数据类型",
                             choices = c("二分类数据" = "binary", "生存数据 (HR)" = "survival"),
                             selected = "binary")),
      column(4, uiOutput(ns("outcome_ui"))),
      column(4, uiOutput(ns("subgroup_ui"))),
      column(4, conditionalPanel(
        condition = paste0("input['", ns("data_type"), "'] == 'survival'"),
        checkboxInput(ns("stratified_filter"), "优先使用分层分析数据 (stratified = Yes)", value = TRUE)
      ))
    ),
    conditionalPanel(
      condition = paste0("input['", ns("subgroup"), "'] == 'Overall'"),
      fluidRow(
        column(4, selectInput(ns("study_group_var"), "临床研究分组变量",
                              choices = c("不分组" = "none",
                                          "试验分期 (phase)" = "phase",
                                          "治疗方案类型 (regimen_type)" = "regimen_type"),
                              selected = "none"))
      )
    ),
    conditionalPanel(
      condition = paste0("(input['", ns("subgroup"), "'] != '' && input['", ns("subgroup"), "'] != 'Overall') || (input['", ns("subgroup"), "'] == 'Overall' && input['", ns("study_group_var"), "'] != 'none')"),
      wellPanel(
        h5("选择亚组水平并调整顺序"),
        fluidRow(
          column(4, h6("可用水平"), selectInput(ns("left"), NULL, choices = NULL, size = 8, selectize = FALSE)),
          column(1, br(), actionButton(ns("add"), "加入 ->", width = "100%"),
                 br(), br(), actionButton(ns("remove"), "<- 移除", width = "100%"),
                 br(), br(), actionButton(ns("up"), "上升", width = "100%"),
                 br(), br(), actionButton(ns("down"), "下降", width = "100%")),
          column(4, h6("已选水平（显示顺序）"), selectInput(ns("right"), NULL, choices = NULL, size = 8, selectize = FALSE)),
          column(2, br(), actionButton(ns("reset_all"), "重置为全部", icon = icon("undo"), width = "100%"),
                 br(), br(), conditionalPanel(
                   condition = paste0("input['", ns("subgroup"), "'] != '' && input['", ns("subgroup"), "'] != 'Overall'"),
                   syntheticUI(ns("synth"))
                 ))
        ),
        hr(),
        h5("已合成的亚组水平"),
        DTOutput(ns("synthetic_levels_table")),
        actionButton(ns("delete_synth_level"), "删除选中的合成水平", icon = icon("trash"), class = "btn-danger")
      )
    )
  )
}

dataSelectionServer <- function(id, studies_info, binary_data, survival_data, selected_study_ids) {
  moduleServer(id, function(input, output, session) {

    # ---------- 内部辅助函数：研究记录优先级选择 ----------
    select_study_record_priority <- function(df, stratified_filter) {
      if (stratified_filter) {
        candidates <- df %>% dplyr::filter(stratified == "Yes")
        if (nrow(candidates) == 0) candidates <- df
      } else {
        candidates <- df %>% dplyr::filter(is.na(stratified) | stratified != "Yes")
        if (nrow(candidates) == 0) candidates <- df
      }
      candidates %>% slice(1)
    }

    # ====== 结局指标 UI ======
    output$outcome_ui <- renderUI({
      if (input$data_type == "binary") {
        outcomes <- unique(binary_data$outcome)
        outcomes <- outcomes[!is.na(outcomes)]
        selectInput(session$ns("outcome"), "结局指标", choices = outcomes, selected = outcomes[1])
      } else {
        outcomes <- unique(survival_data$outcome)
        outcomes <- outcomes[!is.na(outcomes)]
        selectInput(session$ns("outcome"), "结局指标", choices = outcomes, selected = outcomes[1])
      }
    })

    # ====== 亚组变量 UI ======
    observeEvent(c(input$outcome, input$data_type), {
      req(input$outcome)
      if (input$data_type == "binary") {
        subgroups <- binary_data %>% dplyr::filter(outcome == input$outcome) %>% pull(subgroup) %>% unique() %>% na.omit()
      } else {
        subgroups <- survival_data %>% dplyr::filter(outcome == input$outcome) %>% pull(subgroup) %>% unique() %>% na.omit()
      }
      subgroup_choices <- c("无亚组" = "", subgroups)
      default_selected <- if ("Overall" %in% subgroups) "Overall" else ""
      updateSelectInput(session, "subgroup", choices = subgroup_choices, selected = default_selected)
    })

    output$subgroup_ui <- renderUI({
      selectInput(session$ns("subgroup"), "亚组变量", choices = NULL)
    })

    # ====== 亚组水平管理 ======
    selected_order <- reactiveVal(character(0))
    synthetic_data <- reactiveVal(NULL)

    # 所有可选水平
    all_levels <- reactive({
      req(input$outcome)
      if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
        if (input$data_type == "binary") {
          levels <- binary_data %>%
            dplyr::filter(outcome == input$outcome, subgroup == input$subgroup, !is.na(subgroup_option)) %>%
            pull(subgroup_option) %>% unique()
        } else {
          levels <- survival_data %>%
            dplyr::filter(outcome == input$outcome, subgroup == input$subgroup, !is.na(subgroup_option)) %>%
            pull(subgroup_option) %>% unique()
        }
        return(levels)
      }
      if (!is.null(input$subgroup) && input$subgroup == "Overall" &&
          !is.null(input$study_group_var) && input$study_group_var != "none") {
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

    # 合成数据子模块
    synth_module <- syntheticServer(
      id = "synth",
      available_levels = reactive(all_levels()),
      data_type = reactive(input$data_type),
      base_data = reactive(current_data_base()),
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
        研究数 = sapply(synth_levels, function(lvl) nrow(dplyr::filter(df, subgroup_level == lvl))),
        stringsAsFactors = FALSE
      )
      datatable(display_df, selection = 'single',
                options = list(dom = 't', pageLength = 10,
                               columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                rownames = FALSE)
    })

    observeEvent(input$delete_synth_level, {
      req(input$synthetic_levels_table_rows_selected)
      selected_row <- input$synthetic_levels_table_rows_selected
      synth_df <- synthetic_data()
      req(synth_df)
      levels_unique <- unique(synth_df$subgroup_level)
      level_to_remove <- levels_unique[selected_row]
      new_synth <- dplyr::filter(synth_df, subgroup_level != level_to_remove)
      if (nrow(new_synth) == 0) new_synth <- NULL
      synthetic_data(new_synth)
      current_order <- selected_order()
      new_order <- current_order[current_order != level_to_remove]
      selected_order(new_order)
      updateSelectInput(session, "right", choices = new_order)
      showNotification(paste0("已删除合成水平：", level_to_remove), type = "warning")
    })

    observeEvent(c(input$outcome, input$subgroup, input$data_type, input$study_group_var), {
      synthetic_data(NULL)
    })

    # ====== 基础数据准备（current_data_base） ======
    # 注意：此处根据 selected_study_ids 过滤研究
    current_data_base <- reactive({
      req(input$outcome)

      # 获取用户选中的研究 ID
      selected_ids <- selected_study_ids()
      if (is.null(selected_ids)) return(NULL)

      if (input$data_type == "binary") {
        # 二分类数据，先按结局和研究 ID 过滤
        filtered <- binary_data %>%
          dplyr::filter(outcome == input$outcome,
                        unique_id %in% selected_ids)

        if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
          # 亚组分析
          filtered <- filtered %>% dplyr::filter(tolower(subgroup) == tolower(input$subgroup))
          level_data <- filtered %>% dplyr::filter(!is.na(subgroup_option)) %>% mutate(level = subgroup_option)
          if (nrow(level_data) == 0) return(NULL)
          level_data <- level_data %>% mutate(arm = if_else(grepl("_ctrl$", treatment), "ctrl", "ici"))
          study_levels <- level_data %>%
            group_by(unique_id, level, arm) %>%
            summarise(d = sum(d, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop") %>%
            pivot_wider(id_cols = c(unique_id, level), names_from = arm, values_from = c(d, n)) %>%
            dplyr::filter(!is.na(d_ici) & !is.na(n_ici) & !is.na(d_ctrl) & !is.na(n_ctrl))
          if (nrow(study_levels) == 0) return(NULL)
          study_levels <- study_levels %>%
            left_join(level_data %>% select(unique_id, study_display) %>% distinct(unique_id, .keep_all = TRUE), by = "unique_id") %>%
            mutate(OR_approx = (d_ici * (n_ctrl - d_ctrl)) / ((n_ici - d_ici) * d_ctrl + 1e-10)) %>%
            arrange(level, OR_approx) %>% select(-OR_approx)
          result <- study_levels %>%
            rename(event_ici = d_ici, n_ici = n_ici, event_ctrl = d_ctrl, n_ctrl = n_ctrl, subgroup_level = level) %>%
            select(unique_id, study_display, event_ici, n_ici, event_ctrl, n_ctrl, subgroup_level)
          return(result)
        } else {
          # 总体分析（无亚组）
          filtered_overall <- dplyr::filter(filtered, subgroup == "Overall")
          if (nrow(filtered_overall) == 0) return(NULL)
          unique_studies <- unique(filtered_overall$unique_id)
          data_list <- list()
          for (s in unique_studies) {
            study_rows <- dplyr::filter(filtered_overall, unique_id == s)
            ctrl_idx <- grepl("_ctrl$", study_rows$treatment)
            exp_idx <- !ctrl_idx
            if (sum(ctrl_idx) != 1 || sum(exp_idx) != 1) next
            ctrl_rows <- study_rows[ctrl_idx, ]; exp_rows <- study_rows[exp_idx, ]
            data_list[[s]] <- data.frame(unique_id = s, study_display = study_rows$study_display[1],
                                         event_ici = exp_rows$d, n_ici = exp_rows$n,
                                         event_ctrl = ctrl_rows$d, n_ctrl = ctrl_rows$n,
                                         stringsAsFactors = FALSE)
          }
          if (length(data_list) == 0) return(NULL)
          result <- do.call(rbind, data_list)
          result$OR_true <- (result$event_ici * (result$n_ctrl - result$event_ctrl)) / ((result$n_ici - result$event_ici) * result$event_ctrl + 1e-10)
          result <- result[order(result$OR_true, decreasing = FALSE), ]; result$OR_true <- NULL

          if (!is.null(input$study_group_var) && input$study_group_var != "none") {
            group_info <- studies_info %>% select(unique_id, phase, regimen_type)
            result <- result %>% left_join(group_info, by = "unique_id")
            if (input$study_group_var == "phase") {
              result$subgroup_level <- result$phase
            } else if (input$study_group_var == "regimen_type") {
              result$subgroup_level <- result$regimen_type
            }
            result <- dplyr::filter(result, !is.na(subgroup_level))
          }
          return(result[, c("unique_id", "study_display", "event_ici", "n_ici", "event_ctrl", "n_ctrl",
                            if("subgroup_level" %in% names(result)) "subgroup_level" else NULL)])
        }
      } else {
        # 生存数据 (HR)
        filtered <- survival_data %>%
          dplyr::filter(outcome == input$outcome,
                        unique_id %in% selected_ids,
                        !is.na(te) & !is.na(se_te))

        if (nrow(filtered) == 0) return(NULL)

        if (!is.null(input$subgroup) && input$subgroup != "" && input$subgroup != "Overall") {
          if (!("subgroup" %in% names(filtered)) || all(is.na(filtered$subgroup))) return(NULL)
          filtered <- dplyr::filter(filtered, tolower(subgroup) == tolower(input$subgroup))
          if (!("subgroup_option" %in% names(filtered)) || all(is.na(filtered$subgroup_option))) return(NULL)
          grouped <- filtered %>% dplyr::filter(!is.na(subgroup_option)) %>% group_by(unique_id, subgroup_option)
          selected_rows <- grouped %>% group_modify(~ select_study_record_priority(.x, input$stratified_filter)) %>% ungroup()
          if (nrow(selected_rows) == 0) return(NULL)
          result <- selected_rows %>% rename(subgroup_level = subgroup_option) %>% select(unique_id, study_display, te, se_te, n1, n2, subgroup_level)
          return(result)
        } else {
          # 总体分析
          if ("subgroup" %in% names(filtered) && any(filtered$subgroup == "Overall", na.rm = TRUE)) {
            filtered_overall <- dplyr::filter(filtered, subgroup == "Overall")
          } else {
            filtered_overall <- filtered
          }
          grouped <- filtered_overall %>% group_by(unique_id)
          selected_rows <- grouped %>% group_modify(~ select_study_record_priority(.x, input$stratified_filter)) %>% ungroup()
          if (nrow(selected_rows) == 0) return(NULL)
          result <- selected_rows %>% select(unique_id, study_display, te, se_te, n1, n2)

          if (!is.null(input$study_group_var) && input$study_group_var != "none") {
            group_info <- studies_info %>% select(unique_id, phase, regimen_type)
            result <- result %>% left_join(group_info, by = "unique_id")
            if (input$study_group_var == "phase") {
              result$subgroup_level <- result$phase
            } else if (input$study_group_var == "regimen_type") {
              result$subgroup_level <- result$regimen_type
            }
            result <- dplyr::filter(result, !is.na(subgroup_level))
          }
          return(result)
        }
      }
    })

    # ====== 最终分析数据（current_data） ======
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
          base <- base %>% group_by(unique_id, subgroup_level) %>% arrange(is_synthetic) %>% slice(n()) %>% ungroup()
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
        base <- dplyr::filter(base, subgroup_level %in% order_vec)
        base$subgroup_level <- factor(base$subgroup_level, levels = order_vec)
        base <- base %>% arrange(subgroup_level)
      }
      base
    })

    # ====== 返回所有对外接口 ======
    reactive({
      list(
        current_data = current_data(),
        data_type = input$data_type,
        outcome = input$outcome,
        subgroup = input$subgroup,
        study_group_var = input$study_group_var,
        stratified_filter = input$stratified_filter,
        model = NULL
      )
    })
  })
}
