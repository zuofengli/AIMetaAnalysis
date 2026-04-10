# ===============================
# Multi-Type Data Extraction Assistant App
# ===============================
# 所有参考数据（studies_info, binary_data, survival, continuous）从包的 clinical_studies_data.xlsx 只读加载
# 用户输入保存在 user_logs.xlsx 的 my_inputs 表中
# 数据字典（data_dictionary）仅用于增强 prompt，应用不会修改字典

library(shiny)
library(clipr)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)

# ----------------------------------------------------------------------
# 1. File paths
# ----------------------------------------------------------------------
pkg_file <- system.file("extdata", "clinical_studies_data.xlsx", package = "AIMetaAnalysis")
if (pkg_file == "") {
  pkg_file <- file.path("extdata", "clinical_studies_data.xlsx")
  if (!file.exists(pkg_file)) pkg_file <- "clinical_studies_data.xlsx"
}
user_logs <- "user_logs.xlsx"

# ----------------------------------------------------------------------
# 2. Helper functions to read reference data (read-only)
# ----------------------------------------------------------------------
read_reference_sheet <- function(sheet_name) {
  if (!file.exists(pkg_file)) return(NULL)
  tryCatch(
    read_excel(pkg_file, sheet = sheet_name),
    error = function(e) NULL
  )
}

load_sheet_data <- function(sheet_name) {
  df <- read_reference_sheet(sheet_name)
  if (is.null(df)) return(list(outcomes = character(), col_names = character()))
  if ("removed" %in% names(df)) {
    df <- df %>% filter(is.na(removed) | removed != "YES")
  }
  outcomes <- unique(df$outcome)
  outcomes <- outcomes[!is.na(outcomes)]
  col_names <- names(df)
  list(outcomes = outcomes, col_names = col_names)
}

# ----------------------------------------------------------------------
# 3. Load data dictionary (read-only)
# ----------------------------------------------------------------------
load_data_dictionary <- function(file) {
  if (!file.exists(file)) return(list())
  # 检查 sheet 是否存在
  sheets <- excel_sheets(file)
  if (!"data_dictionary" %in% sheets) {
    warning("data_dictionary sheet not found. Please run 'update_data_dictionary.R' to create it.")
    return(list())
  }
  dict_df <- tryCatch(
    read_excel(file, sheet = "data_dictionary"),
    error = function(e) NULL
  )
  if (is.null(dict_df) || nrow(dict_df) == 0) return(list())
  # 只保留 active 的行
  dict_df <- dict_df %>% filter(status == "active")
  # 构建嵌套列表
  dict <- list()
  for (i in 1:nrow(dict_df)) {
    dtype <- dict_df$data_type[i]
    col <- dict_df$sheet_column[i]
    if (!dtype %in% names(dict)) dict[[dtype]] <- list()
    if (!col %in% names(dict[[dtype]])) dict[[dtype]][[col]] <- list()
    ct <- dict_df$constraint_type[i]
    cv <- dict_df$constraint_value[i]
    dict[[dtype]][[col]][[ct]] <- cv
    # 附加其他字段
    if ("is_critical" %in% names(dict_df)) dict[[dtype]][[col]]$is_critical <- dict_df$is_critical[i]
    if ("example" %in% names(dict_df)) dict[[dtype]][[col]]$example <- dict_df$example[i]
    if ("notes" %in% names(dict_df)) dict[[dtype]][[col]]$notes <- dict_df$notes[i]
  }
  return(dict)
}

# 加载参考数据
studies_info <- read_reference_sheet("studies_info")
if (is.null(studies_info) || nrow(studies_info) == 0) {
  studies_info <- data.frame(trial = character(), pub_year = integer(),
                             phase = character(), stringsAsFactors = FALSE)
}
if ("year" %in% names(studies_info) && !"pub_year" %in% names(studies_info)) {
  studies_info$pub_year <- studies_info$year
  studies_info$year <- NULL
}
if (!"pub_year" %in% names(studies_info)) {
  studies_info$pub_year <- NA_integer_
}
studies_info$study_display <- ifelse(is.na(studies_info$pub_year),
                                     studies_info$trial,
                                     paste(studies_info$trial, studies_info$pub_year, sep = " - "))

binary_data   <- load_sheet_data("binary_data")
survival_data <- load_sheet_data("survival")
continuous_data <- load_sheet_data("continuous")

# 加载数据字典（只读）
data_dict <- load_data_dictionary(pkg_file)

# ----------------------------------------------------------------------
# 4. User logs handling (unchanged)
# ----------------------------------------------------------------------
ensure_user_logs <- function() {
  if (!file.exists(user_logs)) {
    empty_df <- data.frame(trial = character(), pub_year = integer(),
                           data_type = character(), outcome_name = character(),
                           section_name = character(), free_text = character(),
                           image_base64 = character(), stringsAsFactors = FALSE)
    wb <- createWorkbook()
    addWorksheet(wb, "my_inputs")
    writeData(wb, "my_inputs", empty_df, colNames = TRUE)
    saveWorkbook(wb, user_logs, overwrite = TRUE)
  }
}
ensure_user_logs()

read_user_inputs <- function(trial, pub_year, data_type = NULL, outcome_name = NULL, section_name = NULL) {
  if (!file.exists(user_logs)) return(data.frame())
  df <- tryCatch(
    read_excel(user_logs, sheet = "my_inputs"),
    error = function(e) data.frame()
  )
  if (nrow(df) == 0) return(df)
  df <- df %>% filter(trial == !!trial, pub_year == !!pub_year)
  if (!is.null(data_type)) df <- df %>% filter(data_type == !!data_type)
  if (!is.null(outcome_name)) df <- df %>% filter(outcome_name == !!outcome_name)
  if (!is.null(section_name)) df <- df %>% filter(section_name == !!section_name)
  df
}

save_user_input <- function(trial, pub_year, data_type, outcome_name, section_name, free_text, image_base64 = NA) {
  ensure_user_logs()
  df <- tryCatch(
    read_excel(user_logs, sheet = "my_inputs"),
    error = function(e) data.frame()
  )
  if (nrow(df) == 0) {
    df <- data.frame(trial = character(), pub_year = integer(),
                     data_type = character(), outcome_name = character(),
                     section_name = character(), free_text = character(),
                     image_base64 = character(), stringsAsFactors = FALSE)
  }
  idx <- which(df$trial == trial & df$pub_year == pub_year &
                 df$data_type == data_type & df$outcome_name == outcome_name &
                 df$section_name == section_name)
  if (length(idx) > 0) {
    df[idx, "free_text"] <- free_text
    if (!is.na(image_base64)) df[idx, "image_base64"] <- image_base64
  } else {
    new_row <- data.frame(trial = trial, pub_year = pub_year, data_type = data_type,
                          outcome_name = outcome_name, section_name = section_name,
                          free_text = free_text, image_base64 = image_base64,
                          stringsAsFactors = FALSE)
    df <- bind_rows(df, new_row)
  }
  wb <- loadWorkbook(user_logs)
  writeData(wb, "my_inputs", df, startCol = 1, startRow = 1, colNames = TRUE)
  saveWorkbook(wb, user_logs, overwrite = TRUE)
}

# ----------------------------------------------------------------------
# 5. UI
# ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Multi-Type Data Extraction Assistant – Generate LLM Prompt for Arm‑Level Data"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Study Information"),
      selectInput("existing_study", "Select existing study",
                  choices = c("New study", studies_info$study_display),
                  selected = "New study"),
      uiOutput("study_metadata_ui"),
      hr(),
      h4("Data Type & Outcome"),
      radioButtons("data_type", "Data type",
                   choices = c("Binary" = "binary",
                               "Survival" = "survival",
                               "Continuous" = "continuous"),
                   selected = "binary"),
      uiOutput("outcome_ui"),
      hr(),
      h4("Period"),
      textInput("period", "Period", value = "Overall"),
      br(),
      actionButton("save_inputs", "Save User Inputs", icon = icon("save")),
      br(),
      actionButton("generate_prompt", "Generate Prompt & Copy to Clipboard",
                   icon = icon("copy"), class = "btn-primary"),
      br(), br(),
      helpText("After copying the prompt, paste it into a large language model (e.g., ChatGPT).",
               "The model will return arm-level data. Copy that data and paste it into",
               "the corresponding sheet (binary_data, survival, or continuous) of your",
               "clinical_studies_data.xlsx file.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Table 1 Text",
                 br(),
                 helpText("Copy the entire Table 1 from the PDF (including headers and numbers) and paste it here. ",
                          "The formatting can be messy; the LLM will interpret it."),
                 textAreaInput("table1_text", "Table 1 text", rows = 15, width = "100%",
                               placeholder = "Paste Table 1 content here...")
        ),
        tabPanel("Subgroup Results Text",
                 br(),
                 helpText("Copy the subgroup analysis results table (forest plot data) from the PDF and paste it here. ",
                          "Include the subgroup names, categories, treatment groups, and the relevant statistics (e.g., rates with CIs, median survival, HR, mean difference)."),
                 textAreaInput("subgroup_text", "Subgroup results text", rows = 15, width = "100%",
                               placeholder = "Paste subgroup results here...")
        ),
        tabPanel("Generated Prompt",
                 br(),
                 h5("Prompt to be sent to LLM:"),
                 verbatimTextOutput("prompt_text")
        )
      )
    )
  )
)

# ----------------------------------------------------------------------
# 6. Server
# ----------------------------------------------------------------------
server <- function(input, output, session) {

  study_metadata <- reactiveValues(study_id = "", trial = "", pub_year = "", phase = "")

  current_study_info <- reactive({
    if (input$existing_study == "New study") return(NULL)
    idx <- which(studies_info$study_display == input$existing_study)
    if (length(idx) == 0) return(NULL)
    list(
      trial = studies_info$trial[idx],
      pub_year = studies_info$pub_year[idx],
      phase = studies_info$phase[idx]
    )
  })

  observeEvent(input$existing_study, {
    if (input$existing_study == "New study") {
      updateTextInput(session, "study_id", value = "")
      updateTextInput(session, "trial", value = "")
      updateTextInput(session, "pub_year", value = "")
      updateTextInput(session, "phase", value = "")
      updateTextAreaInput(session, "table1_text", value = "")
      updateTextAreaInput(session, "subgroup_text", value = "")
    } else {
      info <- current_study_info()
      if (!is.null(info)) {
        updateTextInput(session, "study_id", value = info$trial)
        updateTextInput(session, "trial", value = info$trial)
        updateTextInput(session, "pub_year", value = as.character(info$pub_year))
        updateTextInput(session, "phase", value = info$phase)

        saved_table1 <- read_user_inputs(trial = info$trial, pub_year = info$pub_year,
                                         data_type = "Overall", outcome_name = "Overall",
                                         section_name = "table_one")
        updateTextAreaInput(session, "table1_text", value = if (nrow(saved_table1) > 0) saved_table1$free_text[1] else "")
      }
    }
  })

  observeEvent(list(input$existing_study, input$data_type, input$outcome), {
    if (input$existing_study != "New study") {
      info <- current_study_info()
      if (!is.null(info)) {
        saved_subgroup <- read_user_inputs(trial = info$trial, pub_year = info$pub_year,
                                           data_type = input$data_type, outcome_name = input$outcome,
                                           section_name = "subgroup")
        updateTextAreaInput(session, "subgroup_text", value = if (nrow(saved_subgroup) > 0) saved_subgroup$free_text[1] else "")
      }
    }
  })

  output$study_metadata_ui <- renderUI({
    tagList(
      textInput("study_id", "Study ID (abbreviation)", value = ""),
      textInput("trial", "Trial full name", value = ""),
      textInput("pub_year", "Publication year", value = ""),
      textInput("phase", "Phase", value = "")
    )
  })

  current_outcomes <- reactive({
    dt <- input$data_type
    if (dt == "binary") return(binary_data$outcomes)
    else if (dt == "survival") return(survival_data$outcomes)
    else return(continuous_data$outcomes)
  })

  output$outcome_ui <- renderUI({
    choices <- current_outcomes()
    selectInput("outcome", "Outcome name",
                choices = c("", choices),
                selected = if (length(choices) > 0) choices[1] else "")
  })

  get_target_columns <- reactive({
    dt <- input$data_type
    if (dt == "binary") return(binary_data$col_names)
    else if (dt == "survival") return(survival_data$col_names)
    else return(continuous_data$col_names)
  })

  # 根据数据字典生成列约束描述
  get_column_constraints_text <- function(dtype, colname) {
    if (!dtype %in% names(data_dict)) return("")
    col_dict <- data_dict[[dtype]][[colname]]
    if (is.null(col_dict)) return("")
    parts <- c()
    if ("data_type" %in% names(col_dict)) {
      parts <- c(parts, paste0("type: ", col_dict$data_type))
    }
    if ("allowed_values" %in% names(col_dict)) {
      parts <- c(parts, paste0("allowed: ", col_dict$allowed_values))
    }
    if ("range" %in% names(col_dict)) {
      parts <- c(parts, paste0("range: ", col_dict$range))
    }
    if ("length" %in% names(col_dict)) {
      parts <- c(parts, paste0("max length: ", col_dict$length))
    }
    if ("required" %in% names(col_dict) && tolower(col_dict$required) == "true") {
      parts <- c(parts, "required")
    }
    if ("pattern" %in% names(col_dict)) {
      parts <- c(parts, paste0("pattern: ", col_dict$pattern))
    }
    if ("description" %in% names(col_dict) && !is.na(col_dict$description)) {
      parts <- c(parts, paste0("desc: ", col_dict$description))
    }
    if ("example" %in% names(col_dict) && !is.na(col_dict$example)) {
      parts <- c(parts, paste0("e.g., ", col_dict$example))
    }
    if (length(parts) == 0) return("")
    paste0(" (", paste(parts, collapse = "; "), ")")
  }

  observeEvent(input$save_inputs, {
    req(input$trial, input$pub_year)
    if (!is.null(input$table1_text) && input$table1_text != "") {
      tryCatch({
        save_user_input(trial = input$trial,
                        pub_year = as.integer(input$pub_year),
                        data_type = "Overall",
                        outcome_name = "Overall",
                        section_name = "table_one",
                        free_text = input$table1_text)
      }, error = function(e) {
        showNotification(paste("保存 Table 1 失败:", e$message), type = "error")
      })
    }
    if (!is.null(input$outcome) && input$outcome != "" &&
        !is.null(input$subgroup_text) && input$subgroup_text != "") {
      tryCatch({
        save_user_input(trial = input$trial,
                        pub_year = as.integer(input$pub_year),
                        data_type = input$data_type,
                        outcome_name = input$outcome,
                        section_name = "subgroup",
                        free_text = input$subgroup_text)
      }, error = function(e) {
        showNotification(paste("保存 Subgroup 失败:", e$message), type = "error")
      })
    }
    showNotification("User inputs saved successfully.", type = "message")
  })

  extraction_guide_detail <- reactive({
    dt <- input$data_type
    if (dt == "binary") {
      return("Extract the event rate (%) for each treatment group and subgroup category. If 95% confidence intervals are provided (e.g., '36.8 (24.3-49.4)'), extract the rate and optionally the CI bounds into appropriate columns (e.g., rate, ci_lower, ci_upper).")
    } else if (dt == "survival") {
      guide <- "Extract the following survival-related statistics from the subgroup results table:\n"
      cols <- get_target_columns()
      if ("hr" %in% cols) guide <- paste0(guide, "   - Hazard Ratio (HR)\n")
      if ("hr_lci" %in% cols && "hr_uci" %in% cols) guide <- paste0(guide, "   - 95% CI for HR (lower and upper)\n")
      if ("n1" %in% cols && "n2" %in% cols) guide <- paste0(guide, "   - Number of patients in each group (n1, n2)\n")
      if ("treatment_1_event_number" %in% cols && "treatment_2_event_number" %in% cols) guide <- paste0(guide, "   - Number of events in each group\n")
      if ("median_follow_up_months" %in% cols) guide <- paste0(guide, "   - Median follow-up (months)\n")
      if (any(c("te", "se_te") %in% cols)) guide <- paste0(guide, "   - Log HR (te) and its SE (se_te)\n")
      guide <- paste0(guide, "Use 'n.d.' for missing values.")
      return(guide)
    } else {
      return("Extract the mean (or mean change) and SD (or 95% CI) for each group. Use 'n.d.' if missing.")
    }
  })

  prompt_text <- reactive({
    req(input$study_id, input$trial, input$pub_year, input$outcome)
    if (is.null(input$table1_text) || input$table1_text == "") {
      showNotification("Table 1 text is empty. Please paste content.", type = "warning")
      return("")
    }
    if (is.null(input$subgroup_text) || input$subgroup_text == "") {
      showNotification("Subgroup results text is empty. Please paste content.", type = "warning")
      return("")
    }

    data_type <- input$data_type
    outcome_type <- ifelse(data_type == "binary", "Binary",
                           ifelse(data_type == "survival", "Survival (time-to-event)", "Continuous"))

    target_cols <- get_target_columns()
    if (length(target_cols) == 0) {
      showNotification(paste("No columns found for", toupper(data_type), "data. Check Excel sheet."), type = "error")
      return("")
    }

    col_descriptions <- c()
    for (col in target_cols) {
      constraint_text <- get_column_constraints_text(data_type, col)
      col_descriptions <- c(col_descriptions, paste0("   - `", col, "`", constraint_text))
    }
    output_cols_str <- paste(col_descriptions, collapse = "\n")

    paste0(
      "You are an assistant that extracts arm‑level data from clinical trial subgroup tables.\n\n",
      "## Study Metadata\n",
      "- Study ID: ", input$study_id, "\n",
      "- Trial: ", input$trial, "\n",
      "- Publication year: ", input$pub_year, "\n",
      "- Phase: ", input$phase, "\n",
      "- Outcome type: ", outcome_type, "\n",
      "- Outcome: ", input$outcome, "\n",
      "- Period: ", input$period, "\n\n",
      "## Raw Table 1 (Baseline Characteristics)\n",
      "```\n", input$table1_text, "\n```\n\n",
      "## Raw Subgroup Results Table\n",
      "```\n", input$subgroup_text, "\n```\n\n",
      "## Instructions\n",
      "1. Parse the text to extract for each subgroup and treatment arm:\n",
      "   - Subgroup name, category, treatment group names\n",
      "   - Number of patients (n) from Table 1\n",
      "   - Outcome measures (as described: ", extraction_guide_detail(), ")\n",
      "2. Combine into one row per subgroup/treatment combination.\n",
      "3. If n=0 for a treatment arm, set outcome to 'n.d.'.\n",
      "4. Follow column constraints strictly.\n",
      "5. Return **plain TSV** with headers exactly as below, no extra text.\n\n",
      "### Output columns (order must match):\n",
      output_cols_str, "\n\n",
      "Now produce the TSV table:"
    )
  })

  output$prompt_text <- renderPrint({
    cat(prompt_text())
  })

  observeEvent(input$generate_prompt, {
    txt <- prompt_text()
    if (txt == "") {
      showNotification("No prompt generated. Please fill in all fields.", type = "error")
      return()
    }
    tryCatch({
      write_clip(txt)
      showNotification("Prompt copied to clipboard!", type = "message")
    }, error = function(e) {
      showNotification("Could not copy to clipboard. Select and copy manually.", type = "warning")
    })
  })
}

# ----------------------------------------------------------------------
# 7. Run app
# ----------------------------------------------------------------------
shinyApp(ui, server)
