# ===============================
# Multi-Type Data Extraction Assistant App
# ===============================
# 所有参考数据（studies_info, binary_data, survival, continuous）从包的 clinical_studies_data.xlsx 只读加载
# 用户输入保存在 user_logs.xlsx 的 my_inputs 表中

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
# 2. Helper functions to read reference data
# ----------------------------------------------------------------------
read_reference_sheet <- function(sheet_name) {
  if (!file.exists(pkg_file)) return(NULL)
  tryCatch(
    read_excel(pkg_file, sheet = sheet_name),
    error = function(e) NULL
  )
}

# Load reference sheets
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

binary_data <- load_sheet_data("binary_data")
survival_data <- load_sheet_data("survival")
continuous_data <- load_sheet_data("continuous")

# ----------------------------------------------------------------------
# 3. User logs handling
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
# 4. UI
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
# 5. Server
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

  observeEvent(input$save_inputs, {
    req(input$trial, input$pub_year)
    # 保存 Table 1
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
    # 保存 Subgroup
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
                           ifelse(data_type == "survival", "Survival", "Continuous"))
    extraction_guide <- switch(data_type,
                               binary = "Extract the event rate (%) for each treatment group and subgroup category. ",
                               survival = "Extract the median survival time (or HR with confidence intervals) for each treatment group and subgroup category. ",
                               continuous = "Extract the mean (or mean change) and standard deviation (or 95% CI) for each treatment group and subgroup category. ")

    target_cols <- get_target_columns()
    if (length(target_cols) == 0) {
      showNotification("No column information found for selected data type.", type = "error")
      return("")
    }
    output_cols_str <- paste(target_cols, collapse = "\n   - ")

    paste0(
      "You are an assistant that helps extract arm‑level data from clinical trial subgroup tables. ",
      "I will provide you with raw text copied from a PDF containing Table 1 (baseline characteristics) ",
      "and a subgroup analysis results table. Your task is to convert this information into a structured ",
      "arm‑level dataset as specified.\n\n",
      "## Study Metadata\n",
      "- Study ID: ", input$study_id, "\n",
      "- Trial: ", input$trial, "\n",
      "- Publication year: ", input$pub_year, "\n",
      "- Phase: ", input$phase, "\n",
      "- Outcome type: ", outcome_type, "\n",
      "- Outcome: ", input$outcome, "\n",
      "- Period: ", input$period, "\n\n",
      "## Raw Table 1 (Baseline Characteristics)\n",
      "The following text is copied directly from the PDF. It contains the number of patients (n) for each ",
      "treatment group and subgroup category. Please extract this information accurately.\n\n",
      "```\n", input$table1_text, "\n```\n\n",
      "## Raw Subgroup Results Table\n",
      "The following text is copied directly from the PDF. It contains the outcome results for ",
      "each treatment group and subgroup category. ", extraction_guide,
      "Values may include 95% confidence intervals (e.g., '36.8 (24.3-49.4)'). ",
      "Use 'n.d.' for not determined.\n\n",
      "```\n", input$subgroup_text, "\n```\n\n",
      "## Instructions\n",
      "1. Parse the provided raw text to extract the following for each subgroup and treatment arm:\n",
      "   - Subgroup name\n",
      "   - Subgroup category (e.g., Male/Female, <65/>=65)\n",
      "   - Treatment group name (e.g., 'Nivolumab+Chemo', 'Chemotherapy alone')\n",
      "   - Number of patients (n) from Table 1\n",
      "   - The outcome measure (rate, median, HR, mean, etc.) from the subgroup results table\n",
      "2. Combine the data to produce a single arm‑level dataset with the following columns (in this exact order):\n",
      "   - `", paste(target_cols, collapse = "`\n   - `"), "`\n",
      "3. For each combination of subgroup, subgroup_option, and treatment, create one row.\n",
      "4. If a treatment group has no patients (n=0) in Table 1, set the outcome measure to 'n.d.'.\n",
      "5. If n is missing for a particular subgroup_option/treatment combination, keep it empty.\n",
      "6. Return the result as a **plain text table with tab-separated values (TSV)**.\n",
      "   - The first row must contain the column headers (exactly the column names listed above).\n",
      "   - Each subsequent row corresponds to one arm-level record, with values in the same order as the headers.\n",
      "   - Do not include any extra commentary, code fences, or markdown formatting.\n",
      "   - The output should be directly copyable and pastable into Excel, where it will automatically split into columns.\n",
      "\n",
      "### Output columns (order must match the TSV headers):\n",
      "   - ", output_cols_str
    )
  })

  output$prompt_text <- renderPrint({
    cat(prompt_text())
  })

  observeEvent(input$generate_prompt, {
    txt <- prompt_text()
    if (txt == "") {
      showNotification("No prompt generated. Please fill in all required fields and paste text.", type = "error")
      return()
    }
    tryCatch({
      write_clip(txt)
      showNotification("Prompt copied to clipboard!", type = "message")
    }, error = function(e) {
      showNotification("Could not copy to clipboard. You can manually select and copy the text.",
                       type = "warning")
    })
  })
}

# ----------------------------------------------------------------------
# 6. Run app
# ----------------------------------------------------------------------
shinyApp(ui, server)
