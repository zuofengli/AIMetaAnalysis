# ===============================
# Binary Data Extraction Assistant App
# ===============================
# This app helps users extract subgroup data from clinical trial publications
# by generating a structured prompt for an LLM. Users paste the raw text of
# Table 1 and subgroup results (from PDF) into text areas. The app combines
# these with study metadata and outcome selection to produce a prompt that
# instructs the LLM to return arm-level data.

library(shiny)
library(clipr)
library(readxl)
library(dplyr)
library(stringr)

# ----------------------------------------------------------------------
# 1. Load data from package's Excel file
# ----------------------------------------------------------------------
get_data_file <- function() {
  pkg_file <- system.file("extdata", "clinical_studies_data.xlsx", package = "AIMetaAnalysis")
  if (pkg_file != "") return(pkg_file)
  rel_file <- file.path("extdata", "clinical_studies_data.xlsx")
  if (file.exists(rel_file)) return(rel_file)
  if (file.exists("clinical_studies_data.xlsx")) return("clinical_studies_data.xlsx")
  stop("Cannot find clinical_studies_data.xlsx. Please ensure it is in extdata/ or current working directory.")
}

data_file <- get_data_file()

# Read studies_info sheet
studies_info <- tryCatch(
  read_excel(data_file, sheet = "studies_info"),
  error = function(e) data.frame(trial = character(), phase = character(),
                                 year = integer(), stringsAsFactors = FALSE)
)

# Read binary_data sheet to get unique outcomes
binary_data <- tryCatch(
  read_excel(data_file, sheet = "binary_data"),
  error = function(e) data.frame(outcome = character(), stringsAsFactors = FALSE)
)

# Filter out removed rows if present
if ("removed" %in% names(binary_data)) {
  binary_data <- binary_data %>% filter(is.na(removed) | removed != "YES")
}

# Extract unique outcomes
outcome_choices <- unique(binary_data$outcome)
outcome_choices <- outcome_choices[!is.na(outcome_choices)]

# Standardize study name column for selection
if ("trial" %in% names(studies_info)) {
  studies_info$study_name <- studies_info$trial
} else if (!"study_name" %in% names(studies_info)) {
  studies_info$study_name <- studies_info[[1]]
}

# ----------------------------------------------------------------------
# 2. UI
# ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Binary Data Extraction Assistant – Generate LLM Prompt for Arm‑Level Data"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Study Information"),
      selectInput("existing_study", "Select existing study",
                  choices = c("New study", studies_info$study_name),
                  selected = "New study"),
      uiOutput("study_metadata_ui"),
      hr(),
      h4("Outcome"),
      selectInput("outcome", "Outcome name",
                  choices = c("", outcome_choices),
                  selected = if(length(outcome_choices) > 0) outcome_choices[1] else ""),
      hr(),
      h4("Period"),
      textInput("period", "Period", value = "Overall"),
      br(),
      actionButton("generate_prompt", "Generate Prompt & Copy to Clipboard",
                   icon = icon("copy"), class = "btn-primary"),
      br(), br(),
      helpText("After copying the prompt, paste it into a large language model (e.g., ChatGPT).",
               "The model will return arm-level data. Copy that data and paste it into",
               "the binary_data sheet of your clinical_studies_data.xlsx file.")
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
                          "Include the subgroup names, categories, treatment groups, and rates with CIs."),
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
# 3. Server
# ----------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Study metadata reactive ----
  study_metadata <- reactiveValues(
    study_id = "",
    trial = "",
    pub_year = "",
    phase = ""
  )

  observeEvent(input$existing_study, {
    if (input$existing_study == "New study") {
      updateTextInput(session, "study_id", value = "")
      updateTextInput(session, "trial", value = "")
      updateTextInput(session, "pub_year", value = "")
      updateTextInput(session, "phase", value = "")
    } else {
      sel <- studies_info[studies_info$study_name == input$existing_study, ]
      updateTextInput(session, "study_id", value = sel$study_name[1])
      updateTextInput(session, "trial", value = sel$trial[1])
      updateTextInput(session, "pub_year", value = as.character(sel$year[1]))
      updateTextInput(session, "phase", value = sel$phase[1])
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

  # ---- Generate prompt ----
  prompt_text <- reactive({
    # Check required fields
    req(input$study_id, input$trial, input$pub_year, input$outcome)

    table1_txt <- input$table1_text
    subgroup_txt <- input$subgroup_text

    if (is.null(table1_txt) || table1_txt == "") {
      showNotification("Table 1 text is empty. Please paste content.", type = "warning")
      return("")
    }
    if (is.null(subgroup_txt) || subgroup_txt == "") {
      showNotification("Subgroup results text is empty. Please paste content.", type = "warning")
      return("")
    }

    # Build prompt
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
      "- Outcome: ", input$outcome, "\n",
      "- Outcome type: Binary\n",
      "- Period: ", input$period, "\n\n",
      "## Raw Table 1 (Baseline Characteristics)\n",
      "The following text is copied directly from the PDF. It contains the number of patients (n) for each ",
      "treatment group and subgroup category. Please extract this information accurately.\n\n",
      "```\n", table1_txt, "\n```\n\n",
      "## Raw Subgroup Results Table\n",
      "The following text is copied directly from the PDF. It contains event rates (e.g., pCR rates) for ",
      "each treatment group and subgroup category. Rates may include 95% confidence intervals (e.g., '36.8 (24.3-49.4)'). ",
      "Use 'n.d.' for not determined.\n\n",
      "```\n", subgroup_txt, "\n```\n\n",
      "## Instructions\n",
      "1. Parse the provided raw text to extract the following for each subgroup and treatment arm:\n",
      "   - Subgroup name\n",
      "   - Subgroup category (e.g., Male/Female, <65/>=65)\n",
      "   - Treatment group name (e.g., 'Nivolumab+Chemo', 'Chemotherapy alone')\n",
      "   - Number of patients (n) from Table 1\n",
      "   - Event rate (%) from the subgroup results table\n",
      "2. Combine the data to produce a single arm‑level dataset with the following columns (in order):\n",
      "   - `removed` (always 'NO' unless data missing)\n",
      "   - `removed_reason` (leave blank)\n",
      "   - `study_id` (fill with study ID from metadata)\n",
      "   - `study` (same as study_id)\n",
      "   - `trial`\n",
      "   - `pub_year`\n",
      "   - `outcome_type` (fill with 'Binary')\n",
      "   - `outcome` (use the outcome name from metadata)\n",
      "   - `subgroup`\n",
      "   - `subgroup_option`\n",
      "   - `grade` (leave blank)\n",
      "   - `period` (use period from metadata)\n",
      "   - `treatment`\n",
      "   - `d` (number of events; calculate as round(event_rate% × n / 100) if n is available; if event_rate is n.d., set d = NA)\n",
      "   - `n` (from Table 1)\n",
      "   - `Event ratio in Subgroup_option+treatment` (keep the original event_rate string, including CI if present)\n",
      "   - `Total number of Events in subgroup_option` (sum of d across treatments for that subgroup_option)\n",
      "3. For each combination of subgroup, subgroup_option, and treatment, create one row.\n",
      "4. If a treatment group has no patients (n=0) in Table 1, set d=0 and event_rate='n.d.'.\n",
      "5. If n is missing for a particular subgroup_option/treatment combination, keep it empty.\n",
      "6. Calculate `Total number of Events in subgroup_option` as the sum of d for all treatments within that subgroup_option.\n",
      "7. Return the result as a markdown table with the specified columns. Do not include any extra commentary.\n\n",
      "### Output columns:\n",
      "removed | removed_reason | study_id | study | trial | pub_year | outcome_type | outcome | subgroup | subgroup_option | grade | period | treatment | d | n | Event ratio in Subgroup_option+treatment | Total number of Events in subgroup_option"
    )
  })

  output$prompt_text <- renderPrint({
    cat(prompt_text())
  })

  # Copy to clipboard on button click
  observeEvent(input$generate_prompt, {
    txt <- prompt_text()
    if (txt == "") {
      showNotification("No prompt generated. Please fill in all required fields and paste text.", type = "error")
      return()
    }
    tryCatch({
      write_clip(txt)
      showNotification("Prompt copied to clipboard!", type = "success")
    }, error = function(e) {
      showNotification("Could not copy to clipboard. You can manually select and copy the text.",
                       type = "warning")
    })
  })
}

# ----------------------------------------------------------------------
# 4. Run app
# ----------------------------------------------------------------------
shinyApp(ui, server)
