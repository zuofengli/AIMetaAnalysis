#' Launch the Meta-Analysis Shiny Application
#'
#' This function starts the Shiny application for meta-analysis of pCR in NSCLC trials.
#' @export
runMetaAnalysis <- function() {
  app_dir <- system.file("app", package = "AIMetaAnalysis")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing `AlMetaAnalysis`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

#' Launch the Data Extraction Assistant App
#'
#' This Shiny app helps users extract subgroup data from clinical trial publications
#' by generating a structured prompt for LLM, then copying the prompt to clipboard.
#' The app reads study metadata from `clinical_studies_data.xlsx` (studies_info sheet)
#' and allows users to input baseline counts and outcome rates from Table 1 and
#' subgroup analysis tables. It then produces a prompt that instructs the LLM to
#' return arm-level data ready for manual copying into the Excel data file.
#'
#' @export
runBinaryDataExtraction <- function() {
  app_dir <- system.file("app","app_binary_data_extraction", package = "AIMetaAnalysis")
  if (app_dir == "") {
    stop("Could not find binary data extraction app directory. Try re-installing the package.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
