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
