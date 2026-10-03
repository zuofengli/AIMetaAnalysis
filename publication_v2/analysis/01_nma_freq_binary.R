# =============================================================================
# 01_nma_freq_binary.R
# Frequentist fixed-effect and REML NMA (Noma et al. NMA package) for the
# binary outcomes of the perioperative chemo-immunotherapy evidence base.
# Run with R 4.5.2 (package NMA available).
# =============================================================================

suppressPackageStartupMessages({
  library(NMA)
  library(dplyr)
})

# --- portable paths: resolved relative to this script ------------------------
script_dir <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f)) return(dirname(normalizePath(sub("^--file=", "", f[1]))))
  if (!is.null(sys.frame(1)$ofile)) return(dirname(normalizePath(sys.frame(1)$ofile)))
  normalizePath(getwd())
})

# --- Windows locale guard (see 00_curate.R) ----------------------------------
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE)
}
pub <- normalizePath(file.path(script_dir, ".."))
cur <- file.path(pub, "analysis/curated")
out <- file.path(pub, "analysis/results")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

armsets <- list(
  pcr      = "arms_pcr.csv",
  mpr      = "arms_mpr.csv",
  trae_any = "arms_trae_any.csv",
  trae_g34 = "arms_trae_g34.csv",
  ae_any   = "arms_ae_any.csv",
  ae_g34   = "arms_ae_g34.csv"
)

fmt_ci <- function(est, lo, hi, digits = 2) {
  paste0(formatC(est, digits = digits, format = "f"), " (",
         formatC(lo, digits = digits, format = "f"), ", ",
         formatC(hi, digits = digits, format = "f"), ")")
}

run_one <- function(arms, method, ref) {
  arms <- arms %>%
    mutate(study = paste0(trial, " (", year, ")"),
           trt = trt,
           d = as.numeric(d), n = as.numeric(n)) %>%
    select(study, trt, d, n)

  setup_obj <- NMA::setup(study = study, trt = trt, d = d, n = n,
                          measure = "OR", ref = ref, data = arms)
  res <- NMA::nma(setup_obj, method = method, eform = TRUE, digits = 6)
  res
}

extract_estimates <- function(res) {
  coding <- res$coding
  coding <- coding[order(coding$code), , drop = FALSE]
  coefm  <- res[["Coef. (vs. treat 1)"]]
  rown <- rownames(coefm)
  codes <- as.integer(sub(":.*$", "", rown))
  # coefficient rows are labelled by the numeric code of the treatment
  trt_by_code <- setNames(coding$treatment, coding$code)
  out <- data.frame(
    treatment = trt_by_code[as.character(codes)],
    est = as.numeric(coefm[, 1]),
    lo  = as.numeric(coefm[, 3]),
    hi  = as.numeric(coefm[, 4]),
    stringsAsFactors = FALSE
  )
  out
}

methods <- c(fixed = "fixed", reml = "REML")

for (nm in names(armsets)) {
  arms <- read.csv(file.path(cur, armsets[[nm]]), stringsAsFactors = FALSE)
  trts <- sort(unique(c(arms$trt, "Chemotherapy")))
  trts <- trts[trts != ""]
  for (m in names(methods)) {
    cat("Running", nm, m, "...\n")
    est_list <- lapply(trts, function(rf) {
      res <- run_one(arms, methods[[m]], rf)
      e <- extract_estimates(res)
      e$vs <- rf
      e
    })
    est <- do.call(rbind, est_list)
    # OR(trt vs vs); rows where trt == vs are identity = 1
    est <- rbind(est,
                 data.frame(treatment = trts, est = 1, lo = 1, hi = 1,
                            vs = trts, stringsAsFactors = FALSE))
    est$outcome <- nm; est$method <- m
    write.csv(est, file.path(out, sprintf("freq_%s_%s_pairs.csv", nm, m)),
              row.names = FALSE)
    # heterogeneity diagnostics
    res <- run_one(arms, methods[[m]], "Chemotherapy")
    cat("  Q: ", res[["Test for Heterogeneity"]], "\n")
    if ("tau" %in% names(res)) cat("  tau: ", res$tau, "\n")
  }
}

cat("Done.\n")
