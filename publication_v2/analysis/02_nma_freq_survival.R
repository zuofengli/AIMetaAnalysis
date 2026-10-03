# =============================================================================
# 02_nma_freq_survival.R
# Frequentist (contrast-based) NMA for EFS and OS using netmeta.
# Run with R 4.3.0 (netmeta available in its library).
# =============================================================================

suppressPackageStartupMessages({
  library(netmeta)
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

run_netmeta <- function(outcome_name, file) {
  dat <- read.csv(file.path(cur, file), stringsAsFactors = FALSE)
  dat$TE <- as.numeric(dat$te)
  dat$seTE <- as.numeric(dat$se_te)
  dat$studlab <- paste0(dat$trial, " (", dat$year, ")")
  nm <- netmeta(TE = TE, seTE = seTE, treat1 = treat1, treat2 = treat2,
                studlab = studlab, data = dat, sm = "HR",
                reference.group = "Chemotherapy",
                common = TRUE, random = TRUE, warn = FALSE)

  trts <- nm$trts
  trts <- trts[trts != "Chemotherapy"]
  refs <- rep("Chemotherapy", length(trts))
  rows_common <- data.frame(
    outcome = outcome_name, model = "common",
    treatment = trts, vs = refs,
    est = as.numeric(nm$TE.common["Chemotherapy", trts]) * NA + as.numeric(
      sapply(trts, function(t) nm$TE.common["Chemotherapy", t])),
    stringsAsFactors = FALSE
  )
  # netmeta stores log effect of the ROW treatment vs the COLUMN treatment in
  # TE/lower/upper matrices.  HR(trt vs chemotherapy) = exp(TE[trt, chemo]).
  hr_c <- sapply(trts, function(t) exp(nm$TE.common[t, "Chemotherapy"]))
  lo_c <- sapply(trts, function(t) exp(nm$lower.common[t, "Chemotherapy"]))
  hi_c <- sapply(trts, function(t) exp(nm$upper.common[t, "Chemotherapy"]))
  hr_r <- sapply(trts, function(t) exp(nm$TE.random[t, "Chemotherapy"]))
  lo_r <- sapply(trts, function(t) exp(nm$lower.random[t, "Chemotherapy"]))
  hi_r <- sapply(trts, function(t) exp(nm$upper.random[t, "Chemotherapy"]))

  resdf <- rbind(
    data.frame(outcome = outcome_name, model = "common", treatment = trts,
               vs = "Chemotherapy", est = hr_c, lo = lo_c, hi = hi_c),
    data.frame(outcome = outcome_name, model = "random", treatment = trts,
               vs = "Chemotherapy", est = hr_r, lo = lo_r, hi = hi_r)
  )
  row.names(resdf) <- NULL

  # full pair matrix (upper triangle = HR(column vs row))
  pairs <- list()
  for (model in c("common", "random")) {
    mat <- if (model == "common") nm$TE.common else nm$TE.random
    lmat <- if (model == "common") nm$lower.common else nm$lower.random
    umat <- if (model == "common") nm$upper.common else nm$upper.random
    tr_all <- nm$trts
    cells <- list()
    for (a in seq_along(tr_all)) {
      for (b in seq_along(tr_all)) {
        if (a == b) next
        cells[[length(cells) + 1]] <- data.frame(
          row = tr_all[a], col = tr_all[b],
          est = exp(mat[tr_all[b], tr_all[a]]),
          lo  = exp(lmat[tr_all[b], tr_all[a]]),
          hi  = exp(umat[tr_all[b], tr_all[a]]),
          stringsAsFactors = FALSE)
      }
    }
    p <- do.call(rbind, cells)
    p$model <- model
    pairs[[model]] <- p
  }
  pairdf <- do.call(rbind, pairs)
  pairdf$outcome <- outcome_name

  write.csv(resdf, file.path(out, sprintf("freq_%s_vs_chemo.csv", outcome_name)),
            row.names = FALSE)
  write.csv(pairdf, file.path(out, sprintf("freq_%s_pairs.csv", outcome_name)),
            row.names = FALSE)

  cat("\n=====", outcome_name, "=====\n")
  Qtxt <- tryCatch(
    paste0("Q = ", round(nm$Q, 2), " (df = ", nm$df.Q, ", p = ",
           signif(nm$pval.Q, 3), ")"),
    error = function(e) "not reported")
  cat("Heterogeneity:", Qtxt, "\n")
  cat("tau:", nm$tau, "\n")
  print(resdf[, c("model", "treatment", "est", "lo", "hi")])
  invisible(list(nm = nm, out = resdf))
}

run_netmeta("efs", "contrasts_efs.csv")
run_netmeta("os", "contrasts_os.csv")
cat("\nDone.\n")
