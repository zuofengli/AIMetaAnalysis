# =============================================================================
# 02b_netmeta_all.R
# Single consistent frequentist framework (netmeta) across binary and survival
# outcomes: fixed (common) & random effects, P-scores, heterogeneity.
# Run with R 4.5.2 (netmeta 3.6-1 installed).
# =============================================================================

suppressPackageStartupMessages({
  library(netmeta)
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

logit <- function(p) log(p / (1 - p))

binary_to_pairwise <- function(arms) {
  arms <- arms %>% mutate(d = round(as.numeric(d)), n = round(as.numeric(n)))
  res <- arms %>%
    group_by(unique_id, trial, year) %>%
    summarise(a = d[arm == "exp"], b = n[arm == "exp"] - d[arm == "exp"],
              c = d[arm == "ctrl"], dd = n[arm == "ctrl"] - d[arm == "ctrl"],
              trt1 = trt[arm == "exp"], trt2 = trt[arm == "ctrl"],
              .groups = "drop") %>%
    mutate(
      # Haldane correction for zero cells
      a = a + (a == 0), b = b + (b == 0), c = c + (c == 0), dd = dd + (dd == 0),
      TE = log((a * dd) / (b * c)),
      seTE = sqrt(1 / a + 1 / b + 1 / c + 1 / dd),
      studlab = paste0(trial, " (", year, ")")
    ) %>%
    select(studlab, trt1, trt2, TE, seTE)
  res
}

run_one <- function(dat, sm, label, small_values = "bad") {
  nm <- netmeta(TE = TE, seTE = seTE, treat1 = trt1, treat2 = trt2,
                studlab = studlab, data = dat, sm = sm,
                reference.group = "Chemotherapy",
                common = TRUE, random = TRUE, warn = FALSE)
  trts <- nm$trts[!nm$trts %in% "Chemotherapy"]
  hr_or <- sapply(trts, function(t) exp(nm$TE.common[t, "Chemotherapy"]))
  lo <- sapply(trts, function(t) exp(nm$lower.common[t, "Chemotherapy"]))
  hi <- sapply(trts, function(t) exp(nm$upper.common[t, "Chemotherapy"]))
  hr_or_r <- sapply(trts, function(t) exp(nm$TE.random[t, "Chemotherapy"]))
  lo_r <- sapply(trts, function(t) exp(nm$lower.random[t, "Chemotherapy"]))
  hi_r <- sapply(trts, function(t) exp(nm$upper.random[t, "Chemotherapy"]))
  vs <- data.frame(
    outcome = label, treatment = trts,
    est_common = hr_or, lo_common = lo, hi_common = hi,
    est_random = hr_or_r, lo_random = lo_r, hi_random = hi_r,
    stringsAsFactors = FALSE)
  nr <- tryCatch(netrank(nm, common = TRUE, random = TRUE,
                         small.values = small_values),
                 error = function(e) NULL)
  psc_c <- if (!is.null(nr$Pscore.common)) nr$Pscore.common else
    if (!is.null(nr$Pscore.fixed)) nr$Pscore.fixed else NULL
  psc_r <- if (!is.null(nr$Pscore.random)) nr$Pscore.random else psc_c
  psc <- data.frame(
    outcome = label,
    treatment = names(psc_c),
    pscore_common = unname(psc_c),
    pscore_random = unname(psc_r),
    stringsAsFactors = FALSE)
  het <- data.frame(
    outcome = label,
    tau = nm$tau,
    Q = nm$Q, df_Q = nm$df.Q, pval_Q = nm$pval.Q,
    I2 = nm$I2)
  list(nm = nm, vs = vs, pscore = psc, het = het)
}

all_vs <- list(); all_ps <- list(); all_het <- list()

bin_cfg <- list(
  pcr = "bad", mpr = "bad", trae_any = "good", trae_g34 = "good",
  ae_any = "good", ae_g34 = "good")

for (nm_out in names(bin_cfg)) {
  arms <- read.csv(file.path(cur, paste0("arms_", nm_out, ".csv")),
                   stringsAsFactors = FALSE)
  dat <- binary_to_pairwise(arms)
  fit <- run_one(dat, "OR", nm_out, bin_cfg[[nm_out]])
  all_vs[[nm_out]] <- fit$vs; all_ps[[nm_out]] <- fit$pscore; all_het[[nm_out]] <- fit$het
  cat("done", nm_out, "\n")
}

for (nm_out in c("efs", "os")) {
  contr <- read.csv(file.path(cur, paste0("contrasts_", nm_out, ".csv")),
                    stringsAsFactors = FALSE)
  dat <- contr %>% mutate(
    studlab = paste0(trial, " (", year, ")"),
    TE = as.numeric(te), seTE = as.numeric(se_te),
    trt1 = treat1, trt2 = treat2) %>%
    select(studlab, trt1, trt2, TE, seTE)
  fit <- run_one(dat, "HR", nm_out, "good")
  all_vs[[nm_out]] <- fit$vs; all_ps[[nm_out]] <- fit$pscore; all_het[[nm_out]] <- fit$het
  cat("done", nm_out, "\n")
}

vs_all <- do.call(rbind, all_vs); rownames(vs_all) <- NULL
ps_all <- do.call(rbind, all_ps); rownames(ps_all) <- NULL
het_all <- do.call(rbind, all_het); rownames(het_all) <- NULL
write.csv(vs_all, file.path(out, "netmeta_vs_chemo_all.csv"), row.names = FALSE)
write.csv(ps_all, file.path(out, "netmeta_pscores_all.csv"), row.names = FALSE)
write.csv(het_all, file.path(out, "netmeta_heterogeneity_all.csv"), row.names = FALSE)
cat("All netmeta results saved.\n")
