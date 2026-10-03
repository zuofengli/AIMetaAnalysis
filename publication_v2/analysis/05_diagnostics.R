# =============================================================================
# 05_diagnostics.R
# Convergence diagnostics (Rhat / effective sample size), model fit (DIC),
# and dual-paradigm agreement statistics for the fixed-effect Bayesian NMAs.
# Also runs a rounding sensitivity analysis for pCR / MPR (integer rounding
# of reconstructed non-integer counts) through the same netmeta pipeline.
# Run with R 4.5.2.
# =============================================================================

suppressPackageStartupMessages({
  library(gemtc)
  library(rjags)
  library(coda)
  library(dplyr)
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
base <- normalizePath(file.path(script_dir, ".."))
cur  <- file.path(base, "analysis/curated")
out  <- file.path(base, "analysis/results")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Fixed RNG seed (keep identical to 03_nma_bayes.R for stable diagnostics).
SEED <- as.integer(Sys.getenv("BAYES_SEED", "20250904"))
set.seed(SEED)

# --- deterministic JAGS RNG (identical rationale to 03_nma_bayes.R) ----------
# set.seed() controls only the initial values drawn in R; the JAGS sampler RNG
# is seeded by JAGS itself from the clock. Attaching .RNG.name/.RNG.seed to
# every chain makes gelman.diag()/effectiveSize() stable across reruns.
local({
  ns <- asNamespace("gemtc")
  orig <- get("mtc.build.syntaxModel", envir = ns)
  patched <- function(model, ...) {
    syn <- orig(model, ...)
    for (i in seq_along(syn$inits)) {
      syn$inits[[i]][[".RNG.name"]] <- "base::Wichmann-Hill"
      syn$inits[[i]][[".RNG.seed"]] <- as.integer(SEED + i)
    }
    syn
  }
  unlockBinding("mtc.build.syntaxModel", ns)
  assign("mtc.build.syntaxModel", patched, envir = ns)
  lockBinding("mtc.build.syntaxModel", ns)
})

ref_trt <- "Chemotherapy"

build_network_binary <- function(arms) {
  data_ab <- arms %>%
    mutate(study = paste0(trial, " (", year, ")"),
           treatment = trt,
           responders = round(as.numeric(d)),
           sampleSize = round(as.numeric(n))) %>%
    filter(!is.na(treatment), !is.na(responders), !is.na(sampleSize),
           responders >= 0, sampleSize > 0, responders <= sampleSize) %>%
    select(study, treatment, responders, sampleSize)
  treatments <- data.frame(id = sort(unique(data_ab$treatment)),
                           description = sort(unique(data_ab$treatment)),
                           stringsAsFactors = FALSE)
  net <- mtc.network(data.ab = data_ab, treatments = treatments)
  list(net = net, data_ab = data_ab, treatments = treatments)
}

build_network_survival <- function(contr) {
  rows <- lapply(seq_len(nrow(contr)), function(i) {
    rbind(
      data.frame(study = paste0(contr$trial[i], " (", contr$year[i], ")"),
                 treatment = contr$treat2[i], diff = NA_real_,
                 std.err = NA_real_, stringsAsFactors = FALSE),
      data.frame(study = paste0(contr$trial[i], " (", contr$year[i], ")"),
                 treatment = contr$treat1[i],
                 diff = as.numeric(contr$te[i]),
                 std.err = as.numeric(contr$se_te[i]),
                 stringsAsFactors = FALSE))
  })
  data_re <- do.call(rbind, rows)
  treatments <- data.frame(id = sort(unique(c(contr$treat1, contr$treat2))),
                           description = sort(unique(c(contr$treat1,
                                                       contr$treat2))),
                           stringsAsFactors = FALSE)
  net <- mtc.network(data.re = data_re, treatments = treatments)
  list(net = net, data_re = data_re, treatments = treatments)
}

fit_fixed <- function(net_obj, likelihood = "binom", link = "logit") {
  model <- mtc.model(network = net_obj$net, type = "consistency",
                     likelihood = likelihood, link = link,
                     linearModel = "fixed", n.chain = 4)
  mtc.run(model, sampler = "rjags", n.adapt = 5000, n.iter = 25000,
          thin = 5)
}

summarise_diag <- function(fit, label) {
  sm <- fit$samples
  cols <- grep("^d\\.", varnames(sm), value = TRUE)
  sub <- sm[, cols, drop = TRUE]
  gd <- tryCatch(gelman.diag(sub, autoburnin = FALSE), error = function(e) NULL)
  if (!is.null(gd)) {
    rhat <- gd$psrf[, 1]
  } else {
    rhat <- NA_real_
  }
  ess <- effectiveSize(sub)
  dic <- tryCatch({
    d <- mtc.dic(fit)
    c(dic = d$dic, pD = d$pD)
  }, error = function(e) c(dic = NA_real_, pD = NA_real_))
  data.frame(model = label,
             n_pars = length(cols),
             rhat_max = ifelse(all(is.na(rhat)), NA, max(rhat, na.rm = TRUE)),
             rhat_gt_1_05 = ifelse(all(is.na(rhat)), NA,
                                   sum(rhat > 1.05, na.rm = TRUE)),
             worst_par = ifelse(all(is.na(rhat)), NA,
                                names(rhat)[which.max(rhat)]),
             ess_min = min(ess),
             ess_gt_200 = sum(ess > 200),
             dic = dic[["dic"]], pD = dic[["pD"]],
             stringsAsFactors = FALSE)
}

binary_cfg <- c(pcr = "arms_pcr.csv", mpr = "arms_mpr.csv",
                trae_any = "arms_trae_any.csv", trae_g34 = "arms_trae_g34.csv",
                ae_any = "arms_ae_any.csv", ae_g34 = "arms_ae_g34.csv")

diag_rows <- list()
for (nm in names(binary_cfg)) {
  arms <- read.csv(file.path(cur, binary_cfg[[nm]]), stringsAsFactors = FALSE)
  net_obj <- build_network_binary(arms)
  cat("Diagnostics:", nm, "...\n")
  fit <- fit_fixed(net_obj)
  diag_rows[[nm]] <- summarise_diag(fit, nm)
}
for (nm in c("efs", "os")) {
  contr <- read.csv(file.path(cur, paste0("contrasts_", nm, ".csv")),
                    stringsAsFactors = FALSE)
  net_obj <- build_network_survival(contr)
  cat("Diagnostics:", nm, "...\n")
  fit <- fit_fixed(net_obj, likelihood = "normal", link = "identity")
  diag_rows[[nm]] <- summarise_diag(fit, nm)
}
diag_all <- do.call(rbind, diag_rows)
rownames(diag_all) <- NULL
write.csv(diag_all, file.path(out, "bayes_convergence_fixed.csv"),
          row.names = FALSE)
print(diag_all)

# ---------------------------------------------------------------------------
# Agreement between frequentist P-scores and Bayesian SUCRA
# ---------------------------------------------------------------------------
ps <- read.csv(file.path(out, "netmeta_pscores_all.csv"),
               stringsAsFactors = FALSE)
su_files <- list.files(out, pattern = "bayes_.*_fixed_sucra\\.csv$",
                       full.names = TRUE)
su <- lapply(su_files, read.csv, stringsAsFactors = FALSE) %>% bind_rows()
agr <- ps %>% filter(treatment != "Chemotherapy") %>%
  select(outcome, treatment, ps = pscore_common) %>%
  inner_join(su %>% filter(treatment != "Chemotherapy") %>%
               select(outcome, treatment, sucra), by = c("outcome", "treatment"))
rank_cor <- agr %>% group_by(outcome) %>%
  summarise(n_agents = n(),
            spearman = cor(ps, sucra, method = "spearman"),
            pearson = cor(ps, sucra, method = "pearson"),
            mean_abs_diff = mean(abs(ps - sucra)),
            max_abs_diff = max(abs(ps - sucra)),
            .groups = "drop")
write.csv(rank_cor, file.path(out, "paradigm_agreement.csv"), row.names = FALSE)
print(rank_cor)

# ---------------------------------------------------------------------------
# Rounding sensitivity for pCR / MPR (reconstructed fractional counts)
# ---------------------------------------------------------------------------
binary_to_pairwise <- function(arms) {
  arms %>% mutate(d = round(as.numeric(d)), n = round(as.numeric(n))) %>%
    group_by(unique_id, trial, year) %>%
    summarise(a = d[arm == "exp"], b = n[arm == "exp"] - d[arm == "exp"],
              c = d[arm == "ctrl"], dd = n[arm == "ctrl"] - d[arm == "ctrl"],
              trt1 = trt[arm == "exp"], trt2 = trt[arm == "ctrl"],
              .groups = "drop") %>%
    mutate(a = a + (a == 0), b = b + (b == 0),
           c = c + (c == 0), dd = dd + (dd == 0),
           TE = log((a * dd) / (b * c)),
           seTE = sqrt(1 / a + 1 / b + 1 / c + 1 / dd),
           studlab = paste0(trial, " (", year, ")")) %>%
    select(studlab, trt1, trt2, TE, seTE)
}

sens_rows <- list()
for (nm in c("pcr", "mpr")) {
  arms <- read.csv(file.path(cur, paste0("arms_", nm, ".csv")),
                   stringsAsFactors = FALSE)
  dat <- binary_to_pairwise(arms)
  nmfit <- netmeta(TE = TE, seTE = seTE, treat1 = trt1, treat2 = trt2,
                   studlab = studlab, data = dat, sm = "OR",
                   reference.group = ref_trt, common = TRUE, random = TRUE)
  trts <- nmfit$trts[nmfit$trts != ref_trt]
  orig <- read.csv(file.path(out, "netmeta_vs_chemo_all.csv"),
                   stringsAsFactors = FALSE) %>%
    filter(outcome == nm, treatment != ref_trt)
  sens_rows[[nm]] <- data.frame(
    outcome = nm, treatment = trts,
    rounded_est = sapply(trts, function(t) exp(nmfit$TE.common[t, ref_trt])),
    rounded_lo = sapply(trts, function(t) exp(nmfit$lower.common[t, ref_trt])),
    rounded_hi = sapply(trts, function(t) exp(nmfit$upper.common[t, ref_trt])),
    original_est = orig$est_common[match(trts, orig$treatment)],
    stringsAsFactors = FALSE) %>%
    mutate(ratio = rounded_est / original_est)
}
sens <- do.call(rbind, sens_rows); rownames(sens) <- NULL
write.csv(sens, file.path(out, "rounding_sensitivity.csv"), row.names = FALSE)
print(sens)

cat("Diagnostics and agreement statistics saved.\n")
