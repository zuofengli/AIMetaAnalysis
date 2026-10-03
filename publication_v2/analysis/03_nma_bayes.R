# =============================================================================
# 03_nma_bayes.R
# Bayesian consistency NMA (gemtc / rjags) for the perioperative
# chemo-immunotherapy evidence base. MCMC settings mirror the AIMetaAnalysis
# application: 4 chains, 5000 adaptation, 25 000 iterations, thinning 5.
# Run with R 4.5.2.
# =============================================================================

suppressPackageStartupMessages({
  library(gemtc)
  library(rjags)
  library(coda)
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

# Fixed RNG seed so the MCMC results are reproducible across reruns.
# Override by exporting BAYES_SEED before running the script.
SEED <- as.integer(Sys.getenv("BAYES_SEED", "20250904"))
set.seed(SEED)

# --- deterministic JAGS RNG --------------------------------------------------
# set.seed() alone is NOT sufficient: gemtc draws the *initial values* from R's
# RNG, but the JAGS sampler RNG is seeded by JAGS itself (from the clock) and
# therefore differs on every run. Attaching an explicit .RNG.name/.RNG.seed to
# every chain fixes this. gemtc::mtc.run() gives no way to pass `inits`, so we
# wrap the internal syntax builder -- everything else (DIC, monitors) is left
# untouched.
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
  dat <- contr
  rows <- lapply(seq_len(nrow(dat)), function(i) {
    base <- data.frame(study = paste0(dat$trial[i], " (", dat$year[i], ")"),
                       treatment = dat$treat2[i],
                       diff = NA_real_, std.err = NA_real_,
                       stringsAsFactors = FALSE)
    exp <- data.frame(study = paste0(dat$trial[i], " (", dat$year[i], ")"),
                      treatment = dat$treat1[i],
                      diff = as.numeric(dat$te[i]),
                      std.err = as.numeric(dat$se_te[i]),
                      stringsAsFactors = FALSE)
    rbind(base, exp)
  })
  data_re <- do.call(rbind, rows)
  treatments <- data.frame(id = sort(unique(c(dat$treat1, dat$treat2))),
                           description = sort(unique(c(dat$treat1, dat$treat2))),
                           stringsAsFactors = FALSE)
  net <- mtc.network(data.re = data_re, treatments = treatments)
  list(net = net, data_re = data_re, treatments = treatments)
}

run_bayes <- function(net_obj, linear_model, likelihood = "binom", link = "logit",
                      save_samples = FALSE, run_label = "") {
  model_args <- list(network = net_obj$net, type = "consistency",
                     likelihood = likelihood, link = link,
                     linearModel = linear_model, n.chain = 4)
  if (linear_model == "random") {
    model_args$hy.prior <- mtc.hy.prior("std.dev", "dunif", 0, 5)
  }
  model <- do.call(mtc.model, model_args)
  result <- mtc.run(model, sampler = "rjags",
                    n.adapt = 5000, n.iter = 25000, thin = 5)
  if (save_samples) {
    saveRDS(result, file.path(out, paste0("bayes_samples_", run_label, ".rds")))
  }
  result
}

pair_estimate <- function(mcmc_result, t1, t2) {
  rel <- relative.effect(mcmc_result, t1 = t1, t2 = t2)
  sm <- summary(rel)
  qs <- sm$summaries$quantiles
  data.frame(treatment = t2, vs = t1,
             med = exp(qs["50%"]), lo = exp(qs["2.5%"]), hi = exp(qs["97.5%"]),
             stringsAsFactors = FALSE)
}

summarise_pairs <- function(mcmc_result, treatments, measure, all_pairs = TRUE) {
  trts <- treatments$id
  cells <- list()
  for (t in trts) {
    if (t == ref_trt) next
    cells[[t]] <- pair_estimate(mcmc_result, ref_trt, t)
  }
  if (all_pairs) {
    for (a in trts) {
      for (b in trts) {
        if (a >= b) next
        cells[[paste(a, b)]] <- pair_estimate(mcmc_result, a, b)
      }
    }
  }
  res <- do.call(rbind, cells)
  res$measure <- measure
  rownames(res) <- NULL
  res
}

compute_sucra <- function(mcmc_result, preferredDirection) {
  rp <- rank.probability(mcmc_result, preferredDirection = preferredDirection)
  trts <- rownames(rp)
  ntrt <- ncol(rp)
  sc <- apply(rp, 1, function(p) {
    cdf <- cumsum(p)
    sum(cdf[seq_len(ntrt - 1)]) / (ntrt - 1)
  })
  data.frame(treatment = names(sc), sucra = unname(sc), stringsAsFactors = FALSE)
}

binary_outcomes <- list(
  pcr      = list(file = "arms_pcr.csv",       direction =  1, random = TRUE),
  mpr      = list(file = "arms_mpr.csv",       direction =  1, random = TRUE),
  trae_any = list(file = "arms_trae_any.csv",  direction = -1, random = TRUE),
  trae_g34 = list(file = "arms_trae_g34.csv",  direction = -1, random = FALSE),
  ae_any   = list(file = "arms_ae_any.csv",    direction = -1, random = FALSE),
  ae_g34   = list(file = "arms_ae_g34.csv",    direction = -1, random = FALSE)
)

args <- commandArgs(TRUE)
only <- if (length(args) >= 1) args[1] else NULL

for (nm in names(binary_outcomes)) {
  if (!is.null(only) && nm != only) next
  cfg <- binary_outcomes[[nm]]
  arms <- read.csv(file.path(cur, cfg$file), stringsAsFactors = FALSE)
  net_obj <- build_network_binary(arms)
  for (lm in c("fixed", if (cfg$random) "random")) {
    label <- paste0(nm, "_", lm)
    cat("Bayes", label, "...\n")
    fit <- run_bayes(net_obj, lm, likelihood = "binom", link = "logit",
                     save_samples = lm == "fixed" && nm %in% c("pcr", "mpr"),
                     run_label = label)
    pairs <- summarise_pairs(fit, net_obj$treatments, if (cfg$direction == 1) "OR" else "OR")
    pairs$outcome <- nm; pairs$model <- lm
    write.csv(pairs, file.path(out, sprintf("bayes_%s_%s_pairs.csv", nm, lm)),
              row.names = FALSE)
    sc <- compute_sucra(fit, cfg$direction)
    sc$outcome <- nm; sc$model <- lm
    write.csv(sc, file.path(out, sprintf("bayes_%s_%s_sucra.csv", nm, lm)),
              row.names = FALSE)
    cat("  done", label, "\n")
  }
}

survival_outcomes <- list(
  efs = list(file = "contrasts_efs.csv", direction = -1),
  os  = list(file = "contrasts_os.csv",  direction = -1)
)

for (nm in names(survival_outcomes)) {
  if (!is.null(only) && nm != only) next
  cfg <- survival_outcomes[[nm]]
  contr <- read.csv(file.path(cur, cfg$file), stringsAsFactors = FALSE)
  net_obj <- build_network_survival(contr)
  for (lm in c("fixed", "random")) {
    label <- paste0(nm, "_", lm)
    cat("Bayes", label, "...\n")
    fit <- run_bayes(net_obj, lm, likelihood = "normal", link = "identity",
                     save_samples = lm == "fixed",
                     run_label = label)
    pairs <- summarise_pairs(fit, net_obj$treatments, "HR")
    pairs$outcome <- nm; pairs$model <- lm
    write.csv(pairs, file.path(out, sprintf("bayes_%s_%s_pairs.csv", nm, lm)),
              row.names = FALSE)
    sc <- compute_sucra(fit, cfg$direction)
    sc$outcome <- nm; sc$model <- lm
    write.csv(sc, file.path(out, sprintf("bayes_%s_%s_sucra.csv", nm, lm)),
              row.names = FALSE)
    cat("  done", label, "\n")
  }
}

cat("Bayesian analyses complete.\n")
