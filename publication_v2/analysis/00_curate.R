# =============================================================================
# 00_curate.R
# Build curated, analysis-ready datasets for the dual-paradigm (frequentist +
# Bayesian) network meta-analysis of perioperative chemo-immunotherapy for
# resectable NSCLC.
#
# Source: AIMetaAnalysis inst/extdata/clinical_studies_data.xlsx
# Strategy labels mirror the AIMetaAnalysis Shiny app (agent-level mapping via
# studies_info$Experimental / $Comparator).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})

# --- portable paths: everything is resolved relative to this script ---------
script_dir <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f)) return(dirname(normalizePath(sub("^--file=", "", f[1]))))
  if (!is.null(sys.frame(1)$ofile)) return(dirname(normalizePath(sys.frame(1)$ofile)))
  normalizePath(getwd())
})

# --- Windows locale guard: a bogus LC_ALL (e.g. "C.UTF-8" exported by some
# shells) makes R fall back to the C locale, which silently strips the high
# bit off non-ASCII path bytes (e.g. ".../办公/...") when talking to the OS.
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE)
}
this_dir  <- script_dir                                  # .../publication_v2/analysis
pub_dir   <- normalizePath(file.path(this_dir, ".."))    # .../publication_v2
root      <- normalizePath(file.path(pub_dir, ".."))     # .../AIMetaAnalysis (package root)
data_file <- file.path(root, "inst/extdata/clinical_studies_data.xlsx")
outdir    <- file.path(pub_dir, "analysis/curated")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
cat("[00_curate] root      :", root, "\n")
cat("[00_curate] data_file :", data_file, "\n")
cat("[00_curate] outdir    :", outdir, "\n")

# --- registry: which trial publication feeds which endpoint ------------------
# Edit analysis/trial_registry.R (NOT this file) when adding a study.
source(file.path(this_dir, "trial_registry.R"))

# --- treatment dictionary: canonical names, display order, accepted spellings -
trt_dict  <- read.csv(file.path(this_dir, "config/treatments.csv"),
                      stringsAsFactors = FALSE, check.names = FALSE)
alias_map <- local({
  a <- character(0)
  for (i in seq_len(nrow(trt_dict))) {
    al <- trt_dict$aliases[i]
    if (!is.na(al) && nzchar(al)) {
      for (x in strsplit(al, ";")[[1]]) a[trimws(x)] <- trt_dict$treatment[i]
    }
  }
  a
})
normalise_trt <- function(x) {
  x <- trimws(x)
  ifelse(x %in% names(alias_map), unname(alias_map[x]), x)
}

studies_info <- read_excel(data_file, sheet = "studies_info") %>%
  rename(year = pub_year) %>%
  mutate(year = as.integer(year))

binary_data <- read_excel(data_file, sheet = "binary_data") %>%
  rename(year = pub_year) %>%
  filter(is.na(removed) | removed != "YES")

survival_data <- read_excel(data_file, sheet = "survival") %>%
  rename(year = pub_year) %>%
  filter(is.na(removed) | removed != "YES")

si_join <- studies_info %>% select(unique_id, study = trial, year, article_id)
binary_data  <- binary_data  %>% left_join(si_join, by = c("trial" = "study", "year", "article_id"))
survival_data<- survival_data %>% left_join(si_join, by = c("trial" = "study", "year", "article_id"))

# Treatment mapping -----------------------------------------------------------
# The agent-level ("Huaxi-Mei harmonised") mapping labels each experimental arm
# by ICI drug and every control arm as "Chemotherapy"; this mirrors the mapping
# used to generate the league tables currently deposited in the manuscript
# folder (agent names incl. Durvalumab, Nivolumab, Pembrolizumab, Tislelizumab,
# Toripalimab, Camrelizumab).
map_exp <- studies_info %>%
  mutate(trt = ifelse(is.na(Experimental_Test_Huaxi_Mei) | Experimental_Test_Huaxi_Mei == "",
                      Experimental, Experimental_Test_Huaxi_Mei)) %>%
  select(unique_id, trt) %>% filter(!is.na(trt), trt != "")
map_ctrl <- studies_info %>%
  mutate(trt = ifelse(is.na(Comparator_Test_HuaXi_Mei) | Comparator_Test_HuaXi_Mei == "",
                      Comparator, Comparator_Test_HuaXi_Mei)) %>%
  select(unique_id, trt) %>% filter(!is.na(trt), trt != "")

agent_map <- bind_rows(
  map_exp %>% mutate(arm = "exp"),
  map_ctrl %>% mutate(arm = "ctrl")
) %>% distinct() %>%
  mutate(trt = normalise_trt(trt))

# Preferred publication records (unique_id) per endpoint -----------------------
# All endpoint membership lives in analysis/trial_registry.R (sourced above):
#   ids_histo, ids_trae_any, ids_ae_any, ids_ae_g34,
#   expected_trae_any, expected_trae_g34, efs_keys, os_keys, trial_chars_order

make_binary_armset <- function(outcome, allowed_ids, grade_rule = NULL,
                               subgroup_candidates = NULL) {
  dat <- binary_data %>%
    filter(outcome == !!outcome, unique_id %in% allowed_ids)
  if (!is.null(subgroup_candidates)) {
    dat <- dat %>% filter(subgroup %in% subgroup_candidates)
  } else {
    dat <- dat %>% filter(subgroup == "Overall")
  }
  if (!is.null(grade_rule)) dat <- grade_rule(dat)
  dat <- dat %>%
    mutate(arm = if_else(grepl("_ctrl$", treatment), "ctrl", "exp")) %>%
    group_by(unique_id, trial, year, arm) %>%
    summarise(d = sum(d, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop")
  out <- dat %>%
    left_join(agent_map, by = c("unique_id", "arm")) %>%
    select(unique_id, trial, year, arm, trt, d, n) %>%
    arrange(unique_id, arm)
  out
}

# --- pCR / MPR (Overall rows) ------------------------------------------------
pcr <- make_binary_armset("PathologicalCompleteRespons", ids_histo)
mpr <- make_binary_armset("MajorPathologicResponse", ids_histo)

# --- TRAE totals ---------------------------------------------------------------
# In the source workbook TRAE totals appear under different outcome/subgroup
# conventions.  To stay faithful to the deposited league tables we match the
# exact published event counts.  The pinned counts (expected_trae_any /
# expected_trae_g34) are defined in analysis/trial_registry.R.

match_expected <- function(dat, expected, outcomes, subgroups) {
  rows <- lapply(seq_len(nrow(expected)), function(i) {
    uid <- expected$unique_id[i]; de <- expected$d_exp[i]
    cand <- dat %>%
      filter(outcome %in% outcomes, unique_id == uid,
             subgroup %in% subgroups,
             is.na(period) | period == "Overall")
    exp_row <- cand %>% filter(!grepl("_ctrl$", treatment)) %>%
      filter(abs(d - de) < 0.6)
    ctrl_row <- cand %>% filter(grepl("_ctrl$", treatment)) %>%
      filter(abs(d - expected$d_ctrl[i]) < 0.6)
    if (nrow(exp_row) == 0) exp_row <- cand %>% filter(!grepl("_ctrl$", treatment)) %>% head(1)
    if (nrow(ctrl_row) == 0) ctrl_row <- cand %>% filter(grepl("_ctrl$", treatment)) %>% head(1)
    if (nrow(exp_row) == 0 || nrow(ctrl_row) == 0) return(NULL)
    bind_rows(exp_row[1, , drop = FALSE], ctrl_row[1, , drop = FALSE])
  })
  rows <- Filter(Negate(is.null), rows)
  bind_rows(rows)
}

trae_any_raw <- match_expected(
  binary_data, expected_trae_any,
  outcomes = c("TreatmentRelatedAdverseEvents", "AdverseEvents"),
  subgroups = c("All", "treatment_related_any_grade"))
trae_any <- trae_any_raw %>%
  mutate(arm = if_else(grepl("_ctrl$", treatment), "ctrl", "exp")) %>%
  group_by(unique_id, trial, year, arm) %>%
  summarise(d = sum(d), n = sum(n), .groups = "drop") %>%
  left_join(agent_map, by = c("unique_id", "arm")) %>%
  select(unique_id, trial, year, arm, trt, d, n)

trae_g34_raw <- match_expected(
  binary_data, expected_trae_g34,
  outcomes = c("TreatmentRelatedAdverseEvents", "AdverseEvents"),
  subgroups = c("All", "treatment_related_GradeMoreEqualThan_3"))
trae_g34 <- trae_g34_raw %>%
  mutate(arm = if_else(grepl("_ctrl$", treatment), "ctrl", "exp")) %>%
  group_by(unique_id, trial, year, arm) %>%
  summarise(d = sum(d), n = sum(n), .groups = "drop") %>%
  left_join(agent_map, by = c("unique_id", "arm")) %>%
  select(unique_id, trial, year, arm, trt, d, n)

# --- Any-cause AE any grade ---------------------------------------------------
ae_any <- make_binary_armset(
  "AdverseEvents", ids_ae_any,
  grade_rule = function(dat) {
    dat %>%
      filter(subgroup == "any_cause_any_grade", period == "Overall")
  },
  subgroup_candidates = "any_cause_any_grade")

# --- Any-cause AE grade >=3 ----------------------------------------------------
ae_g34 <- make_binary_armset(
  "AdverseEvents", ids_ae_g34,
  grade_rule = function(dat) {
    dat %>%
      filter(period == "Overall") %>%
      mutate(grade_pref = case_when(
        trial == "KEYNOTE-671" ~ if_else(subgroup == "any_cause_GradeMoreEqualThan_3", 1, 2),
        trial == "RATIONALE-315" ~ if_else(subgroup %in% c("any_cause_GradeMoreEqualThan_3",
                                                            "any_caluse_GradeMoreEqualThan_3"), 1, 2),
        TRUE ~ if_else(subgroup == "any_cause_Grade3or4", 1, 2)
      )) %>%
      group_by(unique_id, treatment) %>%
      slice_min(grade_pref, n = 1, with_ties = FALSE) %>%
      ungroup()
  },
  subgroup_candidates = c("any_cause_Grade3or4", "any_cause_GradeMoreEqualThan_3",
                          "any_caluse_GradeMoreEqualThan_3"))

# --- Survival contrast datasets ------------------------------------------------
sel_survival_record <- function(dat, key) {
  # picks one overall row per trial given ordered preferred keys (record, hr)
  out <- list()
  for (k in key) {
    hit <- dat %>% filter(unique_id == k$id)
    if (is.null(k$hr)) {
      if (nrow(hit) > 0) hit <- hit[1, , drop = FALSE]
    } else {
      tol <- 0.005
      hit <- hit %>% filter(abs(hr - k$hr) < tol)
      if (nrow(hit) > 0) hit <- hit[1, , drop = FALSE]
    }
    if (nrow(hit) > 0) {
      out[[k$id]] <- hit
      break
    }
  }
  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

# efs_keys / os_keys (which survival record and which HR) are defined in
# analysis/trial_registry.R.

make_survival_contrasts <- function(outcome, keys) {
  rows <- lapply(keys, function(k) {
    dat <- survival_data %>%
      filter(outcome == !!outcome, subgroup == "Overall", unique_id == k$id)
    if (nrow(dat) == 0) return(NULL)
    if (!is.null(k$hr)) {
      dat <- dat %>% filter(abs(hr - k$hr) < 0.005)
    }
    if (nrow(dat) == 0) return(NULL)
    dat[1, , drop = FALSE]
  })
  rows <- Filter(Negate(is.null), rows)
  dat <- bind_rows(rows)
  dat <- dat %>%
    mutate(
      # readxl types a whole column as character when even one cell holds a
      # non-numeric token; the survival sheet has one legacy row whose upper CI
      # is the literal text "Inf".  Coerce explicitly, so that log() always sees
      # numbers and a record with a missing `se_te` cannot abort the run
      # (base::ifelse only forces `yes` when at least one test value is TRUE,
      # which previously masked this).
      hr     = as.numeric(hr),
      hr_lci = as.numeric(hr_lci),
      hr_uci = as.numeric(hr_uci),
      te     = as.numeric(te),
      se_te  = as.numeric(se_te),
      te    = ifelse(is.na(te), log(hr), te),
      se_te = ifelse(is.na(se_te), (log(hr_uci) - log(hr_lci)) / (2 * qnorm(0.975)), se_te)
    ) %>%
    select(-treat1, -treat2) %>%
    left_join(agent_map %>% filter(arm == "exp") %>% select(unique_id, treat1 = trt), by = "unique_id") %>%
    left_join(agent_map %>% filter(arm == "ctrl") %>% select(unique_id, treat2 = trt), by = "unique_id") %>%
    select(unique_id, trial, year, treat1, treat2, hr, hr_lci, hr_uci, te, se_te, n1, n2) %>%
    distinct()
  dat
}

efs <- make_survival_contrasts("EventFreeSurvival", efs_keys)
os  <- make_survival_contrasts("OverallSurvival", os_keys)

# IFCT-0002 anchor (neoadjuvant chemo vs perioperative chemo incl. adjuvant
# chemotherapy), used only for the treatment-architecture sensitivity analysis.
ifct <- survival_data %>%
  filter(trial == "IFCT-0002", subgroup == "Overall") %>%
  mutate(hr     = as.numeric(hr),
         hr_lci = as.numeric(hr_lci),
         hr_uci = as.numeric(hr_uci),
         te     = as.numeric(te),
         se_te  = as.numeric(se_te),
         te    = ifelse(is.na(te), log(hr), te),
         se_te = ifelse(is.na(se_te), (log(hr_uci) - log(hr_lci)) / (2 * qnorm(0.975)), se_te)) %>%
  select(unique_id, trial, year, outcome, treat1, treat2, hr, hr_lci, hr_uci, te, se_te, n1, n2) %>%
  filter(outcome %in% c("EventFreeSurvival", "OverallSurvival")) %>%
  group_by(outcome) %>% slice(1) %>% ungroup()

# ------------------------------------------------------------------------------
# Depth-of-pathological-response arm-level data (pCR within MPR)
# ------------------------------------------------------------------------------
pcr_counts <- pcr %>% select(unique_id, trial, year, arm, trt, d_pcr = d, n_pcr = n)
mpr_counts <- mpr %>% select(unique_id, trial, year, arm, trt, d_mpr = d, n_mpr = n)
depth <- inner_join(pcr_counts, mpr_counts, by = c("unique_id", "trial", "year", "arm", "trt")) %>%
  mutate(
    d_pcr_int  = round(d_pcr), d_mpr_int = round(d_mpr),
    n_int      = round(n_mpr),
    mpr_only   = d_mpr - d_pcr,
    no_mpr     = n_mpr - d_mpr,
    pct_pcr    = 100 * d_pcr / n_pcr,
    pct_mpr    = 100 * d_mpr / n_mpr,
    pct_mpr_only = 100 * mpr_only / n_mpr,
    pcr_among_mpr = 100 * d_pcr / d_mpr
  )

# ------------------------------------------------------------------------------
# Trial characteristic table
# ------------------------------------------------------------------------------
raw_info <- read_excel(data_file, sheet = "raw_data") %>%
  select(trial, comparator_category)

trial_chars <- studies_info %>%
  filter(trial %in% trial_chars_order) %>%
  group_by(trial) %>%
  arrange(trial, year) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(raw_info, by = "trial") %>%
  select(trial, phase, year, first_author, stage_included,
         experimental_treatment_category, comparator_category,
         n_total, arm_n_experimental, arm_n_comparator,
         clinicaltrials_gov_id, pubmed_id)

write.csv(pcr,       file.path(outdir, "arms_pcr.csv"), row.names = FALSE)
write.csv(mpr,       file.path(outdir, "arms_mpr.csv"), row.names = FALSE)
write.csv(trae_any,  file.path(outdir, "arms_trae_any.csv"), row.names = FALSE)
write.csv(trae_g34,  file.path(outdir, "arms_trae_g34.csv"), row.names = FALSE)
write.csv(ae_any,    file.path(outdir, "arms_ae_any.csv"), row.names = FALSE)
write.csv(ae_g34,    file.path(outdir, "arms_ae_g34.csv"), row.names = FALSE)
write.csv(efs,       file.path(outdir, "contrasts_efs.csv"), row.names = FALSE)
write.csv(os,        file.path(outdir, "contrasts_os.csv"), row.names = FALSE)
write.csv(ifct,      file.path(outdir, "contrasts_ifct0002.csv"), row.names = FALSE)
write.csv(depth,     file.path(outdir, "arms_depth.csv"), row.names = FALSE)
write.csv(trial_chars, file.path(outdir, "trial_characteristics.csv"), row.names = FALSE)

# quick console report for manual verification
for (nm in c("pcr", "mpr", "trae_any", "trae_g34", "ae_any", "ae_g34")) {
  d <- get(nm)
  cat("\n==", nm, "==\n")
  print(as.data.frame(d))
}
cat("\n== EFS contrasts ==\n"); print(as.data.frame(efs))
cat("\n== OS contrasts ==\n"); print(as.data.frame(os))
cat("\n== depth (experimental arms) ==\n")
print(as.data.frame(depth %>% filter(arm == "exp") %>% arrange(trt) %>%
  select(trial, trt, n_int, d_pcr_int, d_mpr_int, mpr_only, pcr_among_mpr)))
