# =============================================================================
# trial_registry.R
# -----------------------------------------------------------------------------
# SINGLE PLACE TO REGISTER TRIALS.
#
# Everything that decides *which* trial publication feeds *which* network lives
# here, so that adding a new study is a data + registry edit and never a hunt
# through the analysis code.
#
# When a new trial is published:
#   1. add its rows to inst/extdata/clinical_studies_data.xlsx
#      (sheets: studies_info, binary_data, survival, raw_data)
#   2. give it a `unique_id` of the form  "<trial>-<year>-<article#>"
#      (e.g. "KEYNOTE-671-2023-1") and register it below
#   3. run  run_pipeline.R
#
# Note on `unique_id`: several trials have more than one publication record
# (e.g. an initial and a final analysis).  The endpoint lists below therefore
# point at the *specific record* whose numbers should be used, and NOT simply
# at the trial name.  This is why e.g. CheckMate 816 appears as
# "CheckMate 816-2022-1" for EFS and "CheckMate 816-2025-1" for OS.
# =============================================================================

# --- Histological (pathological) response endpoints --------------------------
# Randomised ICI + chemotherapy vs chemotherapy comparisons contributing to the
# pCR and MPR networks.
ids_histo <- c(
  "CheckMate 816-2022-1",
  "CheckMate 77T-2024-1",
  "KEYNOTE-671-2023-1",
  "AEGEAN-2023-1",
  "NADIM-II-2023-1",
  "NEOTORCH-2024-1",
  "RATIONALE-315-2024-1",
  "TD-FOREKNOW-2023-1"
)

# --- Safety: treatment-related AEs, any grade --------------------------------
# Same set minus NADIM-II, which has no usable any-grade TRAE report.
ids_trae_any <- setdiff(ids_histo, "NADIM-II-2023-1")

# --- Safety: any-cause AE, any grade -----------------------------------------
ids_ae_any <- c(
  "CheckMate 816-2022-1",
  "CheckMate 77T-2024-1",
  "KEYNOTE-671-2023-1",
  "AEGEAN-2023-1",
  "NEOTORCH-2024-1",
  "TD-FOREKNOW-2023-1"
)

# --- Safety: any-cause AE, grade >= 3 ----------------------------------------
ids_ae_g34 <- c(
  "CheckMate 816-2022-1",
  "CheckMate 77T-2024-1",
  "KEYNOTE-671-2023-1",
  "AEGEAN-2023-1",
  "NEOTORCH-2024-1",
  "TD-FOREKNOW-2023-1",
  "RATIONALE-315-2024-1"
)

# --- TRAE totals pinned to the exact published event counts -------------------
# The source workbook stores TRAE totals under several competing outcome /
# subgroup conventions.  To stay faithful to the numbers already deposited in
# the manuscript we select the row whose event count matches the published
# value (tolerance +/- 0.6).  Add a row here for every trial that enters the
# TRAE networks.
#
#   unique_id : the publication record
#   d_exp     : treatment-related AE events, experimental arm
#   d_ctrl    : treatment-related AE events, comparator arm
expected_trae_any <- data.frame(
  unique_id = c("CheckMate 816-2022-1", "CheckMate 77T-2024-1", "AEGEAN-2023-1",
                "KEYNOTE-671-2023-1", "NEOTORCH-2024-1", "RATIONALE-315-2024-1",
                "TD-FOREKNOW-2023-1"),
  d_exp  = c(145, 203, 348, 383, 201, 224, 41),
  d_ctrl = c(156, 200, 321, 379, 199, 225, 40),
  stringsAsFactors = FALSE
)

expected_trae_g34 <- data.frame(
  unique_id = c("KEYNOTE-671-2023-1", "RATIONALE-315-2024-1", "TD-FOREKNOW-2023-1"),
  d_exp  = c(178, 163, 11),
  d_ctrl = c(149, 150, 5),
  stringsAsFactors = FALSE
)

# --- Survival contrasts: which record and which HR ---------------------------
# A trial can report several HRs (subgroups, sensitivity analyses, updated
# analyses).  Each entry picks the publication record by `id` and the contrast
# by the reported point estimate `hr`, so the selection is unambiguous.
#
#   id : publication record (unique_id)
#   hr : the published hazard-ratio point estimate to match (tolerance 0.005)
#        use  NULL  to take the first available Overall row for that record.
#
# 2026-10-04 (P1-8): EFS and OS now use the MOST MATURE analysis of each trial
# (data cut-offs 2024-05-10 to 2025-07-03), i.e. the -2026- records for
# KEYNOTE-671, RATIONALE-315 and AEGEAN.  Response (pCR/MPR) and safety remain
# on the original records.  Network structure is unchanged (EFS 8, OS 5).
efs_keys <- list(
  list(id = "CheckMate 816-2022-1", hr = 0.63),
  list(id = "CheckMate 77T-2024-1", hr = 0.58),
  list(id = "KEYNOTE-671-2026-1",   hr = 0.58),   # 5-year analysis
  list(id = "AEGEAN-2026-1",        hr = 0.69),   # updated outcomes
  list(id = "NADIM-II-2023-1",      hr = 0.47),
  list(id = "NEOTORCH-2024-1",      hr = 0.40),
  list(id = "RATIONALE-315-2026-1", hr = 0.58),   # final analysis
  list(id = "TD-FOREKNOW-2023-1",   hr = 0.52)
)

os_keys <- list(
  list(id = "CheckMate 816-2025-1", hr = 0.71),
  list(id = "KEYNOTE-671-2026-1",   hr = 0.74),   # 5-year analysis
  list(id = "NADIM-II-2023-1",      hr = 0.43),
  list(id = "NEOTORCH-2024-1",      hr = 0.62),
  list(id = "RATIONALE-315-2026-1", hr = 0.65)    # final analysis
)

# --- Trials shown in the baseline characteristics table (Table 1) ------------
# Order of appearance in the manuscript table.
trial_chars_order <- c(
  "CheckMate 816", "CheckMate 77T", "KEYNOTE-671", "AEGEAN",
  "NADIM-II", "NEOTORCH", "RATIONALE-315", "TD-FOREKNOW",
  "IFCT-0002"
)
