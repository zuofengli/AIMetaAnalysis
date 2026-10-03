# =============================================================================
# run_pipeline.R
# One-command driver for the publication_v2 pipeline.
#
#   Rscript run_pipeline.R            # full pipeline (analysis + tables)
#   Rscript run_pipeline.R --fast     # skip the Bayesian MCMC steps
#   Rscript run_pipeline.R --pdf      # ... and also build main/supplementary PDF
#   Rscript run_pipeline.R --fast --pdf
#
# It reruns, in order:
#   analysis/00_curate.R            -> curated/*.csv      (the single source of truth)
#   analysis/01_nma_freq_binary.R   -> frequentist NMA (NMA package)
#   analysis/02b_netmeta_all.R      -> frequentist NMA (netmeta), P-scores, heterogeneity
#   analysis/02_nma_freq_survival.R -> EFS/OS pair matrices
#   analysis/03_nma_bayes.R         -> Bayesian NMA (gemtc/rjags), SUCRA
#   analysis/04_figures.R           -> <manuscript>/figures/fig2..fig6
#   analysis/05_diagnostics.R       -> convergence, agreement, rounding sensitivity
#   analysis/make_latex_tables.py   -> <manuscript>/tables/league_*.tex
#   analysis/make_supp_tables.py    -> <manuscript>/tables/supp_s*.tex
#   analysis/check_table_fit.py     -> guard rail: fails if any table no longer
#                                      fits its page block (rotated league tables
#                                      are capped by the text height)
#   (with --pdf)  pdflatex+bibtex   -> <manuscript>/main.pdf, supplementary.pdf
#
# <manuscript> is a directory OUTSIDE this git repository -- by default
# ../../manuscript_v2, i.e. 999.manuscript/writeup/manuscript_v2.  Keeping the
# manuscript out of the repository means it can never be committed along with
# the public data/code release.  Point AIMETA_TEX_DIR somewhere else to override.
#
# Requirements: R >= 4.5 with gemtc, rjags, netmeta, NMA, coda, ggplot2, dplyr,
# tidyr, readxl; JAGS 4.x; Python 3.
# =============================================================================

# --- Windows locale guard: must run BEFORE anything touches the path, because
# a bogus LC_ALL (e.g. "C.UTF-8" exported by some shells) makes R fall back to
# the C locale and silently strips the high bit off non-ASCII path bytes
# (e.g. ".../办公/..."). Setting a real UTF-8 locale fixes both this process and
# the child processes spawned below (via the exported LC_ALL).
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  try(Sys.setlocale("LC_ALL", "English_United States.utf8"), silent = TRUE)
}
if (.Platform$OS.type == "windows" && isTRUE(l10n_info()[["UTF-8"]])) {
  Sys.setenv(LC_ALL = "English_United States.utf8")
}

# --- locate this script so the pipeline can be run from any directory --------
here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f)) return(dirname(normalizePath(sub("^--file=", "", f[1]))))
  if (!is.null(sys.frame(1)$ofile)) return(dirname(normalizePath(sys.frame(1)$ofile)))
  normalizePath(getwd())
})
ANALYSIS <- file.path(here, "analysis")

# --- manuscript output root --------------------------------------------------
# The manuscript, its figures and its tables deliberately live OUTSIDE this git
# repository, so that they can never be swept into the public data/code release
# by accident.  The location is resolved relative to this script (no drive
# letter is hard-coded) and can be overridden with AIMETA_TEX_DIR.
TEX <- Sys.getenv("AIMETA_TEX_DIR", "")
if (!nzchar(TEX)) TEX <- file.path(here, "..", "..", "manuscript_v2")
TEX <- normalizePath(TEX, winslash = "/", mustWork = FALSE)
# exported so every child R and Python step resolves the same location
Sys.setenv(AIMETA_TEX_DIR = TEX)

args  <- commandArgs(trailingOnly = TRUE)
fast  <- "--fast" %in% args
build_pdf <- "--pdf" %in% args

cat("=====================================================================\n")
cat(" publication_v2 pipeline\n")
cat(" project root :", here, "\n")
cat(" manuscript   :", TEX, "\n")
cat(" R            :", R.version.string, "\n")
cat(" mode         :", if (fast) "fast (no MCMC)" else "full",
    if (build_pdf) "+ PDF build" else "", "\n")
cat("=====================================================================\n")

RSCRIPT <- file.path(R.home("bin"), "Rscript")
if (.Platform$OS.type == "windows" && !file.exists(RSCRIPT)) {
  RSCRIPT <- file.path(R.home("bin"), "Rscript.exe")
}
if (!file.exists(RSCRIPT)) {
  cand <- Sys.which("Rscript")
  if (nzchar(cand)) RSCRIPT <- cand
}

steps <- c(
  "00_curate.R",
  "01_nma_freq_binary.R",
  "02b_netmeta_all.R",
  "02_nma_freq_survival.R",
  if (!fast) "03_nma_bayes.R",
  "04_figures.R",
  if (!fast) "05_diagnostics.R"
)

run_step <- function(cmd, args) {
  # do NOT shQuote() the command path (system2 handles it); quote args only
  system2(cmd, args)
}

failures <- character(0)
t0 <- Sys.time()
for (s in steps) {
  cat("\n----- [", format(Sys.time(), "%H:%M:%S"), "] running", s, "-----\n")
  st <- run_step(RSCRIPT, shQuote(file.path(ANALYSIS, s)))
  if (!identical(st, 0L) && !identical(st, 0)) {
    cat("  !! STEP FAILED:", s, "(exit", st, ")\n")
    failures <- c(failures, s)
  }
}

# --- LaTeX table fragments (Python) ------------------------------------------
find_python <- function() {
  cand <- c(Sys.getenv("PYTHON", ""),
            unname(Sys.which("python")), unname(Sys.which("python3")),
            "C:/Users/lizuo/.workbuddy/binaries/python/versions/3.13.12/python.exe",
            file.path(Sys.getenv("LOCALAPPDATA"),
                      "Programs/Python/Python313/python.exe"),
            "C:/Python313/python.exe", "C:/Python312/python.exe")
  cand <- cand[nzchar(cand)]
  for (c in cand) if (file.exists(c)) return(c)
  ""
}
pybin <- find_python()
if (nzchar(pybin)) {
  cat("\n----- python :", pybin, "\n")
  for (p in c("make_latex_tables.py", "make_supp_tables.py")) {
    cat("----- [", format(Sys.time(), "%H:%M:%S"), "] running", p, "-----\n")
    st <- run_step(pybin, shQuote(file.path(ANALYSIS, p)))
    if (!identical(st, 0L) && !identical(st, 0)) {
      cat("  !! STEP FAILED:", p, "(exit", st, ")\n")
      failures <- c(failures, p)
    }
  }

  # Guard rail: the league tables are rotated 90 degrees and must fit the
  # portrait text height, the supplementary tables the text width.  A new trial
  # or agent can widen a grid, so every fragment is measured before it is
  # allowed through.
  cat("\n----- [", format(Sys.time(), "%H:%M:%S"), "] checking table widths -----\n")
  st <- run_step(pybin, shQuote(file.path(ANALYSIS, "check_table_fit.py")))
  if (!identical(st, 0L) && !identical(st, 0)) {
    cat("  !! STEP FAILED: check_table_fit.py (exit", st, ")\n")
    failures <- c(failures, "check_table_fit.py")
  }
} else {
  cat("\n!! Python not found; skipping LaTeX table generation.\n")
  cat("   Set the PYTHON environment variable to your python.exe and rerun.\n")
}

# --- optional: build the PDFs (--pdf) ---------------------------------------
if (build_pdf) {
  tex_dir  <- TEX
  pdflatex <- unname(Sys.which("pdflatex"))
  bibtex   <- unname(Sys.which("bibtex"))
  if (nzchar(pdflatex)) {
    owd <- getwd(); setwd(tex_dir)
    for (doc in c("main", "supplementary")) {
      if (!file.exists(paste0(doc, ".tex"))) next
      cat("\n----- [", format(Sys.time(), "%H:%M:%S"), "] building", doc, ".pdf -----\n")
      run_step(pdflatex, c("-interaction=nonstopmode", shQuote(doc)))
      if (nzchar(bibtex)) run_step(bibtex, shQuote(doc))
      run_step(pdflatex, c("-interaction=nonstopmode", shQuote(doc)))
      run_step(pdflatex, c("-interaction=nonstopmode", shQuote(doc)))
      if (file.exists(paste0(doc, ".pdf"))) {
        cat("  built", paste0(doc, ".pdf"), "\n")
      } else {
        cat("  !! failed to build", doc, ".pdf\n")
        failures <- c(failures, paste0(doc, ".pdf"))
      }
      # Report what still overruns the margin.  An "in alignment" box is the
      # signature of a table wider than its block and is treated as a failure;
      # ordinary justified-paragraph overfulls are reported for information.
      lg <- paste0(doc, ".log")
      if (file.exists(lg)) {
        lines <- readLines(lg, warn = FALSE, encoding = "latin1")
        ov <- grep("^Overfull \\\\hbox", lines, value = TRUE)
        if (length(ov) == 0) {
          cat("  no overfull box\n")
        } else {
          amt <- suppressWarnings(as.numeric(sub("^Overfull \\\\hbox \\(([0-9.]+)pt.*",
                                                 "\\1", ov)))
          cat(sprintf("  %d overfull box(es), worst %.2f pt\n",
                      length(ov), max(amt, na.rm = TRUE)))
          tab_ov <- grep("in alignment", ov, value = TRUE)
          if (length(tab_ov)) {
            cat("  !! table wider than its block:\n")
            cat(paste0("     ", tab_ov, collapse = "\n"), "\n")
            failures <- c(failures, paste0(doc, ".tex table overflow"))
          }
        }
      }
    }
    setwd(owd)
  } else {
    cat("\n!! pdflatex not found; skipping PDF build.\n")
  }
}

elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
cat("\n=====================================================================\n")
if (length(failures) == 0) {
  cat(" pipeline finished OK in", elapsed, "min\n")
} else {
  cat(" pipeline finished with", length(failures), "failure(s) in", elapsed, "min:\n")
  cat("   ", paste(failures, collapse = ", "), "\n")
}
cat(" outputs :", file.path(here, "analysis/results"), "\n")
cat("           ", file.path(TEX, "figures"), "\n")
cat("           ", file.path(TEX, "tables"), "\n")
cat("=====================================================================\n")
