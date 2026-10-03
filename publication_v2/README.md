# Dual-paradigm NMA manuscript package (publication_v2)

This folder contains the analysis pipeline and a submission-ready LaTeX draft
for the project:

> **Perioperative PD-1/PD-L1 inhibitors plus platinum-based chemotherapy in
> resectable NSCLC: a dual-paradigm (frequentist and Bayesian) network
> meta-analysis of pathological response, survival and safety**

The draft was prepared for a Chinese physician-researcher team and is written
in English for international journal submission.

## What is inside

```text
publication_v2/
  run_pipeline.R          # ONE-COMMAND driver (analysis + LaTeX tables)
  run_pipeline.bat        # same, double-clickable on Windows
  PIPELINE_GUIDE.md       # 中文操作指南: 一键重跑 + 新研究数据如何录入
  analysis/
    00_curate.R           # builds curated endpoint datasets from the source workbook
    trial_registry.R      # SINGLE place to register which trial feeds which endpoint
    config/treatments.csv # treatment order + accepted spellings (shared by R and Python)
    01_nma_freq_binary.R  # frequentist NMA (NMA package), binary endpoints
    02b_netmeta_all.R     # frequentist NMA (netmeta), binary + survival
    02_nma_freq_survival.R# frequentist pair matrices for EFS/OS league tables
    03_nma_bayes.R        # Bayesian NMA (gemtc/rjags), all endpoints
    04_figures.R          # figures 2-5
    05_diagnostics.R      # Rhat/ESS, P-score-SUCRA agreement, rounding sensitivity
    make_latex_tables.py  # regenerates league-table LaTeX fragments
    make_supp_tables.py   # regenerates supplementary-table LaTeX fragments
    check_table_fit.py    # guard rail: fails if any table no longer fits its page
    curated/*.csv         # data used by all analyses (source of truth)
    results/*.csv|.rds    # numeric outputs, diagnostics, session info
  (the manuscript is NOT stored here -- see "Where the manuscript lives")
```

All script paths are resolved **relative to the script itself**, so the whole
`publication_v2/` folder can be moved or copied anywhere without editing code.

### Where the manuscript lives

The manuscript -- `main.tex`, `supplementary.tex`, `references.bib`, the
compiled PDFs, and the generated `tables/` and `figures/` -- deliberately lives
**outside this git repository**, by default at `../../manuscript_v2`, i.e.
`999.manuscript/writeup/manuscript_v2`. Keeping it out of the repository means
it can never be swept into the public data/code release by accident.

The pipeline writes its tables, figures and PDFs into that folder; nothing in
this repository depends on it being present, and the folder is not tracked by
git. Export `AIMETA_TEX_DIR` before running the pipeline to point somewhere
else.

## Reproduce the analyses

R 4.5.x is required, together with `gemtc`, `rjags`, `netmeta` (3.6-1), `NMA`,
`coda`, `ggplot2`, `dplyr`, `tidyr` and JAGS 4.3.2. Then, from this folder:

```bash
Rscript run_pipeline.R          # full pipeline incl. Bayesian MCMC
Rscript run_pipeline.R --fast   # skip the MCMC steps (minutes instead of tens of minutes)
Rscript run_pipeline.R --pdf    # also build writeup/main.pdf and supplementary.pdf
```

or double-click `run_pipeline.bat`. The driver runs, in order:

```text
analysis/00_curate.R              -> analysis/curated/*.csv
analysis/01_nma_freq_binary.R
analysis/02b_netmeta_all.R
analysis/02_nma_freq_survival.R
analysis/03_nma_bayes.R
analysis/04_figures.R
analysis/05_diagnostics.R
analysis/make_latex_tables.py     -> <manuscript>/tables/league_*.tex
analysis/make_supp_tables.py      -> <manuscript>/tables/supp_s*.tex
analysis/check_table_fit.py       -> fails the run if any table overruns its page
```

Individual steps can still be run one at a time with `Rscript analysis/<step>.R`
(and `python analysis/<step>.py`). To add a new study, edit the source workbook
plus `analysis/trial_registry.R` --- see `PIPELINE_GUIDE.md` (Chinese) for the
step-by-step procedure.

## Table layout

The 8 dual-paradigm league tables are **rotated 90 degrees onto landscape float
pages** (`sidewaystable`, from the `rotating` package). A 7-agent league table is
roughly 720-775 pt wide, while the portrait text block is only 483 pt, so no
readable font size fits it upright; landscape is the journal-standard treatment
for wide tables. Rotated, the available extent equals the text *height*
(664 pt), and the fragments are set in `\scriptsize` with `\tabcolsep = 4pt`
so the widest table (pCR) measures 643 pt. An `adjustbox` cap of
`max width=\textheight` is a safety net: if a future update widens a grid, the
table is scaled down instead of running into the margin.

Table 1 (trial characteristics) stays upright --- its overrun was an arithmetic
error in the column widths, now corrected (`\footnotesize`, `\tabcolsep = 2.5pt`,
13.55 cm of fixed columns inside the 17.04 cm block).

Run `python analysis/check_table_fit.py` after any change to the tables; it
prints the measured width, the budget and the remaining headroom for every
fragment and exits non-zero if one no longer fits.

## Build the PDF

Compile with a TeX distribution that includes `pdflatex`, `bibtex` and the
`booktabs`, `natbib`, `tikz`, `rotating`, `adjustbox` packages:

```text
pdflatex main
bibtex main
pdflatex main
pdflatex main
```

and the same for `supplementary.tex`. The user opens the resulting `.pdf` with
SumatraPDF (`999.manuscript/writeup/SumatraPDF-3.6.1-64.exe`), which is a PDF
viewer only --- compilation is done by the TeX distribution.

## Data and endpoint conventions (read before submission)

* Curated datasets (`analysis/curated/`) mirror the numbers already validated
  against the user's own published league tables and against Mei et al. 2024.
* pCR/MPR numerators for several trials are reconstructed from reported
  percentages and may be non-integer before rounding; rounding to integers
  does not change estimates (Supplementary Table S3). Patients not undergoing
  surgery count as non-responders under each trial's ITT/mITT denominator.
* Safety reporting conventions differ between trials (any-cause vs
  treatment-related; grade 3-4 vs grade >=3). The three principal safety
  networks respect those conventions; RATIONALE-315's grade-specific table is
  any-cause, TD-FOREKNOW's summaries are treatment-related, and KEYNOTE-671's
  grade >=3 summary is treatment-related. These nuances are described in
  Methods/Limitations and Supplementary Methods.
* NADIM-II contributes a PFS contrast to the EFS-type network and a 24-month
  OS contrast to the OS network; CheckMate 77T and AEGEAN have no OS data at
  the 31-Aug-2025 data lock.

## Placeholders that must be completed by the authors

1. Author names, affiliations, corresponding author, acknowledgements,
   funding and conflicts of interest (visible `[...]` markers in `main.tex`).
2. PROSPERO registration number if the target journal requires registration.
3. Final RoB 2 domain judgments (Supplementary Methods S3 currently states
   that they are being finalised).
4. Search date "30 June 2025" and data-lock date "31 August 2025" should be
   checked against the team's actual search log.
5. Title/keywords/journal-specific formatting after target journal choice.

## Notes for the Chinese team (简要说明)

* **稿件不在本仓库内**：正文 `main.tex`、补充材料 `supplementary.tex` 与生成的
  `tables/`、`figures/`、PDF 都放在仓库外的 `999.manuscript/writeup/manuscript_v2/`
  （脚本默认按 `../../manuscript_v2` 解析，可用环境变量 `AIMETA_TEX_DIR` 覆盖）；
  全部表格和图表均由 `analysis/` 下脚本自动生成，直接 `\input` 进文稿，
  改数据后重跑脚本即可同步更新，不会出现手抄数字不一致。
* **一键重跑**：`Rscript run_pipeline.R`（或双击 `run_pipeline.bat`）。
  新增研究数据如何录入、登记表在哪改，见 `PIPELINE_GUIDE.md`。
* 图 1 为 PRISMA 流程图（TikZ），图 2–5 为双范式森林图、排序图、病理
  反应深度图和显著性一致性矩阵；联赛表为“上三角频率学派、下三角贝叶斯”
  的双范式格式。
* 本会话中没有可用的 AI4Science skill，因此文稿并未依赖任何特定 skill；
  全部分析代码与数据都保留在本目录中，便于审稿人复核。
