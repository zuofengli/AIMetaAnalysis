# -*- coding: utf-8 -*-
"""Generate LaTeX fragments for the supplementary tables (S1-S10)."""

import csv
import os

# Paths are resolved relative to this script so the project can be moved.
BASE = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
RES = os.path.join(BASE, "analysis", "results")

# Manuscript output root: OUTSIDE this git repository, so the tables can never
# be committed with the data/code release.  Resolved relative to this script
# (no absolute drive letter); override with AIMETA_TEX_DIR.
TEX = os.environ.get("AIMETA_TEX_DIR") or os.path.abspath(
    os.path.join(BASE, "..", "..", "manuscript_v2"))
TAB = os.path.join(TEX, "tables")
os.makedirs(TAB, exist_ok=True)

def f2(x):
    return f"{float(x):.2f}"

def f1(x):
    return f"{float(x):.1f}"

def load(path):
    with open(path, encoding="utf-8-sig") as fh:
        return list(csv.DictReader(fh))

# ---------------------------------------------------------------------------
# S1: frequentist engine cross-validation (binary outcomes)
# ---------------------------------------------------------------------------
bin_outcomes = ["pcr", "mpr", "trae_any", "ae_any", "ae_g34", "trae_g34"]
out_label = {
    "pcr": "pCR", "mpr": "MPR", "trae_any": "TRAE (any grade)",
    "trae_g34": "TRAE grade >=3", "ae_any": "AE any cause (any grade)",
    "ae_g34": "AE any cause (grade >=3)"}

rows = []
for o in bin_outcomes:
    nma = {r["treatment"]: r for r in load(os.path.join(RES, f"freq_{o}_fixed_pairs.csv"))
           if r["vs"] == "Chemotherapy" and r["treatment"] != "Chemotherapy"}
    nm = {r["treatment"]: r for r in load(os.path.join(RES, "netmeta_vs_chemo_all.csv"))
          if r["outcome"] == o and r["treatment"] != "Chemotherapy"}
    for t in sorted(nma, key=lambda x: -float(nma[x]["est"])):
        a, b = nma[t], nm[t]
        rows.append((out_label[o], t, a["est"], a["lo"], a["hi"],
                     b["est_common"], b["lo_common"], b["hi_common"]))

lines = [r"\begin{table}[htbp]", r"\centering\small",
         r"\setlength{\tabcolsep}{4pt}",
         r"\renewcommand{\arraystretch}{1.2}",
         r"\caption{Cross-validation of the two "
         r"independent frequentist implementations (NMA package versus "
         r"netmeta), fixed-effect estimates versus chemotherapy.}",
         r"\begin{tabular}{llcccccc}",
         r"\toprule",
         r"Outcome & Treatment & \multicolumn{3}{c}{NMA package} & "
         r"\multicolumn{3}{c}{netmeta} \\",
         r"\cmidrule(lr){3-5}\cmidrule(lr){6-8}",
         r" & & OR & 95\% CI & & OR & 95\% CI & \\",
         r"\midrule"]
for o, t, e1, l1, h1, e2, l2, h2 in rows:
    lines.append(f"{o} & {t} & {f2(e1)} & {f2(l1)}, {f2(h1)} & & "
                 f"{f2(e2)} & {f2(l2)}, {f2(h2)} & \\\\")
lines += [r"\bottomrule", r"\end{tabular}", r"\end{table}"]
with open(os.path.join(TAB, "supp_s1_cross_engine.tex"), "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines) + "\n")
print("wrote S1")

# ---------------------------------------------------------------------------
# S2: convergence diagnostics
# ---------------------------------------------------------------------------
rows = load(os.path.join(RES, "bayes_convergence_fixed.csv"))
lines = [r"\begin{table}[htbp]", r"\centering\small",
         r"\setlength{\tabcolsep}{4pt}",
         r"\renewcommand{\arraystretch}{1.2}",
         r"\caption{Convergence diagnostics for the "
         r"fixed-effect Bayesian NMAs (four chains, 25,000 iterations, thinning "
         r"5). Rhat: Brooks--Gelman--Rubin statistic across monitored "
         r"parameters; ESS: effective sample size.}",
         r"\begin{tabular}{lccc}",
         r"\toprule",
         r"Outcome & Max Rhat & Min ESS & Parameters with ESS > 200 \\",
         r"\midrule"]
for r in sorted(rows, key=lambda x: x["model"]):
    lines.append(f"{out_label.get(r['model'], r['model'])} & "
                 f"{float(r['rhat_max']):.4f} & "
                 f"{float(r['ess_min']):.0f} & {r['ess_gt_200']} \\\\")
lines += [r"\bottomrule", r"\end{tabular}", r"\end{table}"]
with open(os.path.join(TAB, "supp_s2_convergence.tex"), "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines) + "\n")
print("wrote S2")

# ---------------------------------------------------------------------------
# S3: rounding sensitivity
# ---------------------------------------------------------------------------
rows = load(os.path.join(RES, "rounding_sensitivity.csv"))
lines = [r"\begin{table}[htbp]", r"\centering\small",
         r"\setlength{\tabcolsep}{4pt}",
         r"\renewcommand{\arraystretch}{1.2}",
         r"\caption{Sensitivity of pCR and MPR "
         r"fixed-effect ORs to integer rounding of reconstructed response "
         r"counts. The ratio column compares rounded with unrounded "
         r"(reported-percentage) counts; all ratios equal 1.}",
         r"\begin{tabular}{llccc}",
         r"\toprule",
         r"Outcome & Treatment & Rounded OR & 95\% CI & Ratio vs original \\",
         r"\midrule"]
for r in rows:
    lines.append(f"{out_label[r['outcome']]} & {r['treatment']} & {f2(r['rounded_est'])} & "
                 f"{f2(r['rounded_lo'])}, {f2(r['rounded_hi'])} & {float(r['ratio']):.3f} \\\\")
lines += [r"\bottomrule", r"\end{tabular}", r"\end{table}"]
with open(os.path.join(TAB, "supp_s3_rounding.tex"), "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines) + "\n")
print("wrote S3")

# ---------------------------------------------------------------------------
# S4: depth of pathological response
# ---------------------------------------------------------------------------
rows = load(os.path.join(BASE, "analysis", "curated", "arms_depth.csv"))
lines = [r"\begin{table}[htbp]", r"\centering\small",
         r"\setlength{\tabcolsep}{3pt}",
         r"\renewcommand{\arraystretch}{1.2}",
         r"\caption{Depth of pathological response by "
         r"trial and arm. Rates are percentages of the trial-specific "
         r"ITT/mITT denominator; MPR without pCR = MPR minus pCR; "
         r"pCR among MPR is the conditional fraction.}",
         r"\begin{tabular}{llcccccc}",
         r"\toprule",
         r"Trial & Arm & Treatment & n & pCR \% & MPR \% & MPR w/o pCR \% & pCR among MPR \% \\",
         r"\midrule"]
for r in rows:
    lines.append(f"{r['trial']} & {r['arm']} & {r['trt']} & {int(float(r['n_pcr']))} & "
                 f"{f1(r['pct_pcr'])} & {f1(r['pct_mpr'])} & "
                 f"{f1(r['pct_mpr_only'])} & {f1(r['pcr_among_mpr'])} \\\\")
lines += [r"\bottomrule", r"\end{tabular}", r"\end{table}"]
with open(os.path.join(TAB, "supp_s4_depth.tex"), "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines) + "\n")
print("wrote S4")

# ---------------------------------------------------------------------------
# S9: paradigm agreement
# ---------------------------------------------------------------------------
rows = load(os.path.join(RES, "paradigm_agreement.csv"))
lines = [r"\begin{table}[htbp]", r"\centering\small",
         r"\setlength{\tabcolsep}{4pt}",
         r"\renewcommand{\arraystretch}{1.2}",
         r"\caption{Agreement between frequentist "
         r"P-scores and Bayesian SUCRA values across agents within each "
         r"outcome.}",
         r"\begin{tabular}{lcccccc}",
         r"\toprule",
         r"Outcome & n agents & Spearman rho & Pearson r & Mean abs diff & Max abs diff \\",
         r"\midrule"]
for r in sorted(rows, key=lambda x: x["outcome"]):
    lines.append(f"{out_label.get(r['outcome'], r['outcome'])} & {r['n_agents']} & "
                 f"{float(r['spearman']):.2f} & {float(r['pearson']):.3f} & "
                 f"{float(r['mean_abs_diff']):.4f} & {float(r['max_abs_diff']):.4f} \\\\")
# pooled row across all agent-outcome pairs
ps_rows = load(os.path.join(RES, "netmeta_pscores_all.csv"))
sucra_rows = {}
for f in os.listdir(RES):
    if not f.startswith("bayes_") or not f.endswith("_fixed_sucra.csv"):
        continue
    o = f.split("bayes_")[1].split("_fixed_sucra")[0]
    sucra_rows[o] = {r["treatment"]: float(r["sucra"])
                     for r in load(os.path.join(RES, f))}
pool = []
for r in ps_rows:
    o, t = r["outcome"], r["treatment"]
    if t != "Chemotherapy" and t in sucra_rows.get(o, {}):
        pool.append((float(r["pscore_common"]), sucra_rows[o][t]))
ps_v = [a for a, _ in pool]
sc_v = [b for _, b in pool]
n = len(pool)
mps = sum(ps_v) / n
msc = sum(sc_v) / n
pearson = (sum((a - mps) * (b - msc) for a, b in pool) /
           (sum((a - mps) ** 2 for a in ps_v) *
            sum((b - msc) ** 2 for b in sc_v)) ** 0.5)
def rank(x):
    s = sorted(x)
    return [s.index(v) + 1 for v in x]
rp, rs = rank(ps_v), rank(sc_v)
mrp, mrs = sum(rp) / n, sum(rs) / n
spearman = (sum((a - mrp) * (b - mrs) for a, b in zip(rp, rs)) /
            (sum((a - mrp) ** 2 for a in rp) *
             sum((b - mrs) ** 2 for b in rs)) ** 0.5)
mad = sum(abs(a - b) for a, b in pool) / n
mxd = max(abs(a - b) for a, b in pool)
lines.append(r"\midrule")
lines.append(f"\\textbf{{All outcomes pooled}} & {n} & {spearman:.3f} & "
             f"{pearson:.3f} & {mad:.4f} & {mxd:.4f} \\\\")
lines += [r"\bottomrule", r"\end{tabular}", r"\end{table}"]
with open(os.path.join(TAB, "supp_s9_agreement.tex"), "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines) + "\n")
print("wrote S9")

# ---------------------------------------------------------------------------
# S10: heterogeneity metrics
# ---------------------------------------------------------------------------
rows = load(os.path.join(RES, "netmeta_heterogeneity_all.csv"))
lines = [r"\begin{table}[htbp]", r"\centering\small",
         r"\setlength{\tabcolsep}{4pt}",
         r"\renewcommand{\arraystretch}{1.2}",
         r"\caption{Heterogeneity metrics for each "
         r"network (Cochran's Q and p-value, I-squared, tau).}",
         r"\begin{tabular}{lcccc}",
         r"\toprule",
         r"Outcome & Q (df) & p (Q) & I-squared (\%) & tau \\",
         r"\midrule"]
for r in rows:
    tau = "NA" if r["tau"] in ("NA", "") else f2(r["tau"])
    q = "NA" if r["Q"] in ("NA", "") else f"{float(r['Q']):.2f}"
    pv = "NA" if r["pval_Q"] in ("NA", "") else f"{float(r['pval_Q']):.3f}"
    i2 = "NA" if r["I2"] in ("NA", "") else f2(r["I2"])
    lines.append(f"{out_label.get(r['outcome'], r['outcome'])} & "
                 f"{q} ({r['df_Q']}) & {pv} & {i2} & {tau} \\\\")
lines += [r"\bottomrule", r"\end{tabular}", r"\end{table}"]
with open(os.path.join(TAB, "supp_s10_heterogeneity.tex"), "w", encoding="utf-8") as fh:
    fh.write("\n".join(lines) + "\n")
print("wrote S10")

print("done")
