# -*- coding: utf-8 -*-
"""Generate dual-paradigm league-table LaTeX fragments from the analysis CSVs."""

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

ORDER = ["Chemotherapy", "Camrelizumab", "Durvalumab", "Nivolumab",
         "Pembrolizumab", "Tislelizumab", "Toripalimab"]

# Treatment display order and accepted spellings (incl. legacy typos) are kept
# in one shared config file so that adding a drug is a data edit, not a code
# edit.  Falls back to the built-in list above if the file is missing.
_CFG = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                    "config", "treatments.csv")
NAME_FIX = {}
if os.path.exists(_CFG):
    _rows = []
    with open(_CFG, encoding="utf-8-sig") as _fh:
        for _r in csv.DictReader(_fh):
            _rows.append(_r)
            _t = (_r.get("treatment") or "").strip()
            _al = (_r.get("aliases") or "").strip()
            if _t and _al:
                for _a in _al.split(";"):
                    if _a.strip():
                        NAME_FIX[_a.strip()] = _t
    _rows.sort(key=lambda r: int(float(r.get("order") or 0)))
    if _rows:
        ORDER = [r["treatment"].strip() for r in _rows]

def norm_name(x):
    return NAME_FIX.get(x, x)

def fmt(x):
    return f"{x:.2f}"

def load_pairs(path, key_treatment="treatment", key_vs="vs"):
    rows = {}
    with open(path, encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            t = norm_name(r[key_treatment]); v = norm_name(r[key_vs])
            if not t or not v:
                continue
            rows[(t, v)] = (float(r["est"]), float(r["lo"]), float(r["hi"]))
    return rows

def bidirectional(rows):
    """Return a dict keyed by (A, B) whose value is the estimate for A vs B.
    If only the reverse direction was stored, add the reciprocal."""
    out = dict(rows)
    for (a, b), (est, lo, hi) in list(rows.items()):
        if a == b:
            continue
        if (b, a) not in out:
            out[(b, a)] = (1.0 / est, 1.0 / hi, 1.0 / lo)
    return out

def bayes_pairs(outcome):
    path = os.path.join(RES, f"bayes_{outcome}_fixed_pairs.csv")
    rows = {}
    with open(path, encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            t, v = norm_name(r["treatment"]), norm_name(r["vs"])
            if t and v:
                rows[(t, v)] = (float(r["med"]), float(r["lo"]), float(r["hi"]))
    return bidirectional(rows)

def freq_pairs(outcome):
    if outcome in ("efs", "os"):
        path = os.path.join(RES, f"freq_{outcome}_pairs.csv")
        rows = {}
        with open(path, encoding="utf-8-sig") as fh:
            for r in csv.DictReader(fh):
                if r.get("model") not in ("common", None):
                    continue
                col, row = norm_name(r["col"]), norm_name(r["row"])
                # stored value is the effect of `col` vs `row`
                rows[(col, row)] = (
                    float(r["est"]), float(r["lo"]), float(r["hi"]))
        return bidirectional(rows)
    path = os.path.join(RES, f"freq_{outcome}_fixed_pairs.csv")
    return bidirectional(load_pairs(path))

def is_sig(ci):
    lo, hi = ci[1], ci[2]
    return (lo > 1.0 and hi > 1.0) or (lo < 1.0 and hi < 1.0)

def cell_html(pair):
    if pair is None:
        return ""
    est, lo, hi = pair
    return f"{fmt(est)} ({fmt(lo)}, {fmt(hi)})"

def league_tex(outcome, measure="OR", caption="", label="", wide=True):
    """Emit one dual-paradigm league table.

    `wide=True` produces a `sidewaystable`, i.e. the table is rotated 90 degrees
    onto a landscape float page.  The 8-column grid of an NMA league table is
    roughly 720-780 pt wide, which cannot fit the 483 pt portrait text block at
    any readable size, so landscape is the journal-standard presentation.  Once
    rotated, the usable extent equals the portrait \\textheight (664 pt); the
    settings below are chosen so that every table clears that budget without
    being scaled (widest case: pCR at 643 pt).  The `adjustbox` guard is a safety
    net only: it engages if a future update makes a table wider than the page.
    """
    fr = freq_pairs(outcome)
    ba = bayes_pairs(outcome)
    trts = [t for t in ORDER
            if t in {k[0] for k in fr} or t in {k[0] for k in ba}]
    n = len(trts)
    lines = []
    lines.append(r"\begin{sidewaystable}[p]" if wide
                 else r"\begin{table}[htbp]")
    lines.append(r"\centering")
    lines.append(r"\scriptsize")
    lines.append(r"\setlength{\tabcolsep}{4pt}")
    lines.append(r"\renewcommand{\arraystretch}{1.2}")
    lines.append(r"\caption{" + caption + "}")
    lines.append(r"\label{" + label + "}")
    if wide:
        lines.append(r"\begin{adjustbox}{max width=\textheight}")
    lines.append(r"\begin{tabular}{l" + "c" * n + "}")
    lines.append(r"\toprule")
    hdr = " & " + " & ".join(trts) + r" \\"
    lines.append(hdr)
    lines.append(r"\midrule")
    for i, row_trt in enumerate(trts):
        cells = [row_trt]
        for j, col_trt in enumerate(trts):
            if i == j:
                cells.append("1 (ref)")
            elif i < j:
                # upper triangle: frequentist estimate of column vs row
                pair = fr.get((col_trt, row_trt))
                txt = cell_html(pair)
                if pair and is_sig(pair):
                    txt = r"\textbf{" + txt + "}"
                cells.append(txt)
            else:
                # lower triangle: Bayesian estimate of column vs row
                pair = ba.get((col_trt, row_trt))
                txt = cell_html(pair)
                if pair and is_sig(pair):
                    txt = r"\textbf{" + txt + "}"
                cells.append(txt)
        lines.append(" & ".join(cells) + r" \\")
    lines.append(r"\bottomrule")
    lines.append(r"\end{tabular}")
    if wide:
        lines.append(r"\end{adjustbox}")
    lines.append(r"\end{sidewaystable}" if wide else r"\end{table}")
    return "\n".join(lines)

SPECS = [
    ("pcr", "OR",
     "Dual-paradigm league table for pathological complete response (pCR). "
     "Upper triangle: frequentist fixed-effect OR (column treatment vs row treatment); "
     "lower triangle: Bayesian fixed-effect OR. Bold: 95\\% CI excludes 1.",
     "tab:league_pcr"),
    ("mpr", "OR",
     "Dual-paradigm league table for major pathological response (MPR). "
     "Upper triangle: frequentist fixed-effect OR; lower triangle: Bayesian "
     "fixed-effect OR. Bold: 95\\% CI excludes 1.",
     "tab:league_mpr"),
    ("efs", "HR",
     "Dual-paradigm league table for event-free survival (EFS). Upper triangle: "
     "frequentist common-effect HR; lower triangle: Bayesian fixed-effect HR. "
     "Bold: 95\\% CI excludes 1.",
     "tab:league_efs"),
    ("os", "HR",
     "Dual-paradigm league table for overall survival (OS). Upper triangle: "
     "frequentist common-effect HR; lower triangle: Bayesian fixed-effect HR. "
     "Bold: 95\\% CI excludes 1.",
     "tab:league_os"),
    ("trae_any", "OR",
     "Dual-paradigm league table for treatment-related adverse events of any "
     "grade (TRAEs). Upper triangle: frequentist fixed-effect OR; lower "
     "triangle: Bayesian fixed-effect OR (OR>1 = more events). Bold: 95\\% CI "
     "excludes 1.",
     "tab:league_trae_any"),
    ("trae_g34", "OR",
     "Exploratory dual-paradigm league table for treatment-related adverse "
     "events of grade $\\ge$3 (TRAEs). Upper triangle: frequentist fixed-effect "
     "OR; lower triangle: Bayesian fixed-effect OR (OR>1 = more events). "
     "Bold: 95\\% CI excludes 1.",
     "tab:league_trae_g34"),
    ("ae_any", "OR",
     "Dual-paradigm league table for adverse events of any cause and any grade. "
     "Upper triangle: frequentist fixed-effect OR; lower triangle: Bayesian "
     "fixed-effect OR. Bold: 95\\% CI excludes 1.",
     "tab:league_ae_any"),
    ("ae_g34", "OR",
     "Dual-paradigm league table for adverse events of any cause, grade $\\ge$3. "
     "Upper triangle: frequentist fixed-effect OR; lower triangle: Bayesian "
     "fixed-effect OR. Bold: 95\\% CI excludes 1.",
     "tab:league_ae_g34"),
]

# League tables whose 8-column grid is too wide for the portrait text block are
# rotated onto a landscape float page.  Keeping the whole family rotated also
# keeps the presentation consistent across the manuscript and the supplement.
LANDSCAPE_OUTCOMES = {"pcr", "mpr", "efs", "os",
                      "trae_any", "trae_g34", "ae_any", "ae_g34"}

for outcome, measure, caption, label in SPECS:
    body = league_tex(outcome, measure, caption, label,
                      wide=outcome in LANDSCAPE_OUTCOMES)
    with open(os.path.join(TAB, f"league_{outcome}.tex"), "w",
              encoding="utf-8") as fh:
        fh.write(body + "\n")
    print("wrote", outcome)

print("done")
