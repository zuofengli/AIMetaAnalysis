# -*- coding: utf-8 -*-
"""Fail-fast check that no generated table overruns its page block.

Every LaTeX fragment in the manuscript's tables/ directory is inspected for

  * which float wrapper it uses (sidewaystable -> rotated, budget = textheight;
    table -> portrait, budget = textwidth), and
  * the font size / column separation it asks for,

then each tabular is typeset on its own into a save box so LaTeX reports its
true natural width.  Anything that does not fit is reported, so that adding a
trial with a long name or a new agent to the workbook cannot silently push a
table past the margin.

Exit status is 1 when at least one table overruns, 0 otherwise, which lets
run_pipeline.R stop and tell the user.
"""
import os
import re
import sys
import glob
import subprocess

HERE = os.path.dirname(os.path.abspath(__file__))
BASE = os.path.abspath(os.path.join(HERE, ".."))

# Manuscript output root: OUTSIDE this git repository (see run_pipeline.R), so
# the tables can never be committed with the data/code release.
TEX = os.environ.get("AIMETA_TEX_DIR") or os.path.abspath(
    os.path.join(BASE, "..", "..", "manuscript_v2"))
TABDIR = os.path.join(TEX, "tables")
WORK = os.path.join(HERE, "_fitcheck")
os.makedirs(WORK, exist_ok=True)

PRE = "\n".join([
    r"\documentclass[11pt]{article}",
    r"\usepackage[margin=2.3cm]{geometry}",
    r"\usepackage[T1]{fontenc}\usepackage[utf8]{inputenc}",
    r"\usepackage{amsmath,graphicx,booktabs,array,tabularx,multirow}",
    r"\newsavebox{\fitbox}",
    r"\begin{document}",
    r"\typeout{FITWIDTH=\the\textwidth}",
    r"\typeout{FITHEIGHT=\the\textheight}",
])

# font commands we accept in the generated fragments
FONTS = (r"\scriptsize", r"\footnotesize", r"\small", r"\normalsize")


def parse(path):
    """Return (rotated, font, tabcolsep, tabular) for one fragment, or None."""
    src = open(path, encoding="utf-8").read()
    m = re.search(r"\\begin\{tabular\}.*?\\end\{tabular\}", src, re.S)
    if not m:
        return None
    rotated = "\\begin{sidewaystable}" in src
    font = next((f for f in FONTS if f in src and
                 re.search(re.escape(f) + r"\s*$", src, re.M)), r"\normalsize")
    sep = re.search(r"\\setlength\{\\tabcolsep\}\{([^}]*)\}", src)
    return rotated, font, (sep.group(1) if sep else "6pt"), m.group(0)


frags = {}
for path in sorted(glob.glob(os.path.join(TABDIR, "*.tex"))):
    parsed = parse(path)
    if parsed:
        frags[os.path.basename(path)] = parsed

if not frags:
    print("check_table_fit: no table fragments found in %s" % TABDIR)
    sys.exit(0)

doc = [PRE]
for name, (rotated, font, sep, tabular) in frags.items():
    doc.append(r"\begin{lrbox}{\fitbox}" + font
               + r"\setlength{\tabcolsep}{" + sep + r"}%")
    doc.append(r"\renewcommand{\arraystretch}{1.2}")
    doc.append(tabular)
    doc.append(r"\end{lrbox}")
    doc.append(r"\typeout{FIT|" + name + r"|" + font + r"|" + sep
               + r"|\the\wd\fitbox}")
doc.append(r"\end{document}")

tex = os.path.join(WORK, "fitcheck.tex")
open(tex, "w", encoding="utf-8").write("\n".join(doc))
subprocess.run(["pdflatex", "-interaction=nonstopmode",
                "-jobname=" + os.path.join(WORK, "fitcheck"), tex],
               capture_output=True, text=True, errors="replace")
log = open(os.path.join(WORK, "fitcheck.log"), encoding="latin-1").read()

textwidth = float(re.search(r"FITWIDTH=([\d.]+)pt", log).group(1))
textheight = float(re.search(r"FITHEIGHT=([\d.]+)pt", log).group(1))

measured = {}
for m in re.finditer(r"FIT\|([^|]+)\|([^|]+)\|([^|]+)\|([\d.]+)pt", log):
    measured[m.group(1)] = (m.group(2), m.group(3), float(m.group(4)))

print("Table fit check - portrait block %.1f pt, rotated block %.1f pt"
      % (textwidth, textheight))
print("%-30s %-9s %-7s %-8s %10s %10s  %s"
      % ("fragment", "wrapper", "font", "sep", "width", "budget", "verdict"))
print("-" * 100)

bad = []
for name in sorted(frags):
    rotated, font, sep, _ = frags[name]
    if name not in measured:
        print("%-30s MEASUREMENT FAILED" % name)
        bad.append(name)
        continue
    f, s, width = measured[name]
    budget = textheight if rotated else textwidth
    head = budget - width
    if head >= 0:
        verdict = "OK (%.1f pt spare)" % head
    elif name.startswith("league_") and rotated:
        verdict = ("OVER by %.1f pt - adjustbox will SHRINK it by %.1f%%"
                   % (-head, 100.0 * width / budget - 100.0))
        bad.append(name)
    else:
        verdict = "OVER by %.1f pt" % -head
        bad.append(name)
    print("%-30s %-9s %-7s %-8s %9.1f %10.1f  %s"
          % (name, "rotated" if rotated else "portrait",
             f.replace("\\", ""), s, width, budget, verdict))

print("-" * 100)
if bad:
    print("%d table(s) do not fit: %s" % (len(bad), ", ".join(bad)))
    print("Widen the columns, drop a font size, or shorten the labels.")
    sys.exit(1)
print("All %d table fragments fit their page block." % len(frags))
