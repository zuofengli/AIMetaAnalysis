# publication_v2 流水线操作指南

本文件面向课题组内部使用，说明**如何用本目录的代码一键重跑全部数据**，以及
**新的临床研究数据来了以后怎么快速更新论文**。稿件本身的投稿格式说明见
`README.md`。

---

## 1. 这套代码是干什么的

`publication_v2/` 是这篇网状 Meta 分析（NMA）的**唯一数据生产链**。论文里的
所有数字、表格、图，都由这里的脚本从原始数据自动算出，**没有一个是手抄的**。

```
inst/extdata/clinical_studies_data.xlsx        ← 唯一数据源（原始研究数据）
        │
        ▼  analysis/00_curate.R                 （按 analysis/trial_registry.R 的登记表整理）
analysis/curated/*.csv                         ← 整理后的分析用数据（真源）
        │
        ├─ analysis/01_nma_freq_binary.R       ┐
        ├─ analysis/02b_netmeta_all.R          │ 频率学派 NMA
        ├─ analysis/02_nma_freq_survival.R     ┘
        ├─ analysis/03_nma_bayes.R             ← 贝叶斯 NMA（gemtc/JAGS，最慢）
        ├─ analysis/04_figures.R               ← 图 2–5（PDF）
        └─ analysis/05_diagnostics.R           ← 收敛诊断、一致性、取整敏感性
        │
        ▼
analysis/results/*.csv                         ← 全部数值结果
        │
        ├─ analysis/make_latex_tables.py       → <稿件>/tables/league_*.tex
        ├─ analysis/make_supp_tables.py        → <稿件>/tables/supp_s*.tex
        └─ analysis/check_table_fit.py         ← 校验：每张表是否还装得下（不合格即报错）
        │
        ▼
writeup/main.tex + writeup/supplementary.tex   ← 用 \input 引用上述表格与图
```

> **`<稿件>` = 仓库之外的稿件目录**，默认 `../../manuscript_v2`
> （即 `999.manuscript/writeup/manuscript_v2/`）。稿件有意放在仓库外面，
> 以免被误提交进公开的数据/代码仓库；可用环境变量 `AIMETA_TEX_DIR` 覆盖，
> 脚本一律按相对路径解析，不写死盘符。

核心设计：**表格和图都是 `\input` / `\includegraphics` 进来的**，所以只要重跑
脚本，稿子里的表格数字自动同步，不可能出现"正文和表格对不上"。

---

## 2. 环境要求

| 组件 | 版本 | 本机位置 |
|---|---|---|
| R | 4.5.x | `D:\tools\R\R-4.5.2\bin\Rscript.exe` |
| R 包 | gemtc, rjags, netmeta(3.6-1), NMA, coda, ggplot2, dplyr, tidyr, readxl | 已装于 `D:\tools\R\R-4.5.2\library` |
| JAGS | 4.3.2 | `C:\Program Files\JAGS\JAGS-4.3.2` |
| Python | 3.x（仅用标准库） | 任意；脚本会自己找 |
| TeX | MiKTeX（pdflatex / bibtex） | `D:\tools\MiKTeX\miktex\bin\x64` |

> 项目路径**可以整体搬移**——所有脚本的路径都是相对脚本自身位置推导的，
> 不依赖任何硬编码盘符。

---

## 3. 一键重跑

### 方式 A：命令行（推荐）

```bash
cd <本目录>
Rscript run_pipeline.R                # 完整跑（含贝叶斯 MCMC，约 5–40 分钟）
Rscript run_pipeline.R --fast         # 跳过贝叶斯步骤，只跑频率学派 + 图表（约 1–2 分钟）
Rscript run_pipeline.R --pdf          # 跑完再自动编译 main.pdf / supplementary.pdf
Rscript run_pipeline.R --fast --pdf   # 数据没动、只想刷新表图和 PDF 时最常用
```

### 方式 B：双击

双击 `run_pipeline.bat` 即可（会自动寻找 Rscript）。`--fast` / `--pdf` 参数也可以
用命令行加。

### 跑完看哪里

* 数值：`analysis/results/`
* 图：`<稿件>/figures/fig2..fig6*.pdf`（fig2 = 网络拓扑图，`fig2_network_geometry` + `fig2_network_legend`）
* 表：`<稿件>/tables/*.tex`
* PDF（加了 `--pdf` 才有）：`<稿件>/main.pdf`、`<稿件>/supplementary.pdf`
* 重跑日志：终端输出，或自行重定向

---

## 4. 有新的临床研究数据来了，怎么更新？

### 4.1 把数据填进 Excel

编辑 `inst/extdata/clinical_studies_data.xlsx`，涉及 4 张表：

| 工作表 | 作用 | 新增研究要做的事 |
|---|---|---|
| `studies_info` | 每一条**发表记录**一行 | 加一行；`unique_id` 必须唯一，格式 `<试验名>-<年份>-<第几篇>`，如 `KEYNOTE-671-2023-1`；填好 `trial / pub_year / article_id / n_total / arm_n_* / Experimental / Comparator` 等 |
| `binary_data` | 二分类结局（长表） | 按 `outcome × subgroup × treatment` 逐行填 `d`（事件数）和 `n`（分母）。pCR 用 `PathologicalCompleteRespons`、MPR 用 `MajorPathologicResponse`、安全性用 `AdverseEvents` / `TreatmentRelatedAdverseEvents`，`subgroup` 填 `Overall`（安全性另见 4.3） |
| `survival` | 生存结局的 HR | 填 `treat1 / treat2 / hr / hr_lci / hr_uci`，`outcome` 为 `EventFreeSurvival` 或 `OverallSurvival`，`subgroup = "Overall"` |
| `raw_data` | 试验层面的分类信息 | 若新试验的 `comparator_category` 等分类是新值，补一行 |

> **注意**：`treatment` 字段里实验臂/对照臂要能对上。全局的臂映射来自
> `studies_info` 的 `Experimental` / `Experimental_Test_Huaxi_Mei` 与
> `Comparator` / `Comparator_Test_HuaXi_Mei` 列（后者非空时优先）。

### 4.2 在登记表里登记这个研究

打开 **`analysis/trial_registry.R`**（这是唯一需要改的代码文件），按注释把新研究的
`unique_id` 加到对应的结局清单里：

* `ids_histo` —— 进 pCR / MPR 网络
* `ids_trae_any`、`ids_ae_any`、`ids_ae_g34` —— 进各自的安全性网络
* `expected_trae_any` / `expected_trae_g34` —— **填该研究已发表的不良事件绝对数**
  （`d_exp` / `d_ctrl`）。这一步是为了在源表多种记录口径中唯一锁定正确那一行；
  漏填会导致该研究的安全性数据取不到。
* `efs_keys` / `os_keys` —— 用 `list(id = "<unique_id>", hr = <已发表的HR点估计>)`
  指定用哪条记录、哪个 HR。同一试验有多个 HR（亚组/更新分析）时靠这个精确匹配。
* `trial_chars_order` —— 让新试验出现在基线特征表（Table 1）里。

### 4.3 安全性口径的坑

源表里安全性数据存在**多种口径**（任何原因 vs 治疗相关；3–4 级 vs ≥3 级）。
`00_curate.R` 里的 `grade_rule` 已按试验分别处理（KEYNOTE-671、RATIONALE-315
有单独规则）。若新试验口径特殊，需要在 `00_curate.R` 的 `ae_g34` 规则里补一条
`case_when` 分支——这是目前唯一可能需要改第二个文件的情况。

### 4.4 如果引入了新药物

编辑 **`analysis/config/treatments.csv`**，加一行：

```
新药名,7,旧写法别名
```

`order` 决定该药在联赛表里的行列位置，`aliases` 用于把源表里的历史错拼
（如 `Duravlumab`、`Tisleizumab`）归一到规范名。R 和 Python 两个脚本都读这个文件。

### 4.5 重跑

```bash
Rscript run_pipeline.R
```

跑完 `<稿件>/tables/` 和 `<稿件>/figures/` 就是最新的；正文段落里若引用了
具体数字（如摘要、Results 段），需要**人工核对一遍**——那些是硬编码在
`main.tex` 里的文字，脚本不会自动改。

> **建议**：正文数字尽量改成由脚本生成（`\Sexpr{}` 或让表格承担），可以彻底
> 消除这一处人工同步。目前 `main.tex` 的正文仍以硬编码数字为主。

---

## 5. 编译 PDF

用 `--pdf` 参数可自动完成（见第 3 节）。手工编译等价于：

```bash
cd ../../manuscript_v2        # 稿件目录（在仓库之外）
pdflatex -interaction=nonstopmode main && bibtex main \
  && pdflatex -interaction=nonstopmode main && pdflatex -interaction=nonstopmode main
pdflatex -interaction=nonstopmode supplementary && bibtex supplementary \
  && pdflatex -interaction=nonstopmode supplementary && pdflatex -interaction=nonstopmode supplementary
```

需要 `booktabs`、`natbib`、`tikz` 宏包。生成的 PDF 用
`../SumatraPDF-3.6.1-64.exe` 打开（SumatraPDF 只是阅读器，不负责编译）。

当前基线：`main.pdf` 22 页、`supplementary.pdf` 12 页，无错误、表格无越界。

> **已修复的编译阻断 bug**：`make_latex_tables.py` 里 8 条联赛表 caption 曾写成
> 未转义的 `95% CI`。`%` 在 LaTeX 中是注释符，会把 `\caption{}` 的右花括号一起
> 注释掉，导致 `Runaway argument` 致命错误、**整稿无法编译**。现已改为 `95\%`。
> 修改 caption 文案时务必转义 `%`、`&`、`_`、`#`。

### 5.1 宽表为什么是横排的

NMA 的 league table 是「药 × 药」方阵，7 个药就是 7 个数据列 + 1 个行标列，
每格还要放 `OR (95% CI)`，整表自然宽度 **约 720–775 pt**；而竖排版心只有
**483 pt**，即使把字号压到 `\scriptsize` 也塞不下。所以 8 张 league table
全部用 `sidewaystable` **旋转 90° 印成横排**（这是期刊对宽表的通行做法）。

旋转后可用的横向空间等于版心**高度** 664 pt，因此规格定为：

| 参数 | 取值 | 说明 |
|---|---|---|
| 浮动体 | `sidewaystable[p]` | 独占一页横排，页眉页脚仍竖直 |
| 字号 | `\scriptsize` | 实测最宽的 pCR 表 642.7 pt，余量 21.4 pt |
| 列距 | `\tabcolsep = 4pt` | 默认 6pt 时 pCR 表会到 674.7 pt，超了 |
| 安全网 | `adjustbox` `max width=\textheight` | 万一以后数据把表撑宽，自动缩到装得下而不越界 |

**Table 1（试验特征表）保持竖排**——它是散文式表格，把列宽算术修对就能装下
（原版固定列宽 15.50 cm + 列距 3.39 cm = 18.89 cm，比 17.04 cm 的版心多出
52.5 pt）。现改为 `\footnotesize` + `\tabcolsep=2.5pt` + 固定列宽合计 13.55 cm。

改表宽时**不要凭感觉调**，跑一次校验即可：

```bash
python analysis/check_table_fit.py     # 逐张报「宽度 / 预算 / 余量」
```

---

## 6. 一致性核查（改数据后建议做一次）

`run_pipeline.R` 现在自带两道关，**不用你手动查**：

1. 出表之后自动跑 `check_table_fit.py`——任何一张表装不下就直接报
   `STEP FAILED`，并在结尾的 failure 列表里点名；
2. 带 `--pdf` 时，编译完自动扫 `main.log` / `supplementary.log`，把
   `Overfull \hbox` 逐条列出；若出现 `in alignment` 型（= 表格比版心宽）
   则判为失败。

想看人工的逐字节比对，仍可照旧：

```bash
# 1) 重跑前把现有产物留一份
cp -r analysis/results _baseline/results
cp -r ../../manuscript_v2/tables  _baseline/tables
# 2) 重跑
Rscript run_pipeline.R
# 3) 逐字节比对，确认只有你预期的文件发生变化
diff -r _baseline/results analysis/results
diff -r _baseline/tables  ../../manuscript_v2/tables
```

> 注意：`<稿件>/tables/league_*.tex` 的**包装行**（`sidewaystable` / 字号 /
> 列距）与数据行在同一次生成里，所以 diff 会同时看到这两类变化。
> 只想比数据，可先跑 `python ../_verify_layout_only.py`（若已删除该脚本，
> 直接看 diff 中 `\toprule` 之后的行即可）。

（本目录当前的 `_baseline/` 就是 2026-10-02 首次打通流水线时留的回归快照，
可用于对照。确认无误后可随时删除。）

**预期行为**：

* `analysis/curated/*.csv` 与 `results/freq_*`、`results/netmeta_*`、`<稿件>/tables/*`
  是**确定性**的——数据没变就必须逐字节一致。
* `results/bayes_*` 是 MCMC 结果，已固定随机种子（`BAYES_SEED`，默认
  `20250904`，见 `03_nma_bayes.R`），因此**同机同版本可复现**；但仍可能存在
  极小的 Monte Carlo 波动。若需要新的抽样实现，设环境变量 `BAYES_SEED` 换种即可。

---

## 7. 已知注意事项

* **路径不可含特殊字符**：本机路径含中文（`办公`）。脚本已加 Windows locale
  守卫（检测到 `LC_ALL=C.UTF-8` 这类无效 locale 时自动切到
  `English_United States.utf8`），否则 R 会把中文路径的高位字节截掉，
  导致子进程找不到文件。**不要删除各脚本顶部的 locale 守卫代码。**
* **`_baseline/` 是回归基线快照**，不是流水线的一部分，可以随时删除；
  重新建立基线请在改动前复制当前产物。
* **JAGS 的模型初始化需要时间**，`03_nma_bayes.R` 要跑 13 个模型（约 10–30 分钟），
  属正常现象。
* `results/bayes_run_log.txt` 是早期手工抓的控制台日志，不参与计算，可忽略。
