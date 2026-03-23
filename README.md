
# 开发用
source("renv/activate.R")

# 选择一个离你较近的镜像，例如清华大学镜像源
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
renv::install("devtools")
renv::install("rentrez")
renv::install(c("shiny", "shinyjs", "DT", "httr", "jsonlite", "pdftools", "rentrez", "xml2"))
renv::install(c("tidyverse"))
renv::install(c("rhandsontable"))
#
# 1. 仅创建空模板（无自动填充）
create_study_structure("CheckMate816", "primary")

# 2. 通过 PMID 自动填充基本信息（推荐）
create_study_structure("CheckMate816", "35403841", pmid = "35403841")

# 3. 为同一研究的另一篇文章创建元数据（例如长期随访结果）
create_study_structure("CheckMate816", "longterm_followup", pmid = "另一篇文章的PMID")

# 4. 如果已存在但想强制重新生成（例如修正信息）
create_study_structure("CheckMate816", "39778121", pmid = "39778121", overwrite = TRUE)

pmid = "39778121"
pubmed_search<-rentrez::entrez_summary(db = "pubmed", id = as.character(pmid))
rlt$title


# 步骤一：创建研究
## 研究名称不再由用户输入，而是从 studies 文件夹下的所有子文件夹名中选择（即已有研究）。
## 如果没有需要的研究，用户可以新建一个临床研究（即输入新研究名称，创建新文件夹）。

# 步骤二：添加文献
## 用户先选择研究（从下拉列表）。
## 输入 PMID，点击 check 按钮。
## 系统通过 rentrez 获取该文献的基本信息。
## 检查研究名称与 PMID 的对应关系：从获取的文献信息（标题、摘要等）中搜索研究名称（考虑大小写和空格，即不区分大小写且忽略空格）。
## 检查是否可以获得 PDF 文件，如果可以，自动下载并保存为 studies/研究名称/raw/pmid.pdf。
## 如果不能自动下载，提示用户手动上传 PDF，并要求文件命名为 pmid.pdf，保存到相同路径。
## 同时，根据获取的文献信息（如第一作者、年份、标题等）更新 01_study_info.csv 中的基本信息（如果该研究已有该文献的信息，可能还要考虑去重或更新逻辑）。

# 步骤三：创建信息提取的prompt
## 表：article_base_info
"pmid","study_name","year","first_author","trial_design","total_patients","drug","notes"
"","CheckMate816",NA,"","",NA,"",""
## 表：study_arm_info
Study_name Year First_author Trial_phase Total_number Arm Treatment_Design
- 其中Arm的值域为 1：干预组  2：对照组
## 表： 


library(AIMetaAnalysis)
runBinaryDataExtraction()

lessThan_65 yr ['<65 years']
