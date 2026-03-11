#' 为元分析项目创建一个新的研究文件夹结构，并可选择通过 PMID 自动填充研究特征表
#'
#' 该函数会在 `studies/` 目录下创建一个以研究名称命名的文件夹，
#' 并在其中自动生成标准的子文件夹：`raw`、`metadata`、`data_processed`、
#' `output`、`reports`。同时，在 `metadata/<article_id>/` 下创建元数据模板。
#' 如果提供了有效的 PMID，则自动获取文献基本信息并填入研究特征表。
#'
#' @param study_name 字符串，新研究的名称（将用作文件夹名，如 "CheckMate816"）。
#' @param article_id 字符串，文章标识（如 PMID、DOI 或自定义简称），用于区分同一研究的多篇文章。
#' @param base_path 字符串，`studies` 文件夹的路径（相对于项目根目录）。默认值为 `"studies"`。
#' @param pmid 可选，PubMed ID（数字或字符串），用于自动提取文献信息填充研究特征表。
#' @param overwrite 逻辑值，如果为 `TRUE`，即使模板文件已存在也重新生成（谨慎使用）。默认为 `FALSE`。
#' @param verbose 逻辑值，是否在控制台输出创建过程的详细信息，默认为 `TRUE`。
#'
#' @return 不可见地返回实际创建的文件夹路径（字符向量）。
#' @export
#'
#' @examples
#' \dontrun{
#' # 仅创建空模板
#' create_study_structure("CheckMate816", "primary")
#'
#' # 通过 PMID 自动填充基本信息
#' create_study_structure("CheckMate816", "39778121", pmid = "39778121")
#' }
create_study_structure <- function(study_name,
                                   article_id,
                                   base_path = "studies",
                                   pmid = NULL,
                                   overwrite = FALSE,
                                   verbose = TRUE) {
  # 输入检查
  study_name <- as.character(study_name)
  if (length(study_name) != 1 || nchar(study_name) == 0) {
    stop("参数 'study_name' 必须是一个非空的字符串。")
  }
  article_id <- as.character(article_id)
  if (length(article_id) != 1 || nchar(article_id) == 0) {
    stop("参数 'article_id' 必须是一个非空的字符串。")
  }

  # 构建新研究的完整路径
  study_path <- file.path(base_path, study_name)

  # 定义需要创建的子文件夹
  subdirs <- c("raw", "metadata", "data_processed", "output", "reports")

  # 创建主研究文件夹（如果不存在）
  if (!dir.exists(study_path)) {
    dir.create(study_path, recursive = TRUE)
    if (verbose) message("创建主文件夹：", study_path)
  } else {
    if (verbose) message("主文件夹已存在：", study_path)
  }

  # 创建各个子文件夹
  created <- character()
  for (sub in subdirs) {
    sub_path <- file.path(study_path, sub)
    if (!dir.exists(sub_path)) {
      dir.create(sub_path)
      if (verbose) message("  创建子文件夹：", sub_path)
      created <- c(created, sub_path)
    } else {
      if (verbose) message("  子文件夹已存在：", sub_path)
    }
  }

  # 在 metadata 文件夹下创建文章级子文件夹
  article_meta_path <- file.path(study_path, "metadata", article_id)
  if (!dir.exists(article_meta_path)) {
    dir.create(article_meta_path, recursive = TRUE)
    if (verbose) message("  创建文章元数据文件夹：", article_meta_path)
    created <- c(created, article_meta_path)
  } else {
    if (verbose) message("  文章元数据文件夹已存在：", article_meta_path)
  }

  # --- 从 PMID 获取信息（如果提供）---
  year_val <- NA_integer_
  first_author_val <- ""
  all_authors_val <- ""
  title_val <- ""
  journal_val <- ""
  pmid_val <- if (!is.null(pmid)) as.character(pmid) else ""

  if (!is.null(pmid)) {
    # 检查并加载 rentrez 包
    if (!requireNamespace("rentrez", quietly = TRUE)) {
      stop("需要安装 'rentrez' 包才能使用 PMID 自动填充功能。请运行 install.packages('rentrez')")
    }
    if (verbose) message("    正在通过 PMID ", pmid, " 获取文献信息...")

    tryCatch({
      # 查询 PubMed
      pubmed_search <- rentrez::entrez_summary(db = "pubmed", id = as.character(pmid))

      # 提取第一作者姓名
      if (!is.null(pubmed_search$authors) && nrow(pubmed_search$authors) > 0) {
        first_author_val <- pubmed_search$authors$name[1]
        all_authors_val <- paste(pubmed_search$authors$name, collapse = " | ")
      }

      # 提取年份
      if (!is.null(pubmed_search$pubdate)) {
        pubdate <- pubmed_search$pubdate
        year_match <- regmatches(pubdate, regexpr("\\d{4}", pubdate))
        if (length(year_match) > 0) {
          year_val <- as.integer(year_match)
        }
      }

      # 提取标题和期刊
      title_val <- if (!is.null(pubmed_search$title)) pubmed_search$title else ""
      journal_val <- if (!is.null(pubmed_search$source)) pubmed_search$source else ""

      if (verbose) message("    成功获取文献信息：", first_author_val, " ", year_val)
    }, error = function(e) {
      warning("通过 PMID 获取文献信息失败：", e$message)
    })
  }

  # --- 读取项目级别的 feature 配置 ---
  config_dir <- file.path(dirname(study_path), "..", "config")
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE)
    if (verbose) message("创建配置文件夹：", config_dir)
  }

  feature_config_file <- file.path(config_dir, "study_features.csv")

  # 如果配置文件不存在，创建默认配置
  if (!file.exists(feature_config_file)) {
    default_features <- data.frame(
      feature_name = c("age_median", "stage_distribution", "pd_l1_positivity_rate",
                       "squamous_histology_rate", "median_followup", "neoadjuvant_cycles",
                       "adjuvant_cycles", "treatment_setting"),
      description = c("中位年龄", "分期分布", "PD-L1阳性率",
                      "鳞癌比例", "中位随访时间", "新辅助治疗周期数",
                      "辅助治疗周期数", "治疗设置"),
      default_unit = c("years", "", "%", "%", "months", "cycles", "cycles", ""),
      example_values = c("65, 64, 63", "Stage 2 (30%); Stage 3A (45%); Stage 3B (25%)",
                         "67, 66, 50", "49, 51, 78", "11.7, 25.2, 41.4", "3, 4", "0, 6, 12, 13",
                         "Perioperative, Neoadjuvant, Adjuvant"),
      is_required = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
      display_order = c(10, 20, 30, 40, 50, 60, 70, 80),
      stringsAsFactors = FALSE
    )
    write.csv(default_features, feature_config_file, row.names = FALSE)
    if (verbose) message("  创建默认 feature 配置文件：", feature_config_file)
  }

  # 读取 feature 配置
  feature_config <- read.csv(feature_config_file, stringsAsFactors = FALSE)
  # 按显示顺序排序
  feature_config <- feature_config[order(feature_config$display_order), ]

  # --- 1a. 研究级别基本信息表 (Study-level basic info) ---
  study_info_file <- file.path(article_meta_path, "01_study_info.csv")

  if (!file.exists(study_info_file) || overwrite) {
    # 构建 notes 字符串
    notes_parts <- c()
    if (nchar(all_authors_val) > 0) {
      notes_parts <- c(notes_parts, paste0("Authors: ", all_authors_val))
    }
    if (nchar(title_val) > 0) {
      notes_parts <- c(notes_parts, paste0("Title: ", title_val))
    }
    if (nchar(journal_val) > 0) {
      notes_parts <- c(notes_parts, paste0("Journal: ", journal_val))
    }
    notes_val <- if (length(notes_parts) > 0) paste(notes_parts, collapse = "; ") else ""

    # 创建基本信息表（只包含通用字段）
    study_info_template <- data.frame(
      pmid = pmid_val,
      study_name = study_name,
      year = year_val,
      first_author = first_author_val,
      trial_design = "",
      total_patients = NA_integer_,
      drug = "",
      notes = notes_val,
      stringsAsFactors = FALSE
    )
    write.csv(study_info_template, study_info_file, row.names = FALSE)
    if (verbose) message("    创建模板：", basename(study_info_file))
  } else if (verbose) {
    message("    模板已存在：", basename(study_info_file))
  }

  # --- 1b. 研究特征键值对表 (Study-level features in key-value format) ---
  #     根据配置文件动态生成 feature 行
  features_file <- file.path(article_meta_path, "01_study_features.csv")

  if (!file.exists(features_file) || overwrite) {
    if (nchar(pmid_val) > 0 && nrow(feature_config) > 0) {
      # 从配置文件创建 feature 行
      features_template <- data.frame(
        pmid = rep(pmid_val, nrow(feature_config)),
        feature_name = feature_config$feature_name,
        feature_value = rep("", nrow(feature_config)),
        feature_unit = feature_config$default_unit,
        notes = rep("", nrow(feature_config)),
        stringsAsFactors = FALSE
      )
    } else {
      # 无 PMID 或无配置，创建空数据框
      features_template <- data.frame(
        pmid = character(0),
        feature_name = character(0),
        feature_value = character(0),
        feature_unit = character(0),
        notes = character(0),
        stringsAsFactors = FALSE
      )
    }

    write.csv(features_template, features_file, row.names = FALSE)
    if (verbose) message("    创建模板：", basename(features_file))
  } else if (verbose) {
    message("    模板已存在：", basename(features_file))
  }

  # --- 1c. Arm级别信息表 (Arm-level details) ---
  arms_file <- file.path(article_meta_path, "01_arms.csv")

  if (!file.exists(arms_file) || overwrite) {
    # 创建空数据框（0行）或示例数据框（2行）
    if (nchar(study_name) > 0 && nchar(pmid_val) > 0) {
      # 有研究信息，创建两行示例
      arms_template <- data.frame(
        pmid = rep(pmid_val, 2),
        study_name = rep(study_name, 2),
        arm = c(1, 2),
        arm_description = c("Intervention", "Control"),
        sample_size = c(NA_integer_, NA_integer_),
        treatment_regimen = c("", ""),
        cycles = c(NA_integer_, NA_integer_),
        notes = c("", ""),
        stringsAsFactors = FALSE
      )
    } else {
      # 无研究信息，创建空数据框
      arms_template <- data.frame(
        pmid = character(0),
        study_name = character(0),
        arm = integer(0),
        arm_description = character(0),
        sample_size = integer(0),
        treatment_regimen = character(0),
        cycles = integer(0),
        notes = character(0),
        stringsAsFactors = FALSE
      )
    }

    write.csv(arms_template, arms_file, row.names = FALSE)
    if (verbose) message("    创建模板：", basename(arms_file))
  } else if (verbose) {
    message("    模板已存在：", basename(arms_file))
  }

  # --- 2. 重叠研究检查表 (Step 2) ---
  overlap_file <- file.path(article_meta_path, "02_overlap_check.csv")
  if (!file.exists(overlap_file) || overwrite) {
    overlap_template <- data.frame(
      pmid1 = character(0),
      pmid2 = character(0),
      overlap_reason = character(0),
      decision = character(0),
      notes = character(0),
      stringsAsFactors = FALSE
    )
    write.csv(overlap_template, overlap_file, row.names = FALSE)
    if (verbose) message("    创建模板：", basename(overlap_file))
  } else if (verbose) {
    message("    模板已存在：", basename(overlap_file))
  }

  # --- 3. 生存数据表 (HR 及其精度) ---
  surv_file <- file.path(article_meta_path, "03_survival_hr.csv")
  if (!file.exists(surv_file) || overwrite) {
    surv_template <- data.frame(
      pmid = character(0),
      study_name = character(0),
      parameter_name = character(0),
      subgroup = character(0),
      measure = character(0),
      effect_size = numeric(0),
      ci_lower = numeric(0),
      ci_upper = numeric(0),
      p_value = numeric(0),
      notes = character(0),
      stringsAsFactors = FALSE
    )
    write.csv(surv_template, surv_file, row.names = FALSE)
    if (verbose) message("    创建模板：", basename(surv_file))
  } else if (verbose) {
    message("    模板已存在：", basename(surv_file))
  }

  # --- 4. 二分类数据表 (事件数 + 总人数) ---
  binary_file <- file.path(article_meta_path, "04_binary_counts.csv")
  if (!file.exists(binary_file) || overwrite) {
    binary_template <- data.frame(
      pmid = character(0),
      study_name = character(0),
      parameter_name = character(0),
      subgroup = character(0),
      arm = integer(0),
      events = integer(0),
      total = integer(0),
      notes = character(0),
      stringsAsFactors = FALSE
    )
    write.csv(binary_template, binary_file, row.names = FALSE)
    if (verbose) message("    创建模板：", basename(binary_file))
  } else if (verbose) {
    message("    模板已存在：", basename(binary_file))
  }

  # --- 5. 连续型数据表 (均值 + 标准差) ---
  cont_file <- file.path(article_meta_path, "05_continuous_means.csv")
  if (!file.exists(cont_file) || overwrite) {
    cont_template <- data.frame(
      pmid = character(0),
      study_name = character(0),
      parameter_name = character(0),
      subgroup = character(0),
      arm = integer(0),
      n = integer(0),
      mean = numeric(0),
      sd = numeric(0),
      notes = character(0),
      stringsAsFactors = FALSE
    )
    write.csv(cont_template, cont_file, row.names = FALSE)
    if (verbose) message("    创建模板：", basename(cont_file))
  } else if (verbose) {
    message("    模板已存在：", basename(cont_file))
  }

  # --- 创建/更新研究级 README 文件 ---
  readme_file <- file.path(study_path, "metadata", "README_metadata.txt")
  if (!file.exists(readme_file) || overwrite) {
    # 从配置文件生成 feature 说明
    feature_descriptions <- apply(feature_config, 1, function(row) {
      sprintf("   - %-30s: %s (单位: %s, 示例: %s)",
              row["feature_name"],
              row["description"],
              ifelse(nchar(row["default_unit"]) > 0, row["default_unit"], "无"),
              row["example_values"])
    })

    readme_lines <- c(
      "此文件夹包含按照 Goldsmith & Komarova (2025) 协议设计的元数据模板，并针对临床试验 Meta 分析进行了优化。",
      "",
      "每个子文件夹以文章标识符（如 PMID、DOI 或自定义简称）命名，存放该文章独立的元数据。",
      "",
      "每个文章子文件夹内包含以下文件：",
      "",
      "【研究特征相关】",
      "01_study_info.csv：研究级别基本信息（每个研究一行）",
      "   - pmid                    : PubMed ID（唯一标识）",
      "   - study_name              : 研究名称（如 'CheckMate816'）",
      "   - year                    : 发表年份（由 PMID 自动填充）",
      "   - first_author            : 第一作者（由 PMID 自动填充）",
      "   - trial_design            : 试验设计（如 'Double blind, randomized phase III'）",
      "   - total_patients          : 研究总人数",
      "   - drug                    : 使用的免疫检查点抑制剂",
      "   - notes                   : 备注（包含完整作者、标题、期刊，由 PMID 自动填充）",
      "",
      "01_study_features.csv：研究特征键值对表（灵活存储特定领域的研究特征）",
      "   - pmid                    : PubMed ID",
      "   - feature_name            : 特征名称",
      "   - feature_value           : 特征值",
      "   - feature_unit            : 单位",
      "   - notes                   : 备注",
      "",
      "   当前项目定义的 feature（来自 config/study_features.csv）：",
      feature_descriptions,
      "",
      "01_arms.csv：各 arm 详细信息（每个 arm 一行）",
      "   - pmid                    : PubMed ID（关联到 study_info）",
      "   - study_name              : 研究名称",
      "   - arm                     : 组别编号（1, 2, 3...）",
      "   - arm_description         : 组别描述（如 'Intervention', 'Control'）",
      "   - sample_size             : 该组样本量",
      "   - treatment_regimen       : 治疗方案详情（如 'Nivolumab (360 mg) + platinum-doublet'）",
      "   - cycles                  : 治疗周期数",
      "   - notes                   : 备注",
      "",
      "【重叠检查】",
      "02_overlap_check.csv：用于标记可能重叠的研究，避免重复数据。",
      "   - pmid1, pmid2            : 可能存在重叠的两篇文章的 PMID",
      "   - overlap_reason          : 重叠原因",
      "   - decision                : 处理决定",
      "",
      "【结局数据】",
      "03_survival_hr.csv：生存分析的风险比（HR）及其 95% 置信区间",
      "   - pmid                    : PubMed ID",
      "   - study_name              : 研究名称",
      "   - parameter_name          : 参数名称（如 'EFS', 'OS'）",
      "   - subgroup                : 亚组信息",
      "   - measure                 : 效应量类型（'HR'）",
      "   - effect_size             : 风险比点估计",
      "   - ci_lower/ci_upper       : 95% 置信区间",
      "   - p_value                 : P 值",
      "",
      "04_binary_counts.csv：二分类结局数据（如 pCR、毒性事件）",
      "   - pmid                    : PubMed ID",
      "   - study_name              : 研究名称",
      "   - parameter_name          : 参数名称（如 'pCR'）",
      "   - subgroup                : 亚组信息",
      "   - arm                     : 组别编号（对应 01_arms.csv 中的 arm）",
      "   - events                  : 事件数",
      "   - total                   : 该组总人数",
      "",
      "05_continuous_means.csv：连续型结局数据（如骨密度变化）",
      "   - pmid                    : PubMed ID",
      "   - study_name              : 研究名称",
      "   - parameter_name          : 参数名称",
      "   - subgroup                : 亚组信息",
      "   - arm                     : 组别编号",
      "   - n                       : 样本量",
      "   - mean                    : 均值",
      "   - sd                      : 标准差",
      "",
      "使用说明：",
      "1. 首先填写 01_study_info.csv，记录研究的基本信息",
      "2. 然后在 01_study_features.csv 中添加该研究特有的特征",
      "3. 在 01_arms.csv 中为每个 arm 添加一行，填写各组的具体信息",
      "4. 在 04_binary_counts.csv 和 05_continuous_means.csv 中记录各 arm 的结局数据",
      "5. 在 03_survival_hr.csv 中记录生存分析的 HR 结果",
      "",
      "【管理 feature 列表】",
      "feature 列表存储在项目根目录的 config/study_features.csv 中。",
      "您可以随时编辑此文件来：",
      "   - 添加新的 feature",
      "   - 删除不需要的 feature",
      "   - 修改 feature 的描述、单位或示例值",
      "   - 调整显示顺序",
      "修改后，新创建的研究将使用更新后的 feature 列表。",
      "已有研究的 feature 文件不会自动更新，如需更新请设置 overwrite = TRUE。",
      "",
      "当同一研究有多篇文章时，请为每篇文章创建独立的子文件夹并分别填写数据。"
    )
    writeLines(readme_lines, readme_file)
    if (verbose) message("  创建/更新研究级说明文件：", basename(readme_file))
  } else if (verbose) {
    message("  研究级说明文件已存在：", basename(readme_file))
  }

  invisible(created)
}
