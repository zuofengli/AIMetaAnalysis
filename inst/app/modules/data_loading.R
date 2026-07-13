# modules/data_loading.R（完整修复版）
# modules/data_loading.R 开头（已验证）
get_data_file <- function() {
  # 使用 rprojroot 查找包根目录（开发模式）
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    root <- tryCatch(
      rprojroot::find_root(rprojroot::has_file("DESCRIPTION")),
      error = function(e) NULL
    )
    if (!is.null(root)) {
      dev_file <- file.path(root, "inst", "extdata", "clinical_studies_data.xlsx")
      if (file.exists(dev_file)) {
        return(dev_file)
      }
    }
  }

  # 备选：检查当前目录下的 inst/extdata（当工作目录恰好是项目根目录时）
  if (file.exists("inst/extdata/clinical_studies_data.xlsx")) {
    return("inst/extdata/clinical_studies_data.xlsx")
  }

  # 最后回退到已安装包中的文件
  pkg_file <- system.file("extdata", "clinical_studies_data.xlsx", package = "AIMetaAnalysis")
  if (pkg_file != "") return(pkg_file)

  stop("无法找到 clinical_studies_data.xlsx 文件。")
}

data_file <- get_data_file()
print(data_file)
studies_info <- read_excel(data_file, sheet = "studies_info")
binary_data  <- read_excel(data_file, sheet = "binary_data")
survival_data <- read_excel(data_file, sheet = "survival")

# 统一年份列名
if ("pub_year" %in% names(studies_info)) studies_info <- studies_info %>% rename(year = pub_year)
if ("pub_year" %in% names(binary_data)) binary_data <- binary_data %>% rename(year = pub_year)
if ("pub_year" %in% names(survival_data)) survival_data <- survival_data %>% rename(year = pub_year)

# 数值转换
binary_numeric_cols <- intersect(c("d", "n", "n?"), names(binary_data))
if (length(binary_numeric_cols) > 0) {
  suppressWarnings(binary_data <- binary_data %>% mutate(across(all_of(binary_numeric_cols), as.numeric)))
}
survival_numeric_cols <- intersect(c("te", "se_te", "n1", "n2", "z_value", "p_value", "hr", "lower_ci", "upper_ci"), names(survival_data))
if (length(survival_numeric_cols) > 0) {
  suppressWarnings(survival_data <- survival_data %>% mutate(across(all_of(survival_numeric_cols), as.numeric)))
}

# ===== 统一研究标识列为 "study"（确保使用 trial 列） =====

# --- studies_info ---
if ("trial" %in% names(studies_info)) {
  if ("study" %in% names(studies_info)) {
    studies_info <- studies_info %>% select(-study)   # 删除旧 study
  }
  studies_info <- studies_info %>% rename(study = trial)
} else if (!"study" %in% names(studies_info)) {
  stop("studies_info 中缺少研究标识列（trial 或 study）")
}

# --- binary_data ---
if ("trial" %in% names(binary_data)) {
  if ("study" %in% names(binary_data)) {
    binary_data <- binary_data %>% select(-study)
  }
  binary_data <- binary_data %>% rename(study = trial)
} else if (!"study" %in% names(binary_data)) {
  stop("binary_data 中缺少研究标识列（trial 或 study）")
}

# --- survival_data ---
if ("trial" %in% names(survival_data)) {
  if ("study" %in% names(survival_data)) {
    survival_data <- survival_data %>% select(-study)
  }
  survival_data <- survival_data %>% rename(study = trial)
} else if (!"study" %in% names(survival_data)) {
  stop("survival_data 中缺少研究标识列（trial 或 study）")
}

# 年份处理
studies_info$year <- as.integer(studies_info$year)
if (!"year" %in% names(binary_data)) binary_data$year <- NA_integer_ else binary_data$year <- as.numeric(binary_data$year)
if (!"year" %in% names(survival_data)) survival_data$year <- NA_integer_ else survival_data$year <- as.numeric(survival_data$year)

if (!"stratified" %in% names(survival_data)) survival_data$stratified <- NA_character_

# 构建或确认 unique_id
if (!"unique_id" %in% names(studies_info)) {
  studies_info <- studies_info %>%
    mutate(unique_id = paste(study, year, article_id, sep = "-"))
}

# 为 binary_data 和 survival_data 匹配 unique_id
binary_data <- binary_data %>%
  left_join(studies_info %>% select(study, year, unique_id), by = c("study", "year")) %>%
  dplyr::filter(!is.na(unique_id))

survival_data <- survival_data %>%
  left_join(studies_info %>% select(study, year, unique_id), by = c("study", "year")) %>%
  dplyr::filter(!is.na(unique_id))

# 添加显示名称
studies_info <- studies_info %>% mutate(study_display = paste0(study, " (", year, ")"))
binary_data <- binary_data %>% mutate(study_display = paste0(study, " (", year, ")"))
survival_data <- survival_data %>% mutate(study_display = paste0(study, " (", year, ")"))

# 移除标记为剔除的研究
if ("removed" %in% names(binary_data)) {
  binary_data <- binary_data %>% dplyr::filter(is.na(removed) | removed != "YES")
}
if ("removed" %in% names(survival_data)) {
  survival_data <- survival_data %>% dplyr::filter(is.na(removed) | removed != "YES")
}

# 统一 subgroup 列
if ("group" %in% names(survival_data) && "groupOption" %in% names(survival_data)) {
  survival_data <- survival_data %>% rename(subgroup = group, subgroup_option = groupOption)
}
if (!"subgroup" %in% names(survival_data)) survival_data$subgroup <- NA
if (!"subgroup_option" %in% names(survival_data)) survival_data$subgroup_option <- NA
survival_data <- survival_data %>% mutate(subgroup = str_trim(subgroup), subgroup_option = str_trim(subgroup_option))
survival_data <- survival_data %>% mutate(te = as.numeric(te), se_te = as.numeric(se_te), n1 = as.numeric(n1), n2 = as.numeric(n2))

if (!("te" %in% names(survival_data)) || !("se_te" %in% names(survival_data)))
  stop("生存数据中必须包含 'te' 和 'se_te' 列。")
