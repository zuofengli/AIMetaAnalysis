# ============================================================
# update_data_dictionary.R
# 用途：在 R 包开发过程中，根据 binary_data, survival, continuous
#       三个 sheet 的实际列结构，自动生成/更新 data_dictionary sheet。
# 运行前请确保工作目录为包根目录，且 clinical_studies_data.xlsx 存在。
# ============================================================

library(openxlsx)
library(dplyr)

# 配置文件路径（请根据实际包结构调整）
excel_file <- "inst/extdata/clinical_studies_data.xlsx"

# 检查文件是否存在
if (!file.exists(excel_file)) {
  stop("Excel file not found: ", excel_file,
       "\n请确认文件路径是否正确，或先运行其他脚本生成该文件。")
}

# 辅助函数：获取 sheet 的所有列名（不读取数据）
get_sheet_columns <- function(file, sheet_name) {
  if (!file.exists(file)) return(character())
  # 只读取第一行来获取列名
  df <- tryCatch(
    read.xlsx(file, sheet = sheet_name, rows = 1, colNames = TRUE),
    error = function(e) NULL
  )
  if (is.null(df)) return(character())
  names(df)
}

# 定义三个数据 sheet
data_sheets <- c("binary_data", "survival", "continuous")
sheet_cols <- list()
for (sht in data_sheets) {
  cols <- get_sheet_columns(excel_file, sht)
  if (length(cols) > 0) {
    sheet_cols[[sht]] <- cols
  } else {
    sheet_cols[[sht]] <- character()
    warning(sht, " sheet 不存在或为空，将忽略。")
  }
}

# 加载或创建 workbook
wb <- loadWorkbook(excel_file)

# 读取现有的 data_dictionary（如果存在）
if ("data_dictionary" %in% names(wb)) {
  dict_df <- read.xlsx(excel_file, sheet = "data_dictionary")
  removeWorksheet(wb, "data_dictionary")
} else {
  dict_df <- data.frame(
    data_type = character(),
    sheet_column = character(),
    constraint_type = character(),
    constraint_value = character(),
    is_critical = logical(),
    example = character(),
    notes = character(),
    status = character(),
    order = integer(),
    stringsAsFactors = FALSE
  )
}

# 确保必要的列存在
required_cols <- c("data_type", "sheet_column", "constraint_type", "constraint_value",
                   "is_critical", "example", "notes", "status", "order")
for (col in required_cols) {
  if (!col %in% names(dict_df)) {
    dict_df[[col]] <- NA_character_
  }
}
dict_df$is_critical <- as.logical(dict_df$is_critical)
dict_df$order <- as.numeric(dict_df$order)

# 对每个数据 sheet 同步列信息
for (dtype in names(sheet_cols)) {
  cols <- sheet_cols[[dtype]]
  existing_rows <- dict_df %>% filter(data_type == dtype)
  existing_cols <- existing_rows$sheet_column

  # 1) 添加新列
  new_cols <- setdiff(cols, existing_cols)
  if (length(new_cols) > 0) {
    # 获取当前该 dtype 的最大 order
    max_order <- if (nrow(existing_rows) > 0) max(existing_rows$order, na.rm = TRUE) else 0
    for (i in seq_along(new_cols)) {
      new_row <- data.frame(
        data_type = dtype,
        sheet_column = new_cols[i],
        constraint_type = "description",
        constraint_value = "待补充描述",
        is_critical = FALSE,
        example = NA_character_,
        notes = NA_character_,
        status = "active",
        order = max_order + i,
        stringsAsFactors = FALSE
      )
      dict_df <- bind_rows(dict_df, new_row)
    }
  }

  # 2) 重新排 order，使其与数据 sheet 中的列顺序一致
  # 获取该 dtype 的所有 active 行
  active_rows <- dict_df %>% filter(data_type == dtype, status == "active")
  if (nrow(active_rows) > 0) {
    # 根据 cols 顺序生成新 order
    active_rows$new_order <- match(active_rows$sheet_column, cols)
    active_rows <- active_rows %>% filter(!is.na(new_order))
    for (j in 1:nrow(active_rows)) {
      dict_df <- dict_df %>%
        mutate(order = ifelse(data_type == dtype & sheet_column == active_rows$sheet_column[j],
                              active_rows$new_order[j], order))
    }
  }

  # 3) 标记已删除的列
  deleted_cols <- setdiff(existing_cols, cols)
  if (length(deleted_cols) > 0) {
    dict_df <- dict_df %>%
      mutate(status = ifelse(data_type == dtype & sheet_column %in% deleted_cols,
                             "deleted", status))
  }
}

# 重新排序字典：先按 data_type，再按 order 升序
dict_df <- dict_df %>%
  arrange(data_type, order) %>%
  select(data_type, sheet_column, constraint_type, constraint_value,
         is_critical, example, notes, status, order)

# 添加新的 data_dictionary sheet
addWorksheet(wb, "data_dictionary")
writeData(wb, "data_dictionary", dict_df, colNames = TRUE)

# 设置表头样式（加粗）
header_style <- createStyle(fontSize = 11, textDecoration = "bold")
addStyle(wb, sheet = "data_dictionary", style = header_style, rows = 1,
         cols = 1:ncol(dict_df), gridExpand = TRUE)

# 设置列宽
setColWidths(wb, sheet = "data_dictionary",
             cols = 1:ncol(dict_df),
             widths = c(15, 20, 18, 30, 10, 20, 30, 12, 8))

# 条件格式：对 status == "deleted" 的行应用灰色填充和灰色字体
deleted_rows <- which(dict_df$status == "deleted")
if (length(deleted_rows) > 0) {
  gray_fill <- createStyle(fgFill = "lightgray", fontColour = "gray50")
  for (row in deleted_rows + 1) {  # +1 因为表头占第1行
    addStyle(wb, sheet = "data_dictionary", style = gray_fill,
             rows = row, cols = 1:ncol(dict_df), gridExpand = TRUE)
  }
}

# 保存文件
saveWorkbook(wb, excel_file, overwrite = TRUE)

message("data_dictionary 已成功更新！文件位置：", excel_file)
