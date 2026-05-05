# ===============================
# Multi-Type Data Extraction Assistant App
# ===============================
# 增强功能：
# - 图片可从剪贴板粘贴（需 HTTPS/localhost）或手动上传
# - 图片单独存储为 RDS 文件（避免 Excel 字符数限制）
# - 预览图更大，点击后模态框支持缩放滑块、拖拽平移、滚轮缩放

library(shiny)
library(shinyjs)
library(clipr)
library(readxl)
library(dplyr)
library(stringr)
library(openxlsx)
library(base64enc)   # 用于 base64encode

# ----------------------------------------------------------------------
# 1. File paths
# ----------------------------------------------------------------------
pkg_file <- system.file("extdata", "clinical_studies_data.xlsx", package = "AIMetaAnalysis")
if (pkg_file == "") {
  pkg_file <- file.path("extdata", "clinical_studies_data.xlsx")
  if (!file.exists(pkg_file)) pkg_file <- "clinical_studies_data.xlsx"
}
user_logs <- "user_logs.xlsx"

# ----------------------------------------------------------------------
# 2. Helper functions to read reference data (read-only)
# ----------------------------------------------------------------------
read_reference_sheet <- function(sheet_name) {
  if (!file.exists(pkg_file)) return(NULL)
  tryCatch(
    read_excel(pkg_file, sheet = sheet_name),
    error = function(e) NULL
  )
}

load_sheet_data <- function(sheet_name) {
  df <- read_reference_sheet(sheet_name)

  if (is.null(df)) {
    return(list(outcomes = character(), col_names = character()))
  }

  # ✅ 关键：统一列名（你旧项目大概率做过这一步）
  colnames(df) <- trimws(tolower(colnames(df)))

  # ✅ 安全过滤 removed
  if ("removed" %in% colnames(df)) {
    df <- df[df$removed != "yes" | is.na(df$removed), , drop = FALSE]
  }

  # ✅ outcome 也要防御
  outcomes <- if ("outcome" %in% colnames(df)) unique(df$outcome) else character()
  outcomes <- outcomes[!is.na(outcomes)]

  col_names <- colnames(df)

  list(outcomes = outcomes, col_names = col_names)
}

# ----------------------------------------------------------------------
# 3. Load data dictionary (read-only)
# ----------------------------------------------------------------------
load_data_dictionary <- function(file) {
  if (!file.exists(file)) return(list())
  sheets <- excel_sheets(file)
  if (!"data_dictionary" %in% sheets) {
    warning("data_dictionary sheet not found. Please run 'update_data_dictionary.R' to create it.")
    return(list())
  }
  dict_df <- tryCatch(
    read_excel(file, sheet = "data_dictionary"),
    error = function(e) NULL
  )
  if (is.null(dict_df) || nrow(dict_df) == 0) return(list())
  dict_df <- dict_df %>% dplyr::filter(status == "active")
  dict <- list()
  for (i in 1:nrow(dict_df)) {
    dtype <- dict_df$data_type[i]
    col <- dict_df$sheet_column[i]
    if (!dtype %in% names(dict)) dict[[dtype]] <- list()
    if (!col %in% names(dict[[dtype]])) dict[[dtype]][[col]] <- list()
    ct <- dict_df$constraint_type[i]
    cv <- dict_df$constraint_value[i]
    dict[[dtype]][[col]][[ct]] <- cv
    if ("is_critical" %in% names(dict_df)) dict[[dtype]][[col]]$is_critical <- dict_df$is_critical[i]
    if ("example" %in% names(dict_df)) dict[[dtype]][[col]]$example <- dict_df$example[i]
    if ("notes" %in% names(dict_df)) dict[[dtype]][[col]]$notes <- dict_df$notes[i]
  }
  return(dict)
}

studies_info <- read_reference_sheet("studies_info")
if (is.null(studies_info) || nrow(studies_info) == 0) {
  studies_info <- data.frame(trial = character(), pub_year = integer(),
                             phase = character(), stringsAsFactors = FALSE)
}
if ("year" %in% names(studies_info) && !"pub_year" %in% names(studies_info)) {
  studies_info$pub_year <- studies_info$year
  studies_info$year <- NULL
}
if (!"pub_year" %in% names(studies_info)) {
  studies_info$pub_year <- NA_integer_
}
studies_info$study_display <- ifelse(is.na(studies_info$pub_year),
                                     studies_info$trial,
                                     paste(studies_info$trial, studies_info$pub_year, sep = " - "))

binary_data   <- load_sheet_data("binary_data")
survival_data <- load_sheet_data("survival")
continuous_data <- load_sheet_data("continuous")

data_dict <- load_data_dictionary(pkg_file)

# ----------------------------------------------------------------------
# 4. User logs handling (图片改用 RDS 存储)
# ----------------------------------------------------------------------
ensure_user_logs <- function() {
  if (!file.exists(user_logs)) {
    empty_df <- data.frame(trial = character(), pub_year = integer(),
                           data_type = character(), outcome_name = character(),
                           section_name = character(), free_text = character(),
                           stringsAsFactors = FALSE)
    wb <- createWorkbook()
    addWorksheet(wb, "my_inputs")
    writeData(wb, "my_inputs", empty_df, colNames = TRUE)
    saveWorkbook(wb, user_logs, overwrite = TRUE)
  }
}
ensure_user_logs()

# 生成 RDS 图片文件名（与 user_logs.xlsx 同目录）
get_image_filename <- function(trial, pub_year, data_type, outcome_name, section_name) {
  fname <- paste0("user_images_", section_name, "_", trial, "_", pub_year,
                  "_", data_type, "_", outcome_name, ".rds")
  gsub("[^[:alnum:]_\\.-]", "_", fname)
}

read_user_inputs <- function(trial, pub_year, data_type = NULL, outcome_name = NULL, section_name = NULL) {
  if (!file.exists(user_logs)) return(data.frame())
  df <- tryCatch(
    read_excel(user_logs, sheet = "my_inputs"),
    error = function(e) data.frame()
  )
  if (nrow(df) == 0) return(df)
  df <- df %>% dplyr::filter(trial == !!trial, pub_year == !!pub_year)
  if (!is.null(data_type)) df <- df %>% dplyr::filter(data_type == !!data_type)
  if (!is.null(outcome_name)) df <- df %>% dplyr::filter(outcome_name == !!outcome_name)
  if (!is.null(section_name)) df <- df %>% dplyr::filter(section_name == !!section_name)

  # 读取对应图片（如果存在）
  df$image_base64 <- NA_character_
  for (i in seq_len(nrow(df))) {
    img_file <- get_image_filename(df$trial[i], df$pub_year[i],
                                   df$data_type[i], df$outcome_name[i],
                                   df$section_name[i])
    if (file.exists(img_file)) {
      df$image_base64[i] <- readRDS(img_file)
    }
  }
  df
}

save_user_input <- function(trial, pub_year, data_type, outcome_name, section_name, free_text, image_base64 = NA) {
  ensure_user_logs()
  df <- tryCatch(
    read_excel(user_logs, sheet = "my_inputs"),
    error = function(e) data.frame()
  )
  if (nrow(df) == 0) {
    df <- data.frame(trial = character(), pub_year = integer(),
                     data_type = character(), outcome_name = character(),
                     section_name = character(), free_text = character(),
                     stringsAsFactors = FALSE)
  }
  idx <- which(df$trial == trial & df$pub_year == pub_year &
                 df$data_type == data_type & df$outcome_name == outcome_name &
                 df$section_name == section_name)
  if (length(idx) > 0) {
    df[idx, "free_text"] <- free_text
  } else {
    new_row <- data.frame(trial = trial, pub_year = pub_year, data_type = data_type,
                          outcome_name = outcome_name, section_name = section_name,
                          free_text = free_text,
                          stringsAsFactors = FALSE)
    df <- bind_rows(df, new_row)
  }
  wb <- loadWorkbook(user_logs)
  writeData(wb, "my_inputs", df, startCol = 1, startRow = 1, colNames = TRUE)
  saveWorkbook(wb, user_logs, overwrite = TRUE)

  # 图片保存到 RDS 文件
  img_file <- get_image_filename(trial, pub_year, data_type, outcome_name, section_name)
  if (!is.na(image_base64) && nzchar(image_base64)) {
    saveRDS(image_base64, img_file)
  } else {
    if (file.exists(img_file)) file.remove(img_file)
  }
}

# ----------------------------------------------------------------------
# 5. UI
# ----------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  # 增强的 JavaScript：剪贴板读取 + 图片缩放平移功能
  tags$script(HTML("
    // 剪贴板读取
    Shiny.addCustomMessageHandler('readClipboardImage', function(message) {
      async function readClipboard() {
        try {
          if (!navigator.clipboard || !navigator.clipboard.read) {
            return { error: 'Clipboard API not available. Use HTTPS or localhost.' };
          }
          const items = await navigator.clipboard.read();
          for (const item of items) {
            for (const type of item.types) {
              if (type.startsWith('image/')) {
                const blob = await item.getType(type);
                return new Promise((resolve) => {
                  const reader = new FileReader();
                  reader.onloadend = () => resolve({ base64: reader.result });
                  reader.onerror = () => resolve({ error: 'Failed to read image blob.' });
                  reader.readAsDataURL(blob);
                });
              }
            }
          }
          return { error: 'No image found in clipboard.' };
        } catch (err) {
          console.error('Clipboard error:', err);
          return { error: err.message || 'Permission denied or unknown error.' };
        }
      }
      readClipboard().then(result => {
        if (result.base64) {
          Shiny.setInputValue(message.inputId, result.base64, {priority: 'event'});
        } else {
          Shiny.setInputValue('clipboard_error', result.error, {priority: 'event'});
        }
      });
    });

    // 模态框内图片缩放、拖拽、滚轮
    function initZoomableImage(modalId, imgId, sliderId, resetBtnId) {
      const img = document.getElementById(imgId);
      const slider = document.getElementById(sliderId);
      const resetBtn = document.getElementById(resetBtnId);
      if (!img || !slider) return;

      let scale = 1;
      let translateX = 0, translateY = 0;
      let isDragging = false, startX, startY;

      function updateTransform() {
        img.style.transform = `translate(${translateX}px, ${translateY}px) scale(${scale})`;
        img.style.cursor = isDragging ? 'grabbing' : 'grab';
      }

      slider.addEventListener('input', (e) => {
        scale = parseFloat(e.target.value) / 100;
        updateTransform();
      });

      resetBtn.addEventListener('click', () => {
        scale = 1;
        translateX = 0; translateY = 0;
        slider.value = 100;
        updateTransform();
      });

      img.addEventListener('mousedown', (e) => {
        isDragging = true;
        startX = e.clientX - translateX;
        startY = e.clientY - translateY;
        img.style.cursor = 'grabbing';
        e.preventDefault();
      });

      window.addEventListener('mousemove', (e) => {
        if (!isDragging) return;
        translateX = e.clientX - startX;
        translateY = e.clientY - startY;
        updateTransform();
      });

      window.addEventListener('mouseup', () => {
        isDragging = false;
        img.style.cursor = 'grab';
      });

      img.addEventListener('wheel', (e) => {
        e.preventDefault();
        const delta = e.deltaY > 0 ? -5 : 5;
        let newScale = Math.min(200, Math.max(50, scale * 100 + delta));
        scale = newScale / 100;
        slider.value = newScale;
        updateTransform();
      }, { passive: false });
    }

    function copyModalImageToClipboard(imgId) {
      const img = document.getElementById(imgId);
      if (!img) return;
      const canvas = document.createElement('canvas');
      canvas.width = img.naturalWidth;
      canvas.height = img.naturalHeight;
      const ctx = canvas.getContext('2d');
      ctx.drawImage(img, 0, 0);
      canvas.toBlob(blob => {
        try {
          navigator.clipboard.write([new ClipboardItem({[blob.type]: blob})]);
          alert('Image copied to clipboard!');
        } catch (err) {
          alert('Copy failed: ' + err);
        }
      });
    }
  ")),
  titlePanel("Multi-Type Data Extraction Assistant – Generate LLM Prompt for Arm‑Level Data"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Study Information"),
      selectInput("existing_study", "Select existing study",
                  choices = c("New study", studies_info$study_display),
                  selected = "New study"),
      uiOutput("study_metadata_ui"),
      hr(),
      h4("Data Type & Outcome"),
      radioButtons("data_type", "Data type",
                   choices = c("Binary" = "binary",
                               "Survival" = "survival",
                               "Continuous" = "continuous"),
                   selected = "binary"),
      uiOutput("outcome_ui"),
      hr(),
      h4("Period"),
      textInput("period", "Period", value = "Overall"),
      br(),
      actionButton("save_inputs", "Save User Inputs", icon = icon("save")),
      br(),
      actionButton("generate_prompt", "Generate Prompt & Copy to Clipboard",
                   icon = icon("copy"), class = "btn-primary"),
      br(), br(),
      helpText("After copying the prompt, paste it into a large language model (e.g., ChatGPT).",
               "The model will return arm-level data. Copy that data and paste it into",
               "the corresponding sheet (binary_data, survival, or continuous) of your",
               "clinical_studies_data.xlsx file.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Table 1 Text",
                 br(),
                 helpText("Copy the entire Table 1 from the PDF (including headers and numbers) and paste it here. ",
                          "The formatting can be messy; the LLM will interpret it."),
                 textAreaInput("table1_text", "Table 1 text", rows = 10, width = "100%",
                               placeholder = "Paste Table 1 content here..."),
                 br(),
                 fluidRow(
                   column(4,
                          actionButton("paste_table1_img", "Paste Image from Clipboard", icon = icon("clipboard")),
                          helpText("Click to paste screenshot from clipboard.")
                   ),
                   column(4,
                          fileInput("upload_table1_img", "Or choose file",
                                    accept = c("image/png", "image/jpeg", "image/gif"))
                   ),
                   column(4,
                          actionButton("clear_table1_img", "Clear Image", icon = icon("trash"))
                   )
                 ),
                 br(),
                 uiOutput("table1_img_preview"),
                 textInput("table1_img_base64", label = NULL, value = "") %>% shinyjs::hidden(),
                 textInput("clipboard_error", label = NULL, value = "") %>% shinyjs::hidden()
        ),
        tabPanel("Subgroup Results Text",
                 br(),
                 helpText("Copy the subgroup analysis results table (forest plot data) from the PDF and paste it here. ",
                          "Include the subgroup names, categories, treatment groups, and the relevant statistics."),
                 textAreaInput("subgroup_text", "Subgroup results text", rows = 10, width = "100%",
                               placeholder = "Paste subgroup results here..."),
                 br(),
                 fluidRow(
                   column(4,
                          actionButton("paste_subgroup_img", "Paste Image from Clipboard", icon = icon("clipboard")),
                          helpText("Click to paste screenshot from clipboard.")
                   ),
                   column(4,
                          fileInput("upload_subgroup_img", "Or choose file",
                                    accept = c("image/png", "image/jpeg", "image/gif"))
                   ),
                   column(4,
                          actionButton("clear_subgroup_img", "Clear Image", icon = icon("trash"))
                   )
                 ),
                 br(),
                 uiOutput("subgroup_img_preview"),
                 textInput("subgroup_img_base64", label = NULL, value = "") %>% shinyjs::hidden()
        ),
        tabPanel("Generated Prompt",
                 br(),
                 h5("Prompt to be sent to LLM:"),
                 verbatimTextOutput("prompt_text")
        )
      )
    )
  )
)

# ----------------------------------------------------------------------
# 6. Server
# ----------------------------------------------------------------------
server <- function(input, output, session) {

  images <- reactiveValues(
    table1 = NULL,
    subgroup = NULL
  )

  study_metadata <- reactiveValues(study_id = "", trial = "", pub_year = "", phase = "")

  current_study_info <- reactive({
    if (input$existing_study == "New study") return(NULL)
    idx <- which(studies_info$study_display == input$existing_study)
    if (length(idx) == 0) return(NULL)
    list(
      trial = studies_info$trial[idx],
      pub_year = studies_info$pub_year[idx],
      phase = studies_info$phase[idx]
    )
  })

  # 剪贴板错误提示
  observeEvent(input$clipboard_error, {
    if (!is.null(input$clipboard_error) && input$clipboard_error != "") {
      showNotification(paste("Clipboard error:", input$clipboard_error),
                       type = "error", duration = 6)
    }
  })

  # 切换研究时加载已保存数据
  observeEvent(input$existing_study, {
    if (input$existing_study == "New study") {
      updateTextInput(session, "study_id", value = "")
      updateTextInput(session, "trial", value = "")
      updateTextInput(session, "pub_year", value = "")
      updateTextInput(session, "phase", value = "")
      updateTextAreaInput(session, "table1_text", value = "")
      updateTextAreaInput(session, "subgroup_text", value = "")
      images$table1 <- NULL
      images$subgroup <- NULL
      updateTextInput(session, "table1_img_base64", value = "")
      updateTextInput(session, "subgroup_img_base64", value = "")
    } else {
      info <- current_study_info()
      if (!is.null(info)) {
        updateTextInput(session, "study_id", value = info$trial)
        updateTextInput(session, "trial", value = info$trial)
        updateTextInput(session, "pub_year", value = as.character(info$pub_year))
        updateTextInput(session, "phase", value = info$phase)

        saved_table1 <- read_user_inputs(trial = info$trial, pub_year = info$pub_year,
                                         data_type = "Overall", outcome_name = "Overall",
                                         section_name = "table_one")
        if (nrow(saved_table1) > 0) {
          updateTextAreaInput(session, "table1_text", value = saved_table1$free_text[1])
          img_base64 <- saved_table1$image_base64[1]
          if (!is.na(img_base64) && img_base64 != "") {
            images$table1 <- img_base64
            updateTextInput(session, "table1_img_base64", value = img_base64)
          } else {
            images$table1 <- NULL
            updateTextInput(session, "table1_img_base64", value = "")
          }
        }
      }
    }
  })

  observeEvent(list(input$existing_study, input$data_type, input$outcome), {
    if (input$existing_study != "New study") {
      info <- current_study_info()
      if (!is.null(info) && !is.null(input$outcome) && input$outcome != "") {
        saved_subgroup <- read_user_inputs(trial = info$trial, pub_year = info$pub_year,
                                           data_type = input$data_type, outcome_name = input$outcome,
                                           section_name = "subgroup")
        if (nrow(saved_subgroup) > 0) {
          updateTextAreaInput(session, "subgroup_text", value = saved_subgroup$free_text[1])
          img_base64 <- saved_subgroup$image_base64[1]
          if (!is.na(img_base64) && img_base64 != "") {
            images$subgroup <- img_base64
            updateTextInput(session, "subgroup_img_base64", value = img_base64)
          } else {
            images$subgroup <- NULL
            updateTextInput(session, "subgroup_img_base64", value = "")
          }
        } else {
          updateTextAreaInput(session, "subgroup_text", value = "")
          images$subgroup <- NULL
          updateTextInput(session, "subgroup_img_base64", value = "")
        }
      }
    }
  })

  output$study_metadata_ui <- renderUI({
    tagList(
      textInput("study_id", "Study ID (abbreviation)", value = ""),
      textInput("trial", "Trial full name", value = ""),
      textInput("pub_year", "Publication year", value = ""),
      textInput("phase", "Phase", value = "")
    )
  })

  current_outcomes <- reactive({
    dt <- input$data_type
    if (dt == "binary") return(binary_data$outcomes)
    else if (dt == "survival") return(survival_data$outcomes)
    else return(continuous_data$outcomes)
  })

  output$outcome_ui <- renderUI({
    choices <- current_outcomes()
    selectInput("outcome", "Outcome name",
                choices = c("", choices),
                selected = if (length(choices) > 0) choices[1] else "")
  })

  get_target_columns <- reactive({
    dt <- input$data_type
    if (dt == "binary") return(binary_data$col_names)
    else if (dt == "survival") return(survival_data$col_names)
    else return(continuous_data$col_names)
  })

  get_column_constraints_text <- function(dtype, colname) {
    if (!dtype %in% names(data_dict)) return("")
    col_dict <- data_dict[[dtype]][[colname]]
    if (is.null(col_dict)) return("")
    parts <- c()
    if ("data_type" %in% names(col_dict)) {
      parts <- c(parts, paste0("type: ", col_dict$data_type))
    }
    if ("allowed_values" %in% names(col_dict)) {
      parts <- c(parts, paste0("allowed: ", col_dict$allowed_values))
    }
    if ("range" %in% names(col_dict)) {
      parts <- c(parts, paste0("range: ", col_dict$range))
    }
    if ("length" %in% names(col_dict)) {
      parts <- c(parts, paste0("max length: ", col_dict$length))
    }
    if ("required" %in% names(col_dict) && tolower(col_dict$required) == "true") {
      parts <- c(parts, "required")
    }
    if ("pattern" %in% names(col_dict)) {
      parts <- c(parts, paste0("pattern: ", col_dict$pattern))
    }
    if ("description" %in% names(col_dict) && !is.na(col_dict$description)) {
      parts <- c(parts, paste0("desc: ", col_dict$description))
    }
    if ("example" %in% names(col_dict) && !is.na(col_dict$example)) {
      parts <- c(parts, paste0("e.g., ", col_dict$example))
    }
    if (length(parts) == 0) return("")
    paste0(" (", paste(parts, collapse = "; "), ")")
  }

  # ----- 图片处理：粘贴、上传、清除 -----
  observeEvent(input$paste_table1_img, {
    session$sendCustomMessage("readClipboardImage", list(inputId = "table1_img_base64"))
  })

  observeEvent(input$paste_subgroup_img, {
    session$sendCustomMessage("readClipboardImage", list(inputId = "subgroup_img_base64"))
  })

  observeEvent(input$upload_table1_img, {
    req(input$upload_table1_img)
    inFile <- input$upload_table1_img
    base64 <- paste0("data:", inFile$type, ";base64,", base64encode(inFile$datapath))
    images$table1 <- base64
    updateTextInput(session, "table1_img_base64", value = base64)
  })

  observeEvent(input$upload_subgroup_img, {
    req(input$upload_subgroup_img)
    inFile <- input$upload_subgroup_img
    base64 <- paste0("data:", inFile$type, ";base64,", base64encode(inFile$datapath))
    images$subgroup <- base64
    updateTextInput(session, "subgroup_img_base64", value = base64)
  })

  observeEvent(input$table1_img_base64, {
    if (!is.null(input$table1_img_base64) && input$table1_img_base64 != "") {
      images$table1 <- input$table1_img_base64
    }
  })

  observeEvent(input$subgroup_img_base64, {
    if (!is.null(input$subgroup_img_base64) && input$subgroup_img_base64 != "") {
      images$subgroup <- input$subgroup_img_base64
    }
  })

  observeEvent(input$clear_table1_img, {
    images$table1 <- NULL
    updateTextInput(session, "table1_img_base64", value = "")
    reset("upload_table1_img")
  })

  observeEvent(input$clear_subgroup_img, {
    images$subgroup <- NULL
    updateTextInput(session, "subgroup_img_base64", value = "")
    reset("upload_subgroup_img")
  })

  # 预览图（尺寸加大）
  output$table1_img_preview <- renderUI({
    if (is.null(images$table1)) return(NULL)
    tagList(
      tags$img(src = images$table1,
               style = "max-height: 350px; max-width: 100%; cursor: pointer; border: 1px solid #ddd; padding: 3px;",
               onclick = "Shiny.setInputValue('show_table1_modal', Math.random())"),
      helpText("点击图片可放大、缩放、拖拽查看细节。")
    )
  })

  output$subgroup_img_preview <- renderUI({
    if (is.null(images$subgroup)) return(NULL)
    tagList(
      tags$img(src = images$subgroup,
               style = "max-height: 350px; max-width: 100%; cursor: pointer; border: 1px solid #ddd; padding: 3px;",
               onclick = "Shiny.setInputValue('show_subgroup_modal', Math.random())"),
      helpText("点击图片可放大、缩放、拖拽查看细节。")
    )
  })

  # 增强的模态框（含缩放控件）
  observeEvent(input$show_table1_modal, {
    if (!is.null(images$table1)) {
      showModal(modalDialog(
        title = "Table 1 Screenshot",
        tags$div(
          style = "overflow: hidden; max-height: 75vh; display: flex; flex-direction: column;",
          tags$div(
            style = "margin-bottom: 10px; display: flex; align-items: center; gap: 10px;",
            tags$label("Zoom:"),
            tags$input(id = "table1_zoom_slider", type = "range", min = "50", max = "200", value = "100", step = "5",
                       style = "width: 200px;"),
            actionButton("table1_zoom_reset", "Reset", class = "btn-sm")
          ),
          tags$div(
            style = "overflow: auto; max-height: 60vh; border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
            tags$img(src = images$table1,
                     id = "modal_table1_img",
                     style = "max-width: 100%; height: auto; transform-origin: 0 0; transition: transform 0.05s; cursor: grab;")
          )
        ),
        footer = tagList(
          actionButton("copy_modal_table1", "Copy to Clipboard", icon = icon("copy")),
          modalButton("Close")
        ),
        size = "l",
        easyClose = TRUE
      ))
      shinyjs::runjs("
        setTimeout(function() {
          initZoomableImage('', 'modal_table1_img', 'table1_zoom_slider', 'table1_zoom_reset');
        }, 100);
      ")
    }
  })

  observeEvent(input$show_subgroup_modal, {
    if (!is.null(images$subgroup)) {
      showModal(modalDialog(
        title = "Subgroup Results Screenshot",
        tags$div(
          style = "overflow: hidden; max-height: 75vh; display: flex; flex-direction: column;",
          tags$div(
            style = "margin-bottom: 10px; display: flex; align-items: center; gap: 10px;",
            tags$label("Zoom:"),
            tags$input(id = "subgroup_zoom_slider", type = "range", min = "50", max = "200", value = "100", step = "5",
                       style = "width: 200px;"),
            actionButton("subgroup_zoom_reset", "Reset", class = "btn-sm")
          ),
          tags$div(
            style = "overflow: auto; max-height: 60vh; border: 1px solid #ccc; padding: 10px; background: #f9f9f9;",
            tags$img(src = images$subgroup,
                     id = "modal_subgroup_img",
                     style = "max-width: 100%; height: auto; transform-origin: 0 0; transition: transform 0.05s; cursor: grab;")
          )
        ),
        footer = tagList(
          actionButton("copy_modal_subgroup", "Copy to Clipboard", icon = icon("copy")),
          modalButton("Close")
        ),
        size = "l",
        easyClose = TRUE
      ))
      shinyjs::runjs("
        setTimeout(function() {
          initZoomableImage('', 'modal_subgroup_img', 'subgroup_zoom_slider', 'subgroup_zoom_reset');
        }, 100);
      ")
    }
  })

  observeEvent(input$copy_modal_table1, {
    shinyjs::runjs("copyModalImageToClipboard('modal_table1_img');")
  })

  observeEvent(input$copy_modal_subgroup, {
    shinyjs::runjs("copyModalImageToClipboard('modal_subgroup_img');")
  })

  # ----- 保存输入 -----
  observeEvent(input$save_inputs, {
    req(input$trial, input$pub_year)
    # Table 1
    if (!is.null(input$table1_text) && input$table1_text != "") {
      tryCatch({
        save_user_input(trial = input$trial,
                        pub_year = as.integer(input$pub_year),
                        data_type = "Overall",
                        outcome_name = "Overall",
                        section_name = "table_one",
                        free_text = input$table1_text,
                        image_base64 = if (!is.null(images$table1)) images$table1 else NA_character_)
      }, error = function(e) {
        showNotification(paste("保存 Table 1 失败:", e$message), type = "error")
      })
    }
    # Subgroup
    if (!is.null(input$outcome) && input$outcome != "" &&
        !is.null(input$subgroup_text) && input$subgroup_text != "") {
      tryCatch({
        save_user_input(trial = input$trial,
                        pub_year = as.integer(input$pub_year),
                        data_type = input$data_type,
                        outcome_name = input$outcome,
                        section_name = "subgroup",
                        free_text = input$subgroup_text,
                        image_base64 = if (!is.null(images$subgroup)) images$subgroup else NA_character_)
      }, error = function(e) {
        showNotification(paste("保存 Subgroup 失败:", e$message), type = "error")
      })
    }
    showNotification("User inputs saved successfully.", type = "message")
  })

  # ----- Prompt 生成（保持不变）-----
  extraction_guide_detail <- reactive({
    dt <- input$data_type
    if (dt == "binary") {
      return("Extract the event rate (%) for each treatment group and subgroup category. If 95% confidence intervals are provided (e.g., '36.8 (24.3-49.4)'), extract the rate and optionally the CI bounds into appropriate columns (e.g., rate, ci_lower, ci_upper).")
    } else if (dt == "survival") {
      guide <- "Extract the following survival-related statistics from the subgroup results table:\n"
      cols <- get_target_columns()
      if ("hr" %in% cols) guide <- paste0(guide, "   - Hazard Ratio (HR)\n")
      if ("hr_lci" %in% cols && "hr_uci" %in% cols) guide <- paste0(guide, "   - 95% CI for HR (lower and upper)\n")
      if ("n1" %in% cols && "n2" %in% cols) guide <- paste0(guide, "   - Number of patients in each group (n1, n2)\n")
      if ("treatment_1_event_number" %in% cols && "treatment_2_event_number" %in% cols) guide <- paste0(guide, "   - Number of events in each group\n")
      if ("median_follow_up_months" %in% cols) guide <- paste0(guide, "   - Median follow-up (months)\n")
      if (any(c("te", "se_te") %in% cols)) guide <- paste0(guide, "   - Log HR (te) and its SE (se_te)\n")
      guide <- paste0(guide, "Use 'n.d.' for missing values.")
      return(guide)
    } else {
      return("Extract the mean (or mean change) and SD (or 95% CI) for each group. Use 'n.d.' if missing.")
    }
  })

  prompt_text <- reactive({
    req(input$study_id, input$trial, input$pub_year, input$outcome)
    if (is.null(input$table1_text) || input$table1_text == "") {
      showNotification("Table 1 text is empty. Please paste content.", type = "warning")
      return("")
    }
    if (is.null(input$subgroup_text) || input$subgroup_text == "") {
      showNotification("Subgroup results text is empty. Please paste content.", type = "warning")
      return("")
    }

    data_type <- input$data_type
    outcome_type <- ifelse(data_type == "binary", "Binary",
                           ifelse(data_type == "survival", "Survival (time-to-event)", "Continuous"))

    target_cols <- get_target_columns()
    if (length(target_cols) == 0) {
      showNotification(paste("No columns found for", toupper(data_type), "data. Check Excel sheet."), type = "error")
      return("")
    }

    col_descriptions <- c()
    for (col in target_cols) {
      constraint_text <- get_column_constraints_text(data_type, col)
      col_descriptions <- c(col_descriptions, paste0("   - `", col, "`", constraint_text))
    }
    output_cols_str <- paste(col_descriptions, collapse = "\n")

    paste0(
      "You are an assistant that extracts arm‑level data from clinical trial subgroup tables.\n\n",
      "## Study Metadata\n",
      "- Study ID: ", input$study_id, "\n",
      "- Trial: ", input$trial, "\n",
      "- Publication year: ", input$pub_year, "\n",
      "- Phase: ", input$phase, "\n",
      "- Outcome type: ", outcome_type, "\n",
      "- Outcome: ", input$outcome, "\n",
      "- Period: ", input$period, "\n\n",
      "## Raw Table 1 (Baseline Characteristics)\n",
      "```\n", input$table1_text, "\n```\n\n",
      "## Raw Subgroup Results Table\n",
      "```\n", input$subgroup_text, "\n```\n\n",
      "## Instructions\n",
      "1. Parse the text to extract for each subgroup and treatment arm:\n",
      "   - Subgroup name, category, treatment group names\n",
      "   - Number of patients (n) from Table 1\n",
      "   - Outcome measures (as described: ", extraction_guide_detail(), ")\n",
      "2. Combine into one row per subgroup/treatment combination.\n",
      "3. If n=0 for a treatment arm, set outcome to 'n.d.'.\n",
      "4. Follow column constraints strictly.\n",
      "5. Return **plain TSV** with headers exactly as below, no extra text.\n\n",
      "### Output columns (order must match):\n",
      output_cols_str, "\n\n",
      "Now produce the TSV table:"
    )
  })

  output$prompt_text <- renderPrint({
    cat(prompt_text())
  })

  observeEvent(input$generate_prompt, {
    txt <- prompt_text()
    if (txt == "") {
      showNotification("No prompt generated. Please fill in all fields.", type = "error")
      return()
    }
    tryCatch({
      write_clip(txt)
      showNotification("Prompt copied to clipboard!", type = "message")
    }, error = function(e) {
      showNotification("Could not copy to clipboard. Select and copy manually.", type = "warning")
    })
  })
}

# ----------------------------------------------------------------------
# 7. Run app
# ----------------------------------------------------------------------
shinyApp(ui, server)
