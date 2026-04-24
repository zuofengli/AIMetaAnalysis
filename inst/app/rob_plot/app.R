# ==================== app.R ====================
# 功能：偏倚风险评价可视化演示（RoB 1 / RoB 2）
#       数据来源于 clinical_studies_data.xlsx 的 studies_info 表
#       支持动态选择研究、切换评估工具、配色方案、调节图形参数
#       交通灯图研究名称水平显示（非旋转90度）

library(shiny)
library(DT)
library(robvis)
library(dplyr)
library(readxl)
library(ggplot2)

# ---------- 数据文件定位 ----------
get_data_file <- function() {
  pkg_file <- system.file("extdata", "clinical_studies_data.xlsx", package = "AIMetaAnalysis")
  if (pkg_file != "") return(pkg_file)
  rel_file <- file.path("extdata", "clinical_studies_data.xlsx")
  if (file.exists(rel_file)) return(rel_file)
  if (file.exists("clinical_studies_data.xlsx")) return("clinical_studies_data.xlsx")
  stop("无法找到 clinical_studies_data.xlsx 文件。")
}

data_file <- get_data_file()
studies_info <- read_excel(data_file, sheet = "studies_info")

# ---------- 数据预处理 ----------
if ("trial" %in% names(studies_info)) {
  studies_info$study_name <- studies_info$trial
} else if ("study" %in% names(studies_info)) {
  studies_info$study_name <- studies_info$study
} else {
  stop("studies_info 表必须包含 'trial' 或 'study' 列。")
}

if ("pub_year" %in% names(studies_info)) {
  studies_info$year <- as.integer(studies_info$pub_year)
} else if ("year" %in% names(studies_info)) {
  studies_info$year <- as.integer(studies_info$year)
} else {
  studies_info$year <- NA_integer_
}

studies_info <- studies_info %>%
  mutate(
    study_id = paste(study_name, year, sep = "_"),
    study_display = paste0(study_name, " (", year, ")")
  )

# 确保偏倚评价字段存在
rob1_cols <- c("rob1_random_seq", "rob1_alloc_conceal", "rob1_blind_partic",
               "rob1_blind_outcome", "rob1_incomplete", "rob1_select_report", "rob1_other")
rob2_cols <- c("rob2_randomization", "rob2_deviations", "rob2_missing",
               "rob2_measurement", "rob2_selection", "rob2_overall", "rob2_weight")

for (col in c(rob1_cols, rob2_cols)) {
  if (!col %in% names(studies_info)) studies_info[[col]] <- NA_character_
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("偏倚风险评价可视化 (RoB 1 / RoB 2) - 发表级质量"),
  sidebarLayout(
    sidebarPanel(
      h4("研究选择"),
      DTOutput("study_table"),
      hr(),
      radioButtons("rob_tool", "评估工具",
                   choices = c("Cochrane RoB 1" = "rob1",
                               "Cochrane RoB 2" = "rob2"),
                   selected = "rob1", inline = TRUE),
      radioButtons("plot_type", "图形类型",
                   choices = c("交通灯图" = "summary", "加权条形图" = "bar"),
                   selected = "summary", inline = TRUE),
      radioButtons("colour_scheme", "配色方案",
                   choices = c("Cochrane 标准" = "cochrane",
                               "色盲友好" = "colourblind"),
                   selected = "cochrane", inline = TRUE),
      hr(),
      h4("高级选项"),
      conditionalPanel(
        condition = "input.plot_type == 'summary'",
        sliderInput("psize", "交通灯点大小", min = 5, max = 30, value = 15, step = 1)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'bar'",
        checkboxInput("weighted_bar", "加权条形图 (按研究精度)", value = FALSE),
        checkboxInput("overall_bar", "显示总体偏倚判断", value = FALSE)
      ),
      br(),
      downloadButton("download_plot", "下载图片 (PNG, 300 DPI)")
    ),
    mainPanel(
      plotOutput("rob_plot", height = "600px")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {

  output$study_table <- renderDT({
    display_df <- studies_info %>%
      select(study_display, phase, year) %>%
      filter(!is.na(study_display))
    datatable(
      display_df,
      selection = 'multiple',
      options = list(pageLength = 10),
      rownames = FALSE,
      colnames = c("研究名称", "分期", "年份")
    )
  })

  selected_ids <- reactive({
    rows <- input$study_table_rows_selected
    if (is.null(rows) || length(rows) == 0) return(NULL)
    studies_info[rows, "study_id", drop = TRUE]
  })

  rob_data <- reactive({
    req(selected_ids())
    df <- studies_info %>% filter(study_id %in% selected_ids())

    if (input$rob_tool == "rob1") {
      rob_df <- df %>%
        select(
          Study = study_name,
          any_of(c(
            D1 = "rob1_random_seq",
            D2 = "rob1_alloc_conceal",
            D3 = "rob1_blind_partic",
            D4 = "rob1_blind_outcome",
            D5 = "rob1_incomplete",
            D6 = "rob1_select_report",
            D7 = "rob1_other"
          ))
        )
      expected_cols <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7")
      for (col in expected_cols) {
        if (!col %in% names(rob_df)) rob_df[[col]] <- NA_character_
      }
      rob_df <- rob_df %>%
        mutate(across(starts_with("D"), as.character)) %>%
        mutate(across(starts_with("D"), ~ ifelse(. %in% c("L", "H", "U"), ., ""))) %>%
        mutate(Overall = "", Weight = 1) %>%
        select(Study, D1, D2, D3, D4, D5, D6, D7, Overall, Weight)

      attr(rob_df, "domain_names") <- c(
        "Random sequence generation",
        "Allocation concealment",
        "Blinding of participants/personnel",
        "Blinding of outcome assessment",
        "Incomplete outcome data",
        "Selective reporting",
        "Other bias"
      )
    } else {
      rob_df <- df %>%
        select(
          Study = study_name,
          any_of(c(
            D1 = "rob2_randomization",
            D2 = "rob2_deviations",
            D3 = "rob2_missing",
            D4 = "rob2_measurement",
            D5 = "rob2_selection",
            Overall = "rob2_overall",
            Weight = "rob2_weight"
          ))
        )
      expected_cols <- c("D1", "D2", "D3", "D4", "D5")
      for (col in expected_cols) {
        if (!col %in% names(rob_df)) rob_df[[col]] <- NA_character_
      }
      if (!"Overall" %in% names(rob_df)) rob_df$Overall <- NA_character_
      if (!"Weight" %in% names(rob_df)) rob_df$Weight <- 1

      rob_df <- rob_df %>%
        mutate(across(c(starts_with("D"), Overall), as.character)) %>%
        mutate(across(c(starts_with("D"), Overall), ~ {
          cleaned <- toupper(.)
          ifelse(cleaned %in% c("L", "S", "H"), cleaned, "")
        })) %>%
        mutate(Weight = suppressWarnings(as.numeric(Weight))) %>%
        mutate(Weight = ifelse(is.na(Weight), 1, Weight)) %>%
        select(Study, D1, D2, D3, D4, D5, Overall, Weight)

      attr(rob_df, "domain_names") <- c(
        "Randomization process",
        "Deviations from intended interventions",
        "Missing outcome data",
        "Measurement of the outcome",
        "Selection of the reported result"
      )
    }

    rob_df <- rob_df[match(df$study_name, rob_df$Study), ]

    domain_cols <- names(rob_df)[grepl("^D[0-9]+$", names(rob_df))]
    if (all(rob_df[, domain_cols] == "")) {
      showNotification("当前工具下无有效偏倚评价数据。", type = "warning")
    }
    rob_df
  })

  output$rob_plot <- renderPlot({
    req(rob_data())
    dat <- rob_data()
    if (nrow(dat) == 0) return(NULL)

    tool_type <- ifelse(input$rob_tool == "rob1", "ROB1", "ROB2")
    colour_choice <- input$colour_scheme

    if (input$plot_type == "summary") {
      p <- robvis::rob_traffic_light(
        data = dat,
        tool = tool_type,
        psize = input$psize,
        colour = colour_choice
      )
      # 🔧 关键修改：强制 y 轴文本水平显示（角度0），并调整对齐
      p <- p + theme(
        axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5),
        plot.margin = margin(l = 20, unit = "pt")  # 为研究名称留出空间
      )
      p
    } else {
      robvis::rob_summary(
        data = dat,
        tool = tool_type,
        overall = input$overall_bar,
        weighted = input$weighted_bar,
        colour = colour_choice
      )
    }
  }, res = 96)

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("risk_of_bias_", input$rob_tool, "_", input$plot_type, "_",
             input$colour_scheme, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      tool_type <- ifelse(input$rob_tool == "rob1", "ROB1", "ROB2")
      colour_choice <- input$colour_scheme
      dat <- rob_data()

      if (input$plot_type == "summary") {
        p <- robvis::rob_traffic_light(
          data = dat,
          tool = tool_type,
          psize = input$psize,
          colour = colour_choice
        )
        # 🔧 下载时同样应用水平文本设置
        p <- p + theme(
          axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5),
          plot.margin = margin(l = 20, unit = "pt")
        )
      } else {
        p <- robvis::rob_summary(
          data = dat,
          tool = tool_type,
          overall = input$overall_bar,
          weighted = input$weighted_bar,
          colour = colour_choice
        )
      }
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
}

# ---------- 启动应用 ----------
shinyApp(ui = ui, server = server)
