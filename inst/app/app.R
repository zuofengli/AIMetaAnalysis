# ==================== app.R ====================
library(shiny)
library(shinyjs)
library(DT)
library(meta)
library(grid)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(metafor)
if (!requireNamespace("metaviz", quietly = TRUE)) {
  warning("metaviz 包未安装，将无法使用 metaviz 绘图选项。")
} else {
  library(metaviz)
}

# ---------- 加载模块 ----------
source("modules/network_meta_module.R")
source("modules/bayesian_meta_module.R")
source("modules/snapshot_module.R")
source("modules/data_loading.R")
source("modules/data_selection_module.R")
source("modules/classical_meta_module.R")


.current_snapshot_to_save <- NULL

# ---------- UI ----------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("NSCLC 围手术期免疫治疗荟萃分析"),

  wellPanel(
    h4("1. 筛选临床研究"),
    fluidRow(
      column(4, selectInput("phase_filter", "试验分期",
                            choices = c("全部", unique(studies_info$phase)),
                            selected = "全部")),
      column(4, sliderInput("year_filter", "发表年份范围",
                            min = min(studies_info$year, na.rm = TRUE),
                            max = max(studies_info$year, na.rm = TRUE),
                            value = range(studies_info$year, na.rm = TRUE),
                            step = 1, sep = ""))
    ),
    br(),
    h4("2. 选择研究（点击行可多选）"),
    fluidRow(column(2, actionButton("select_all", "全选", icon = icon("check-square"))),
             column(2, actionButton("clear_selection", "清除", icon = icon("times")))),
    DTOutput("study_table"),
    br(),
    textOutput("data_availability")
  ),

  # 数据选择模块（2.5 节）
  dataSelectionUI("data_select"),

  # 分析选项卡
  tabsetPanel(
    id = "analysis_tab",
    tabPanel("经典荟萃分析", classicalMetaUI("classical")),
    tabPanel("网络荟萃分析", networkMetaUI("network")),
    tabPanel("贝叶斯荟萃分析", bayesianMetaUI("bayesian"))
  ),

  # 快照模块
  snapshotUI("snapshot_module")
)

# ---------- Server ----------
server <- function(input, output, session) {

  # ====== 研究表格与选择 ======
  proxy <- dataTableProxy('study_table')
  observeEvent(input$select_all, { req(filtered_studies()); selectRows(proxy, 1:nrow(filtered_studies())) })
  observeEvent(input$clear_selection, { selectRows(proxy, NULL) })

  # app.R 修改点：
  filtered_studies <- reactive({
    df <- studies_info
    if (input$phase_filter != "全部") df <- dplyr::filter(df, phase == input$phase_filter)
    df <- dplyr::filter(df, year >= input$year_filter[1], year <= input$year_filter[2])
    df
  })

  output$study_table <- renderDT({
    # 表格显示列可以加上 unique_id 或仅保留 study_display
    display_df <- filtered_studies()[, c("study_display", "phase", "year", "unique_id")]
    datatable(display_df, selection = 'multiple',
              options = list(pageLength = 10, autoWidth = TRUE,
                             columnDefs = list(list(visible = FALSE, targets = 3))), # 隐藏 unique_id 列
              rownames = FALSE, colnames = c("研究名称", "分期", "年份", "ID"))
  })

  selected_study_ids <- reactive({
    rows <- input$study_table_rows_selected
    if (is.null(rows)) return(NULL)
    filtered_studies()[rows, "unique_id", drop = TRUE]
  })

  # ====== 调用数据选择模块 ======
  data_selected <- dataSelectionServer(
    id = "data_select",
    studies_info = studies_info,
    binary_data = binary_data,
    survival_data = survival_data,
    selected_study_ids = selected_study_ids
  )


  # 提取当前数据用于显示可用研究数量
  output$data_availability <- renderText({
    ds <- data_selected()
    req(ds)
    cur <- ds$current_data
    if (is.null(cur)) count <- 0 else count <- length(unique(cur$study_id))
    paste("当前筛选结果中，", count, "项研究有可用的数据。")
  })

  classical <- classicalMetaServer("classical",
                                   data_type        = reactive(data_selected()$data_type),
                                   outcome          = reactive(data_selected()$outcome),
                                   subgroup         = reactive(data_selected()$subgroup),
                                   study_group_var  = reactive(data_selected()$study_group_var),
                                   selected_studies  = reactive(data_selected()$current_data$unique_id),  # ✅ 修改
                                   current_data     = reactive(data_selected()$current_data)
  )

  network <- networkMetaServer("network",
                               data_type      = reactive(data_selected()$data_type),
                               outcome        = reactive(data_selected()$outcome),
                               selected_studies = reactive(data_selected()$current_data$unique_id),   # ✅ 已修正
                               current_data   = reactive(data_selected()$current_data),
                               studies_info   = studies_info                                         # ✅ 新增
  )

  bayesian <- bayesianMetaServer(
    "bayesian",

    data_type = reactive(data_selected()$data_type),

    current_data = reactive(data_selected()$current_data),

    studies_info = reactive(data_selected()$studies_info),

    selected_studies = reactive(
      unique(data_selected()$current_data$unique_id)
    )
  )

  # 切换活跃结果
  active_meta_result <- reactive({
    switch(input$analysis_tab,
           "经典荟萃分析" = classical$meta_result(),
           "网络荟萃分析" = network$meta_result(),
           "贝叶斯荟萃分析" = bayesian$meta_result()
    )
  })

  active_model <- reactive({
    switch(input$analysis_tab,
           "经典荟萃分析" = classical$model(),
           "网络荟萃分析" = network$model(),
           "贝叶斯荟萃分析" = bayesian$model()
    )
  })

  # 快照模块
  snapshots <- snapshotServer("snapshot_module",
                              meta_result = active_meta_result,
                              data_type = reactive(data_selected()$data_type),
                              outcome = reactive(data_selected()$outcome),
                              subgroup_name = reactive({
                                sg <- data_selected()$subgroup
                                if (is.null(sg) || sg == "") "Overall" else sg
                              }),
                              model = active_model
  )
}

shinyApp(ui = ui, server = server)
