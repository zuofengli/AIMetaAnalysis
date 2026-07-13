# modules/synthetic_module.R
library(shiny)
library(dplyr)
library(shinyjs)

syntheticUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("synthetic_btn"), "合成数据 *", icon = icon("plus-circle"), width = "100%")
  )
}

syntheticServer <- function(id,
                            available_levels,           # reactive: 原始亚组水平
                            data_type,                  # reactive: "binary" 或 "survival"
                            base_data,                  # reactive: 原始数据（不含合成）
                            selected_order_reactive,    # reactiveVal: 亚组顺序
                            synthetic_data_reactive     # reactiveVal: 已存储的合成数据
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 用于存储待确认的合成参数（仅用于生存数据的 MH 切换确认）
    pending_synth <- reactiveVal(NULL)

    # 控制按钮启用/禁用
    observe({
      req(available_levels())
      if (length(available_levels()) == 0) {
        shinyjs::disable("synthetic_btn")
      } else {
        shinyjs::enable("synthetic_btn")
      }
    })

    # 弹出合成模态框
    observeEvent(input$synthetic_btn, {
      req(available_levels())
      showModal(
        modalDialog(
          title = "合成新的亚组水平",
          fluidRow(
            column(6,
                   selectInput(ns("synth_levels"), "选择要合并的水平（可多选）",
                               choices = available_levels(),
                               multiple = TRUE, size = 8, selectize = FALSE)
            ),
            column(6,
                   radioButtons(ns("synth_method"), "合并方法",
                                choices = c("逆方差加权法" = "invvar",
                                            "Mantel-Haenszel 法" = "mh"),
                                selected = "invvar"),
                   selectizeInput(ns("synth_name"), "新水平名称",
                                  choices = available_levels(),
                                  selected = "Combined",
                                  options = list(create = TRUE, placeholder = "请选择或输入新名称")),
                   helpText("注意：生存数据仅支持逆方差加权法；二分类数据直接合并事件数和总人数。",
                            "如果选择已存在的名称，将追加数据（不会覆盖原有数据）。")
            )
          ),
          footer = tagList(
            modalButton("取消"),
            actionButton(ns("confirm_synth"), "确认合成", class = "btn-primary")
          ),
          easyClose = FALSE
        )
      )
    })

    # -------------------------------------------------------------------
    # 核心合成函数
    # -------------------------------------------------------------------
    perform_synthesis <- function(selected_levels, new_name, method = "invvar") {
      base_df <- base_data()
      data_type_val <- data_type()

      # ---------- 防御性检查 ----------
      if (is.null(base_df) || nrow(base_df) == 0) {
        showNotification("基础数据为空，无法进行合成。", type = "error")
        return()
      }
      if (!"subgroup_level" %in% names(base_df)) {
        showNotification("当前数据不支持亚组合成（缺少 subgroup_level 列）", type = "error")
        return()
      }

      # 获取已有合成数据（若为空则创建一个与 base_df 同结构的空数据框）
      synth_data <- synthetic_data_reactive()
      if (is.null(synth_data) || nrow(synth_data) == 0) {
        synth_data <- base_df[0, , drop = FALSE]   # 空模板，列结构一致
      }

      # 筛选要合并的水平的数据
      raw_data <- base_df[base_df$subgroup_level %in% selected_levels, , drop = FALSE]
      if (nrow(raw_data) == 0) {
        showNotification("所选水平下没有可用数据。", type = "warning")
        return()
      }

      # ============================================================
      # 根据数据类型执行不同的合并逻辑
      # ============================================================
      if (data_type_val == "binary") {
        # --- 二分类数据：宽格式，按研究直接累加事件数和人数 ---
        # 检查必要列
        required_cols <- c("unique_id", "study_display", "event_ici", "n_ici", "event_ctrl", "n_ctrl")
        missing <- setdiff(required_cols, names(raw_data))
        if (length(missing) > 0) {
          showNotification(
            paste("二分类数据缺少必要列：", paste(missing, collapse = ", ")),
            type = "error"
          )
          return()
        }

        # 按研究分组，对四列计数求和
        merged <- raw_data %>%
          dplyr::group_by(unique_id, study_display) %>%
          dplyr::summarise(
            event_ici = sum(event_ici, na.rm = TRUE),
            n_ici     = sum(n_ici, na.rm = TRUE),
            event_ctrl= sum(event_ctrl, na.rm = TRUE),
            n_ctrl    = sum(n_ctrl, na.rm = TRUE),
            # 其他列（如 subgroup_level 等）取第一个值（假设在研究内不变）
            across(!any_of(c("unique_id", "study_display", "event_ici", "n_ici",
                             "event_ctrl", "n_ctrl", "subgroup_level")), ~ first(.x)),
            .groups = "drop"
          ) %>%
          dplyr::mutate(subgroup_level = new_name)

      } else if (data_type_val == "survival") {
        # --- 生存数据：逆方差加权法（基于 HR 和 se） ---
        required_cols <- c("unique_id", "study_display", "te", "se_te")
        missing <- setdiff(required_cols, names(raw_data))
        if (length(missing) > 0) {
          showNotification(
            paste("生存数据缺少必要列：", paste(missing, collapse = ", ")),
            type = "error"
          )
          return()
        }

        combine_invvar <- function(df) {
          w <- 1 / (df$se_te^2)
          te_comb <- sum(w * df$te) / sum(w)
          se_comb <- sqrt(1 / sum(w))
          data.frame(te = te_comb, se_te = se_comb,
                     n1 = NA_real_, n2 = NA_real_,
                     stringsAsFactors = FALSE)
        }

        merged <- raw_data %>%
          dplyr::group_by(unique_id, study_display) %>%
          dplyr::group_modify(~ combine_invvar(.x)) %>%
          dplyr::ungroup() %>%
          # 保留其他列（取第一个）
          left_join(
            raw_data %>%
              select(-te, -se_te, -n1, -n2, -subgroup_level) %>%
              group_by(unique_id, study_display) %>%
              slice(1) %>%
              ungroup(),
            by = c("unique_id", "study_display")
          ) %>%
          dplyr::mutate(subgroup_level = new_name)

      } else {
        showNotification("不支持的数据类型", type = "error")
        return()
      }

      # ============================================================
      # 处理已有同名水平（追加逻辑）
      # ============================================================
      # 从原始数据中取出同名水平
      existing_raw <- base_df[base_df$subgroup_level == new_name, , drop = FALSE]
      # 从合成数据中取出同名水平
      existing_synth <- synth_data[synth_data$subgroup_level == new_name, , drop = FALSE]

      # 合并原始和合成的已有数据，并对每条记录标记来源（优先保留合成数据）
      existing_all <- dplyr::bind_rows(
        existing_raw %>% mutate(source = "raw"),
        existing_synth %>% mutate(source = "synth")
      )

      if (nrow(existing_all) > 0) {
        # 定义去重键：binary 用 unique_id，survival 也用 unique_id（因为每个研究只有一行）
        key_cols <- "unique_id"
        # 按 key 去重，优先保留合成数据
        existing_all <- existing_all %>%
          dplyr::group_by(across(all_of(key_cols))) %>%
          dplyr::arrange(desc(source == "synth")) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::select(-source)
      }

      # 从新合成的数据中排除已存在的记录（按相同 key）
      if (nrow(existing_all) > 0) {
        merged <- merged %>%
          dplyr::anti_join(existing_all, by = key_cols)
      }

      # 合并已有数据和新合成数据，得到该新水平的完整数据集
      final_level_data <- dplyr::bind_rows(existing_all, merged)

      # 更新合成数据存储：移除旧版本，添加新水平的数据
      old_synth <- synth_data %>% dplyr::filter(subgroup_level != new_name)
      synthetic_data_reactive(dplyr::bind_rows(old_synth, final_level_data))

      # 自动更新亚组顺序：移除被合并的原始水平，添加新水平
      current_order <- selected_order_reactive()
      new_order <- current_order[!current_order %in% selected_levels]
      if (!(new_name %in% new_order)) {
        new_order <- c(new_order, new_name)
      }
      selected_order_reactive(new_order)

      showNotification(paste0("成功合成新水平：", new_name), type = "message")
    }

    # -------------------------------------------------------------------
    # 确认合成按钮事件
    # -------------------------------------------------------------------
    observeEvent(input$confirm_synth, {
      req(input$synth_levels, length(input$synth_levels) >= 2)
      req(base_data())

      new_name <- input$synth_name
      selected_levels <- input$synth_levels
      method_original <- input$synth_method

      if (new_name %in% selected_levels) {
        showNotification("新水平名称不能与要合并的任一水平相同。", type = "error")
        return()
      }

      # ---------- 根据数据类型决定行为 ----------
      if (data_type() == "binary") {
        # 二分类：直接执行（忽略方法选择）
        tryCatch({
          perform_synthesis(selected_levels, new_name, "binary")
          removeModal()
        }, error = function(e) {
          showNotification(paste("合成失败：", e$message), type = "error")
        })
        return()
      }

      if (data_type() == "survival") {
        # 生存数据：若用户选了 MH 法，弹出确认切换
        if (method_original == "mh") {
          pending_synth(list(selected_levels = selected_levels, new_name = new_name))
          showModal(
            modalDialog(
              title = "方法切换确认",
              "生存数据合成仅支持逆方差加权法（基于 HR 及其标准误）。是否切换为逆方差加权法继续合成？",
              footer = tagList(
                modalButton("取消"),
                actionButton(ns("confirm_switch"), "确认切换", class = "btn-primary")
              ),
              easyClose = FALSE
            )
          )
          return()
        }

        # 直接执行逆方差加权
        tryCatch({
          perform_synthesis(selected_levels, new_name, "invvar")
          removeModal()
        }, error = function(e) {
          showNotification(paste("合成失败：", e$message), type = "error")
        })
        return()
      }

      # 未知类型
      showNotification("不支持的数据类型", type = "error")
    })

    # 用户确认切换方法后的处理
    observeEvent(input$confirm_switch, {
      req(pending_synth())
      params <- pending_synth()
      removeModal()  # 关闭切换确认框
      tryCatch({
        perform_synthesis(params$selected_levels, params$new_name, "invvar")
        removeModal()  # 关闭合成主对话框
      }, error = function(e) {
        showNotification(paste("合成失败：", e$message), type = "error")
      })
      pending_synth(NULL)
    })

    # 返回模块的输出（如果需要）
    return(list(new_level = reactive({ input$synth_name })))
  })
}
