# 杂交组合配置 Shiny 模块开发框架

本文档描述了基于 Shiny 的杂交组合配置（Cross Configuration）模块的设计框架。该模块旨在通过四个标准化步骤，帮助用户快速、准确地生成杂交计划。

## 1. 功能概述

模块核心功能是将杂交配置过程分解为四个引导式步骤：
1.  **选择母本** (Select Female Parents)
2.  **选择父本** (Select Male Parents)
3.  **设定批次** (Set Batch)
4.  **确定组合数** (Determine Count & Preview)

最终调用后台 `mod_cross.R` 中的逻辑将计划写入数据库。

## 2. UI 设计 (User Interface)

采用 **分步向导 (Wizard)** 或 **多列布局 (Column Layout)** 形式，确保逻辑清晰。

### 2.1 布局结构
建议使用 `navlistPanel` 或 `tabsetPanel` (type = "pills") 来模拟步骤流，或者使用 `fluidRow` + `column` 的左右分栏布局。

*   **左侧/顶部**：控制面板（步骤导航）。
*   **主区域**：当前步骤的操作界面。
*   **底部**：全局状态栏（已选母本数、已选父本数、预计组合数）。

### 2.2 详细步骤设计

#### 步骤 1：选择母本 (Female Parent Selection)
*   **组件**：`DT::dataTableOutput` (带 checkbox)。
*   **功能**：
    *   显示 `parents` 表中 `active=1` 的记录。
    *   提供搜索框（按名称/特征搜索）。
    *   支持多选。
*   **输出**：`selected_females` (Reactive Vector of IDs)。

#### 步骤 2：选择父本 (Male Parent Selection)
*   **组件**：`DT::dataTableOutput` (复用母本选择器的逻辑，但独立实例)。
*   **功能**：
    *   同上，显示活跃亲本。
    *   可增加“排除已选母本”的过滤选项（避免自交）。
*   **输出**：`selected_males` (Reactive Vector of IDs)。

#### 步骤 3：设定批次 (Batch Assignment)
*   **组件**：
    *   `textInput` ("输入新批次名") 或 `selectInput` ("选择已有批次")。
    *   `checkboxInput` ("自动添加反交", value = TRUE)。
*   **功能**：
    *   验证批次名是否为空。
    *   提示该批次下已有的组合数。

#### 步骤 4：确定组合数 (Determine Count & Preview)
*   **组件**：
    *   `verbatimTextOutput` (显示摘要：M × N = Total)。
    *   `numericInput` ("限制生成数量", value = NULL, placeholder = "默认生成所有组合")。
        *   *注：对应 `create_cross_plan_n` 功能，允许用户仅生成前 N 个或随机 N 个组合。*
    *   `actionButton` ("btn_generate", "生成计划", class = "btn-primary")。
*   **功能**：
    *   实时计算预计生成的组合数量。
    *   展示前 10 个预览组合。

## 3. Server 逻辑 (Server Logic)

### 3.1 响应式状态 (Reactive Values)
```r
values <- reactiveValues(
  females = character(0), # 存储母本 ID
  males = character(0),   # 存储父本 ID
  batch = "",             # 批次名
  preview_data = NULL     # 预览数据
)
```

### 3.2 核心流程
1.  **加载亲本**：调用 `mod_parent.R` 中的 `load_parents` 获取数据源。
2.  **监听选择**：
    *   `input$tbl_females_rows_selected` -> 更新 `values$females`。
    *   `input$tbl_males_rows_selected` -> 更新 `values$males`。
3.  **生成预览**：
    *   当母本、父本、批次变化时，调用 `expand.grid` 生成内存中的预览表。
    *   检查数据库中是否已存在（调用 `mod_cross::check_existing_crosses`），计算“新增”与“跳过”数量。
4.  **执行保存**：
    *   点击“生成”按钮后，判断是否设置了“限制数量”。
    *   若无限制：调用 `mod_cross::create_cross_plan`。
    *   若有限制：调用 `mod_cross::create_cross_plan_n`。
    *   弹出 `showNotification` 反馈结果。
    *   重置选择或保留（根据用户偏好）。

## 4. 代码框架示例

```r
library(shiny)
library(DT)
source("R/mod_cross.R")
source("R/mod_parent.R")

# UI
cross_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # 步骤 1 & 2：亲本选择（左右分栏）
      column(4, 
        h4("1. 选择母本"),
        DT::dataTableOutput(ns("tbl_females"))
      ),
      column(4, 
        h4("2. 选择父本"),
        DT::dataTableOutput(ns("tbl_males"))
      ),
      # 步骤 3 & 4：配置与执行
      column(4,
        h4("3. 配置参数"),
        textInput(ns("input_batch"), "批次名称", value = format(Sys.Date(), "%Y春季")),
        checkboxInput(ns("check_reciprocal"), "自动生成反交", TRUE),
        hr(),
        h4("4. 确认与生成"),
        numericInput(ns("input_limit"), "限制数量 (可选)", value = NA, min = 1),
        uiOutput(ns("ui_summary")),
        actionButton(ns("btn_run"), "🚀 生成杂交计划", class = "btn-primary btn-lg btn-block")
      )
    )
  )
}

# Server
cross_config_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. 数据加载
    parents <- reactive({
      # 假设 load_parents 返回 active=1 的亲本数据框
      con <- dbConnect(SQLite(), "data/db/soy_cross.db")
      on.exit(dbDisconnect(con))
      dbGetQuery(con, "SELECT id, name FROM parents WHERE active=1")
    })
    
    # 2. 渲染表格
    output$tbl_females <- DT::renderDataTable({
      DT::datatable(parents(), selection = "multiple", options = list(pageLength = 10))
    })
    
    output$tbl_males <- DT::renderDataTable({
      DT::datatable(parents(), selection = "multiple", options = list(pageLength = 10))
    })
    
    # 3. 实时摘要
    output$ui_summary <- renderUI({
      n_f <- length(input$tbl_females_rows_selected)
      n_m <- length(input$tbl_males_rows_selected)
      n_total <- n_f * n_m
      if (input$check_reciprocal) n_total <- n_total * 2
      
      tagList(
        p(glue::glue("已选母本: {n_f}")),
        p(glue::glue("已选父本: {n_m}")),
        p(glue::glue("预计组合: {n_total}"), style = "font-weight: bold; color: blue;")
      )
    })
    
    # 4. 执行逻辑
    observeEvent(input$btn_run, {
      req(input$input_batch)
      
      # 获取选中行的 ID
      p_data <- parents()
      f_ids <- p_data$id[input$tbl_females_rows_selected]
      m_ids <- p_data$id[input$tbl_males_rows_selected]
      
      if (length(f_ids) == 0 || length(m_ids) == 0) {
        showNotification("请至少选择一个母本和一个父本", type = "error")
        return()
      }
      
      withProgress(message = '正在生成计划...', {
        tryCatch({
          if (is.na(input$input_limit)) {
            # 全量生成
            res <- create_cross_plan(
              batch_name = input$input_batch,
              mothers = f_ids,
              fathers = m_ids,
              include_reciprocal = input$check_reciprocal,
              use_id = TRUE
            )
          } else {
            # 限量生成
            res <- create_cross_plan_n(
              batch_name = input$input_batch,
              mothers = f_ids,
              fathers = m_ids,
              n = input$input_limit,
              include_reciprocal = input$check_reciprocal,
              use_id = TRUE
            )
          }
          
          showNotification(glue::glue("成功！新增 {res$summary$new_crosses} 个组合"), type = "message")
          
        }, error = function(e) {
          showNotification(paste("错误:", e$message), type = "error")
        })
      })
    })
  })
}
```

## 5. 后续优化建议
*   **高级筛选**：在亲本选择表中增加“特征特性”列，方便按性状互补配组。
*   **冲突检测**：在选择父本时，高亮显示已经与当前母本配过组的亲本（调用 `mod_analysis::find_unused_partners`）。
*   **批量导入**：对于复杂的 4x4 或 NCII 设计，提供 CSV 导入功能替代手动点击。