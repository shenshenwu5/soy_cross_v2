# =============================================================================
# Shiny 应用：杂交组合配置（四步向导）
# =============================================================================

library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)
if (!requireNamespace("rhandsontable", quietly = TRUE)) {
  stop("缺少依赖包：rhandsontable，请先安装：install.packages('rhandsontable')")
} else {
  library(rhandsontable)
}

# 项目根目录检测
tryCatch({
  script_path <- normalizePath(sys.frame(1)$ofile, mustWork = FALSE)
  if (file.exists(script_path)) {
    app_dir <- dirname(script_path)
    project_root <- dirname(app_dir)
  } else {
    project_root <- getwd()
  }
}, error = function(e) { project_root <<- getwd() })
if (basename(project_root) %in% c("apps", "scripts")) project_root <- dirname(project_root)

# 加载模块
mod_cross_path <- file.path(project_root, "R", "mod_cross.R")
if (!file.exists(mod_cross_path)) stop("❌ 找不到文件：", mod_cross_path)
source(mod_cross_path)
mod_analysis_path <- file.path(project_root, "R", "mod_analysis.R")
if (file.exists(mod_analysis_path)) source(mod_analysis_path)

# UI
ui <- navbarPage("杂交组合配置", id = "steps",
  tabPanel("A 母本",
    fluidPage(
      DT::dataTableOutput("tbl_females"),
      actionButton("confirm_females", "母本确认", class = "btn-primary")
    )
  ),
  tabPanel("A 已选母本",
    fluidPage(
      DT::dataTableOutput("tbl_females_selected"),
      actionButton("remove_females", "移除选中", class = "btn-warning"),
      actionButton("goto_males", "进入父本选择", class = "btn-primary")
    )
  ),
  tabPanel("B 父本",
    fluidPage(
      checkboxInput("only_available", "只显示可配父本", FALSE),
      DT::dataTableOutput("tbl_males"),
      actionButton("confirm_males", "父本确认", class = "btn-primary")
    )
  ),
  tabPanel("B 已选父本",
    fluidPage(
      DT::dataTableOutput("tbl_males_selected"),
      actionButton("remove_males", "移除选中", class = "btn-warning"),
      actionButton("goto_matrix", "进入组合矩阵", class = "btn-success")
    )
  ),
  tabPanel("C 组合矩阵",
    fluidPage(
      tags$head(tags$style(HTML("
        .handsontable .existing { background-color:#e2e3e5 !important;} 
        .handsontable .diagonal { background-color:#f8d7da !important;}
        /* DataTables compacted rows */
        table.dataTable tbody td, table.dataTable tbody th {
            padding: 4px 8px !important;
            line-height: 1.2 !important;
        }
        /* DataTables filter inputs visibility */
        .dataTables_wrapper input {
            color: #333 !important;
            background-color: #fff !important;
            border: 1px solid #ccc !important;
            padding: 2px 5px !important;
        }
      "))),
      div(style = 'overflow-x: hidden;', rHandsontableOutput("matrix")),
      verbatimTextOutput("matrix_summary"),
      textInput("batch", "批次名", value = format(Sys.Date(), "%Y春季")),
      numericInput("limit", "生成数量 (可选)", value = NA, min = 1),
      actionButton("run_write", "写入数据库", class = "btn-danger"),
      verbatimTextOutput("run_summary"),
      DT::dataTableOutput("generated_table"),
      DT::dataTableOutput("skipped_table")
    )
  ),
  tabPanel("D 批次管理",
    fluidPage(
      fluidRow(
        column(4,
          h4("批次列表"),
          div(style="margin-bottom: 10px;",
            actionButton("refresh_batches", "刷新", icon = icon("refresh"), class = "btn-info btn-sm"),
            actionButton("delete_batch_btn", "删除选中批次", icon = icon("trash"), class = "btn-danger btn-sm")
          ),
          DT::dataTableOutput("batch_list_table")
        ),
        column(8,
          h4("批次详情"),
          DT::dataTableOutput("batch_detail_table")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  db_path <- file.path(project_root, "data", "db", "soy_cross.db")
  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)

  parents <- reactive({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)
    dbGetQuery(con, "SELECT * FROM parents WHERE active=1")
  })
  get_display_cols <- function(df) {
    names(df)
  }

  females_sel <- reactiveVal(character(0))
  males_sel <- reactiveVal(character(0))
  selected_pairs <- reactiveVal(data.frame(female_id=character(0), male_id=character(0)))

  # A 母本
  output$tbl_females <- DT::renderDataTable({
    df <- parents(); cols <- get_display_cols(df)
    DT::datatable(
      df[, cols, drop=FALSE], 
      selection = "multiple", 
      filter = "top", 
      class = "compact stripe hover",
      options = list(pageLength=20, scrollY = '60vh', scrollCollapse = TRUE, searchHighlight = TRUE)
    )
  })
  observeEvent(input$confirm_females, {
    df <- parents(); s <- input$tbl_females_rows_selected
    females_sel(df$id[s])
    updateTabsetPanel(session, "steps", selected = "A 已选母本")
  })
  output$tbl_females_selected <- DT::renderDataTable({
    df <- parents(); ids <- females_sel();
    DT::datatable(df[df$id %in% ids, , drop=FALSE], selection = "multiple", class = "compact stripe hover", options = list(pageLength=10))
  })
  observeEvent(input$remove_females, {
    df <- parents(); ids <- females_sel(); s <- input$tbl_females_selected_rows_selected
    if (length(s) > 0) {
      df_sel <- df[df$id %in% ids, , drop=FALSE]
      ids_to_remove <- df_sel$id[s]
      females_sel(setdiff(ids, ids_to_remove))
    }
  })
  observeEvent(input$goto_males, {
    updateTabsetPanel(session, "steps", selected = "B 父本")
  })

  # B 父本 + 冲突检测 + 禁止自交
  males_data <- reactive({
    df_all <- parents()
    f_ids <- females_sel()  # 已选母本的ID
    
    # 先获取已选母本的名称（于过滤前）
    moms <- df_all$name[df_all$id %in% f_ids]
    if (length(moms)==0 || !exists("find_unused_partners")) { 
      df_all$is_conflict <- FALSE
      # 这里也要排除自交（母本不能是父本）
      df_all <- df_all[!(df_all$id %in% f_ids), , drop = FALSE]
      return(df_all)
    }
    
    unused_list <- lapply(moms, function(m) tryCatch(find_unused_partners(m, role="female", db_path=db_path)$id, error=function(e) df_all$id))
    common_unused <- Reduce(intersect, unused_list)
    df_all$is_conflict <- !(df_all$id %in% common_unused)
    
    # 禁止自交：排除已选母本中的ID
    df_all <- df_all[!(df_all$id %in% f_ids), , drop = FALSE]
    
    if (isTRUE(input$only_available)) df_all <- df_all[!df_all$is_conflict, , drop=FALSE]
    df_all
  })
  output$tbl_males <- DT::renderDataTable({
    df <- males_data(); cols <- get_display_cols(df)
    opts <- list(pageLength = 20, scrollY = '60vh', scrollCollapse = TRUE, searchHighlight = TRUE)
    if ("is_conflict" %in% cols) {
      conf_idx <- which(cols == "is_conflict") - 1
      opts$columnDefs <- list(list(visible = FALSE, targets = conf_idx))
    }
    dt <- DT::datatable(df[, cols, drop=FALSE], selection="multiple", filter="top", class = "compact stripe hover", options=opts)
    if ("is_conflict" %in% names(df)) {
      dt <- dt %>% DT::formatStyle('id', valueColumns='is_conflict', target='row', backgroundColor=DT::styleEqual(c(TRUE,FALSE), c('#ffeeba','white')))
    }
    dt
  })
  observeEvent(input$confirm_males, {
    df <- males_data(); s <- input$tbl_males_rows_selected
    males_sel(df$id[s])
    updateTabsetPanel(session, "steps", selected = "B 已选父本")
  })
  output$tbl_males_selected <- DT::renderDataTable({
    df <- parents(); ids <- males_sel();
    DT::datatable(df[df$id %in% ids, , drop=FALSE], selection = "multiple", class = "compact stripe hover", options = list(pageLength=10))
  })
  observeEvent(input$remove_males, {
    df <- parents(); ids <- males_sel(); s <- input$tbl_males_selected_rows_selected
    if (length(s) > 0) {
      df_sel <- df[df$id %in% ids, , drop=FALSE]
      ids_to_remove <- df_sel$id[s]
      males_sel(setdiff(ids, ids_to_remove))
    }
  })
  observeEvent(input$goto_matrix, {
    updateTabsetPanel(session, "steps", selected = "C 组合矩阵")
  })

  # C 矩阵
  # 矩阵：行/列显示亲本名称；已存在（灰），可配置（绿），'-' 忽略（白），自交禁用（粉）
  output$matrix <- renderRHandsontable({
    f_ids <- females_sel(); m_ids <- males_sel(); if (length(f_ids)==0 || length(m_ids)==0) return(NULL)
    dfp <- parents(); id2name <- setNames(dfp$name, dfp$id)
    f_names <- unname(id2name[f_ids]); m_names <- unname(id2name[m_ids])
    con <- dbConnect(SQLite(), db_path); on.exit(dbDisconnect(con), add = TRUE)
    in_f <- paste(sprintf("'%s'", f_ids), collapse = ",")
    in_m <- paste(sprintf("'%s'", m_ids), collapse = ",")
    sql <- paste0("SELECT female_id, male_id, name FROM crosses WHERE (female_id IN (", in_f, ") AND male_id IN (", in_m, ")) OR (female_id IN (", in_m, ") AND male_id IN (", in_f, "))")
    ex_df <- dbGetQuery(con, sql)
    
    # 生成选择矩阵（默认全选 "TRUE"，后续根据情况修改）
    mat_vals <- matrix("TRUE", nrow=length(f_ids), ncol=length(m_ids))
    df_mat <- as.data.frame(mat_vals, stringsAsFactors = FALSE, check.names = FALSE)
    rownames(df_mat) <- f_names
    colnames(df_mat) <- m_names

    # 计算已存在组合坐标，并填充名称
    cell_props <- list()
    if (nrow(ex_df)>0) {
      for (i in seq_len(nrow(ex_df))) {
        fi <- ex_df$female_id[i]; mi <- ex_df$male_id[i]; nm <- ex_df$name[i]
        
        # 查找坐标 (fi in f_ids, mi in m_ids)
        r_idx <- match(fi, f_ids)
        c_idx <- match(mi, m_ids)
        
        if (!is.na(r_idx) && !is.na(c_idx)) {
          df_mat[r_idx, c_idx] <- nm
          cell_props[[length(cell_props)+1]] <- list(row = r_idx-1, col = c_idx-1, type = 'text', readOnly = TRUE, className = 'existing')
        }
        
        # 查找反交坐标 (fi in m_ids, mi in f_ids) -> 这里是否要显示？
        # 用户需求是"这个组合被配置过了"，通常指正交或反交。
        # 如果当前矩阵位置是 (Mother A, Father B)，而数据库里有 (Mother B, Father A)，
        # 这算是"配置过了"吗？通常正反交是分开的。
        # 但之前的逻辑 existing_coords 是把两者都算的。
        # 之前的逻辑：
        # if (fi %in% f_ids && mi %in% m_ids) ...
        # else if (fi %in% m_ids && mi %in% f_ids) ...
        # 如果是反交存在，是否要在正交位置显示？
        # 如果当前位置是 A x B。
        # 数据库有 B x A (name: B-A).
        # A x B 位置是否要显示 "B-A"？或者只是 A x B 自己的状态？
        # 通常矩阵里的单元格 (Row=A, Col=B) 代表 A x B。
        # 如果 A x B 已存在，显示名称。
        # 如果 B x A 已存在，那是 (Row=B, Col=A) 的事。
        # 所以只需要匹配 (Row=Female, Col=Male) 与 (Database Female, Database Male)。
        # 之前的 existing_coords 逻辑似乎是想把正反交都标记出来。
        # 但矩阵是 非对称的 (Rows=Moms, Cols=Dads)。
        # 如果 Row i 是 A, Col j 是 B. Cell is A x B.
        # 只有当 DB 中有 female=A, male=B 时，才是这个 cell 的 match。
        # DB 中 female=B, male=A 是另一个 cell (如果 B 在 Moms 里, A 在 Dads 里)。
        # 所以只需要精确匹配。
        
        # 修正：只匹配 exact match
        # 但考虑到 names 可能有 mapping 问题，直接用 id match
      }
    }
    
    # 重新遍历以处理对角线（自交）
    for (i in seq_along(f_ids)) {
      for (j in seq_along(m_ids)) {
        if (f_ids[i] == m_ids[j]) {
          df_mat[i, j] <- "FALSE" # 自交默认不选
          cell_props[[length(cell_props)+1]] <- list(row = i-1, col = j-1, readOnly = TRUE, className = 'diagonal')
        }
      }
    }

    # 创建表
    ht <- rhandsontable::rhandsontable(df_mat, rowHeaders = f_names, cell = cell_props) %>%
      rhandsontable::hot_table(height = 500)

    # 列设置为 checkbox (处理 "TRUE"/"FALSE" 字符串)
    widths <- pmax(80, pmin(300, nchar(m_names)*12))
    for (cn in colnames(df_mat)) {
      ht <- rhandsontable::hot_col(ht, cn, type = 'checkbox', checkedTemplate = "TRUE", uncheckedTemplate = "FALSE")
    }
    ht <- rhandsontable::hot_cols(ht, manualColumnResize = TRUE, colWidths = widths)
    ht <- rhandsontable::hot_cols(ht, renderer = "function (instance, td, row, col, prop, value, cellProperties) {
      if (cellProperties.type === 'checkbox') {
        Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
      } else {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
      }
      
      if (cellProperties.readOnly) {
         if (cellProperties.className && cellProperties.className.indexOf('diagonal') > -1) {
             td.style.background = '#f8d7da'; // Pink for diagonal
         } else {
             td.style.background = '#e2e3e5'; // Gray for existing
         }
      } else {
        td.style.background = 'white';
      }
    }")

    # 移除之前的 hot_cell 循环，因为已经集成到 cell_props
    
    ht
  })
  output$matrix_summary <- renderText({
    x <- input$matrix; if (is.null(x)) return("")
    m <- as.matrix(hot_to_r(x));
    # 重新计算已存在组合与自交数量
    f_ids <- females_sel(); m_ids <- males_sel();
    dfp <- parents(); id2name <- setNames(dfp$name, dfp$id)
    f_names <- unname(id2name[f_ids]); m_names <- unname(id2name[m_ids])
    con <- dbConnect(SQLite(), db_path); on.exit(dbDisconnect(con), add = TRUE)
    in_f <- paste(sprintf("'%s'", f_ids), collapse = ",")
    in_m <- paste(sprintf("'%s'", m_ids), collapse = ",")
    sql <- paste0("SELECT female_id, male_id FROM crosses WHERE (female_id IN (", in_f, ") AND male_id IN (", in_m, ")) OR (female_id IN (", in_m, ") AND male_id IN (", in_f, "))")
    ex_df <- dbGetQuery(con, sql)
    n_exist <- 0L
    if (nrow(ex_df)>0) {
      for (i in seq_len(nrow(ex_df))) {
        fi <- ex_df$female_id[i]; mi <- ex_df$male_id[i]
        if ((fi %in% f_ids && mi %in% m_ids) || (fi %in% m_ids && mi %in% f_ids)) {
          n_exist <- n_exist + 1L
        }
      }
    }
    n_diag <- length(intersect(f_ids, m_ids))
    total_cells <- length(f_ids) * length(m_ids)
    n_config <- total_cells - n_exist - n_diag
    n_ignore <- 0L
    glue("可配置: {n_config}，已存在: {n_exist}，忽略: {n_ignore}")
  })
  observeEvent(input$confirm_matrix, {
    # 已移除确认按钮，保留空逻辑占位避免错误引用
  })

  # D 执行
  observeEvent(input$run_write, {
    req(input$batch)
    x <- input$matrix; if (is.null(x)) { showNotification("矩阵为空", type="warning"); return(NULL) }
    m <- as.matrix(hot_to_r(x)); f_names <- rownames(m); m_names <- colnames(m)
    dfp <- parents(); name2id <- setNames(dfp$id, dfp$name)
    
    # 查找选中的组合（值为 "TRUE" 的单元格）
    pairs_idx <- which(m == "TRUE", arr.ind=TRUE)
    if (nrow(pairs_idx)==0) { showNotification("无可配置组合", type="warning"); return(NULL) }
    pairs <- data.frame(
      female_id = unname(name2id[f_names[pairs_idx[,1]]]),
      male_id   = unname(name2id[m_names[pairs_idx[,2]]]),
      stringsAsFactors = FALSE
    ) %>% dplyr::filter(female_id != male_id)
    
    # 最终检查：确保没有自交记录（应该为空）
    if (any(pairs$female_id == pairs$male_id)) {
      showNotification("错误：检测到自交组合，无法写入！", type="error")
      return(NULL)
    }
    
    # 调用 R/mod_cross.R 中的封装函数
    tryCatch({
      res <- create_specific_cross_plan(
        batch_name = input$batch,
        pairs = pairs,
        db_path = db_path,
        include_reciprocal = TRUE, # 默认生成反交
        limit = input$limit
      )
      
      output$run_summary <- renderText(glue(
        "写入成功：正交 {res$summary$inserted_n}，反交 {res$summary$reciprocal_added}，总计 {res$summary$total_inserted}；",
        "跳过：正交 {res$summary$skipped_direct}，反交 {res$summary$skipped_recip}"
      ))
      
      output$generated_table <- DT::renderDataTable({ 
        DT::datatable(res$new_crosses, class = "compact stripe hover", options=list(pageLength=20)) 
      })
      
      output$skipped_table <- DT::renderDataTable({ 
        DT::datatable(res$skipped, class = "compact stripe hover", options=list(pageLength=20)) 
      })
      
      if (res$db_updated) {
        showNotification("写入成功", type="message")
      } else {
        showNotification("未写入任何数据（可能全部已存在）", type="warning")
      }
      
    }, error = function(e) {
      output$run_summary <- renderText(paste("错误：", e$message))
      showNotification(paste("写入失败：", e$message), type="error")
    })
  })

  # D 批次管理
  # ------------------------------------------------------------------
  
  # 批次列表数据
  batches_df <- reactiveVal()
  
  load_batches <- function() {
    tryCatch({
      # 检查是否有 summarize_cross_batches_db 函数，如果没有则手动查询
      if (exists("summarize_cross_batches_db")) {
        df <- summarize_cross_batches_db(db_path = db_path)
      } else {
        con <- dbConnect(SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        df <- dbGetQuery(con, "
          SELECT 
            batch,
            COUNT(*) as total_crosses,
            SUM(CASE WHEN is_reciprocal = 0 THEN 1 ELSE 0 END) as direct_crosses,
            SUM(CASE WHEN is_reciprocal = 1 THEN 1 ELSE 0 END) as reciprocal_crosses,
            MAX(updated_at) as last_updated
          FROM crosses
          GROUP BY batch ORDER BY last_updated DESC
        ")
      }
      batches_df(df)
    }, error = function(e) {
      showNotification(paste("加载批次失败:", e$message), type = "error")
    })
  }
  
  # 初始化加载
  observe({
    load_batches()
  })
  
  observeEvent(input$refresh_batches, {
    load_batches()
  })
  
  # 渲染批次列表
  output$batch_list_table <- DT::renderDataTable({
    df <- batches_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    # 简单的列重命名
    DT::datatable(df, selection = "single", class = "compact stripe hover", 
                  colnames = c("批次", "总数", "正交", "反交", "状态数", "状态", "更新时间")[1:ncol(df)],
                  options = list(pageLength = 15, dom = 'ftp'))
  })
  
  # 渲染批次详情
  output$batch_detail_table <- DT::renderDataTable({
    s <- input$batch_list_table_rows_selected
    if (length(s) == 0) return(NULL)
    
    df_b <- batches_df()
    batch_name <- df_b$batch[s]
    
    if (exists("get_crosses_by_batch")) {
      details <- get_crosses_by_batch(batch = batch_name, db_path = db_path, include_reciprocal = TRUE)
    } else {
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      details <- dbGetQuery(con, "SELECT * FROM crosses WHERE batch = ?", params = list(batch_name))
    }
    DT::datatable(details, class = "compact stripe hover", options = list(pageLength = 15))
  })
  
  # 删除按钮
  observeEvent(input$delete_batch_btn, {
    s <- input$batch_list_table_rows_selected
    if (length(s) == 0) {
      showNotification("请先选择一个批次", type = "warning")
      return()
    }
    
    df_b <- batches_df()
    batch_name <- df_b$batch[s]
    
    showModal(modalDialog(
      title = "确认删除",
      paste0("确定要删除批次 '", batch_name, "' 吗？此操作将删除该批次下所有记录，且不可恢复！"),
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_delete_batch", "确认删除", class = "btn-danger")
      )
    ))
  })
  
  # 确认删除
  observeEvent(input$confirm_delete_batch, {
    removeModal()
    s <- input$batch_list_table_rows_selected
    if (length(s) == 0) return()
    
    df_b <- batches_df()
    batch_name <- df_b$batch[s]
    
    tryCatch({
      con <- dbConnect(SQLite(), db_path)
      dbExecute(con, "DELETE FROM crosses WHERE batch = ?", params = list(batch_name))
      dbDisconnect(con)
      
      showNotification(paste("批次", batch_name, "已删除"), type = "message")
      load_batches() # 刷新列表
    }, error = function(e) {
      showNotification(paste("删除失败:", e$message), type = "error")
    })
  })
}


shinyApp(ui, server)
