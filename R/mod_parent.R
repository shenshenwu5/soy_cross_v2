library(shiny)
library(DT)
library(DBI)
library(RSQLite)

parent_admin_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
        .sidebar-scroll { 
          max-height: calc(100vh - 160px); 
          overflow-y: auto; 
          padding-right: 6px;
        }
      "))
    ),
    titlePanel("亲本管理"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "sidebar-scroll",
          checkboxInput(ns("filter_active"), "仅显示活跃亲本", value = TRUE),
          textInput(ns("search_name"), "按名称搜索", ""),
          actionButton(ns("btn_refresh"), "刷新", class = "btn-primary"),
          hr(),
          actionButton(ns("btn_add"), "新增", class = "btn-success"),
          actionButton(ns("btn_edit"), "修改", class = "btn-warning"),
          actionButton(ns("btn_soft_del"), "停用", class = "btn-danger"),
          actionButton(ns("btn_enable"), "启用", class = "btn-success"),
          actionButton(ns("btn_hard_del"), "物理删除", class = "btn-danger")
        )
      ),
      mainPanel(
        DT::dataTableOutput(ns("tbl_parents"))
      )
    )
  )
}

parent_admin_server <- function(id, db_path = "data/db/soy_cross.db") {
  moduleServer(id, function(input, output, session) {
    db_path <- normalizePath(db_path, winslash = "/", mustWork = FALSE)
    pending_delete_id <- reactiveVal(NULL)
    ensure_log <- function() {
      d <- file.path(getwd(), "logs")
      if (!dir.exists(d)) dir.create(d, showWarnings = FALSE)
      file.path(d, "parent_edit_log.csv")
    }
    log_write <- function(op, details, status = "success") {
      f <- ensure_log()
      entry <- data.frame(
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        operation = op,
        details = details,
        status = status,
        user = Sys.info()[["user"]],
        stringsAsFactors = FALSE
      )
      utils::write.table(entry, f, sep = ",", row.names = FALSE, col.names = !file.exists(f), append = TRUE, fileEncoding = "UTF-8")
    }
    load_parents <- function(active_only = TRUE, name_like = NULL) {
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      sql <- "SELECT * FROM parents"
      params <- list()
      where <- c()
      if (active_only && "active" %in% dbListFields(con, "parents")) {
        where <- c(where, "active = 1")
      }
      if (!is.null(name_like) && nzchar(name_like) && "name" %in% dbListFields(con, "parents")) {
        where <- c(where, "name LIKE ?")
        params <- c(params, paste0("%", name_like, "%"))
      }
      if (length(where) > 0) {
        sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
      }
      if (length(params) > 0) {
        dbGetQuery(con, sql, params = unname(params))
      } else {
        dbGetQuery(con, sql)
      }
    }
    output$tbl_parents <- DT::renderDataTable({
      df <- load_parents(input$filter_active, input$search_name)
      DT::datatable(df, selection = "single", options = list(pageLength = 10, lengthMenu = c(10, 25, 50)))
    })
    observeEvent(input$btn_refresh, {
      output$tbl_parents <- DT::renderDataTable({
        df <- load_parents(input$filter_active, input$search_name)
        DT::datatable(df, selection = "single", options = list(pageLength = 10, lengthMenu = c(10, 25, 50)))
      })
      showNotification("已刷新", type = "message")
    })
    get_selected_row <- reactive({
      s <- input$tbl_parents_rows_selected
      if (is.null(s) || length(s) == 0) return(NULL)
      df <- load_parents(input$filter_active, input$search_name)
      df[s[1], , drop = FALSE]
    })
    observeEvent(input$btn_add, {
      showModal(modalDialog(
        title = "新增亲本",
        textInput(session$ns("add_name"), "名称", ""),
        checkboxInput(session$ns("add_active"), "启用", TRUE),
        footer = tagList(
          modalButton("取消"),
          actionButton(session$ns("confirm_add"), "保存", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_add, {
      removeModal()
      name <- input$add_name
      active <- if (isTRUE(input$add_active)) 1L else 0L
      if (!nzchar(name)) {
        showNotification("名称不能为空", type = "error")
        return(NULL)
      }
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      fields <- dbListFields(con, "parents")
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dup_name <- if ("name" %in% fields) dbGetQuery(con, "SELECT COUNT(*) AS n FROM parents WHERE name = ?", params = list(name))$n else 0
      if (dup_name > 0) {
        showNotification("名称已存在", type = "error")
        log_write("add", paste("name=", name), "error")
        return(NULL)
      }
      ids <- dbGetQuery(con, "SELECT id FROM parents")
      vec <- ids$id
      nums <- suppressWarnings(as.integer(gsub("^P(\\d+)$", "\\1", vec)))
      base <- if (is.finite(max(nums, na.rm = TRUE))) max(nums, na.rm = TRUE) else 0L
      id <- sprintf("P%04d", base + 1L)
      cols <- intersect(c("id","name","active","created_at","updated_at"), fields)
      vals <- list(id, name, active, now, now)[seq_along(cols)]
      sql <- paste0("INSERT INTO parents (", paste(cols, collapse = ", "), ") VALUES (", paste(rep("?", length(cols)), collapse = ", "), ")")
      dbExecute(con, sql, params = unname(vals))
      log_write("add", paste("id=", id, "name=", name))
      output$tbl_parents <- DT::renderDataTable({
        df <- load_parents(input$filter_active, input$search_name)
        DT::datatable(df, selection = "single", options = list(pageLength = 10, lengthMenu = c(10, 25, 50)))
      })
      showNotification("已新增", type = "message")
    })
    observeEvent(input$btn_edit, {
      row <- get_selected_row()
      if (is.null(row)) {
        showNotification("请先选择一行", type = "warning")
        return(NULL)
      }
      output$edit_fields <- renderUI({
        con <- dbConnect(RSQLite::SQLite(), db_path)
        on.exit(dbDisconnect(con), add = TRUE)
        schema <- dbGetQuery(con, "PRAGMA table_info(parents)")
        ns <- session$ns
        items <- lapply(seq_len(nrow(schema)), function(i) {
          col <- schema$name[i]
          val <- if (col %in% names(row)) row[[col]] else ""
          type <- tolower(schema$type[i])
          if (col == "active") {
            checkboxInput(ns(paste0("edit_", col)), "启用", isTRUE(as.integer(val) == 1))
          } else if (grepl("int|real|num", type)) {
            numericInput(ns(paste0("edit_", col)), col, suppressWarnings(as.numeric(val)))
          } else {
            textInput(ns(paste0("edit_", col)), col, as.character(val))
          }
        })
        do.call(tagList, items)
      })
      showModal(modalDialog(
        title = "修改亲本",
        uiOutput(session$ns("edit_fields")),
        footer = tagList(
          modalButton("取消"),
          actionButton(session$ns("confirm_edit"), "保存", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_edit, {
      removeModal()
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      fields <- dbListFields(con, "parents")
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      schema <- dbGetQuery(con, "PRAGMA table_info(parents)")
      cols <- schema$name
      orig_row <- get_selected_row()
      if (is.null(orig_row)) {
        showNotification("未选择行", type = "error")
        return(NULL)
      }
      vals <- lapply(cols, function(col) {
        input[[paste0("edit_", col)]]
      })
      names(vals) <- cols
      if ("active" %in% cols) {
        vals[["active"]] <- if (isTRUE(vals[["active"]])) 1L else 0L
      }
      if ("updated_at" %in% cols) {
        vals[["updated_at"]] <- now
      }
      if (!("id" %in% cols) || !nzchar(as.character(vals[["id"]]))) {
        showNotification("ID 不能为空", type = "error")
        return(NULL)
      }
      if (!("name" %in% cols) || !nzchar(as.character(vals[["name"]]))) {
        showNotification("名称不能为空", type = "error")
        return(NULL)
      }
      exist <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM parents WHERE id = ?", params = list(orig_row$id))$n
      if (exist == 0) {
        showNotification("ID 不存在", type = "error")
        log_write("edit", paste("id=", orig_row$id), "error")
        return(NULL)
      }
      if (as.character(vals[["id"]]) != as.character(orig_row$id)) {
        dup_id <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM parents WHERE id = ?", params = list(vals[["id"]]))$n
        if (dup_id > 0) {
          showNotification("ID 已存在", type = "error")
          return(NULL)
        }
      }
      if ("name" %in% cols && as.character(vals[["name"]]) != as.character(orig_row$name)) {
        dup_name <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM parents WHERE name = ?", params = list(vals[["name"]]))$n
        if (dup_name > 0) {
          showNotification("名称已存在", type = "error")
          return(NULL)
        }
      }
      upd_cols <- intersect(cols, fields)
      set_clause <- paste(paste0(upd_cols, " = ?"), collapse = ", ")
      sql <- paste0("UPDATE parents SET ", set_clause, " WHERE id = ?")
      params <- c(unname(as.list(vals[upd_cols])), orig_row$id)
      dbExecute(con, sql, params = unname(params))
      log_write("edit", paste("id=", orig_row$id))
      output$tbl_parents <- DT::renderDataTable({
        df <- load_parents(input$filter_active, input$search_name)
        DT::datatable(df, selection = "single", options = list(pageLength = 10, lengthMenu = c(10, 25, 50)))
      })
      showNotification("已修改", type = "message")
    })
    observeEvent(input$btn_enable, {
      row <- get_selected_row()
      if (is.null(row)) {
        showNotification("请先选择一行", type = "warning")
        return(NULL)
      }
      id <- row$id
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dbExecute(con, "UPDATE parents SET active = 1, updated_at = ? WHERE id = ?", params = unname(list(now, id)))
      log_write("enable", paste("id=", id))
      output$tbl_parents <- DT::renderDataTable({
        df <- load_parents(input$filter_active, input$search_name)
        DT::datatable(df, selection = "single", options = list(pageLength = 10, lengthMenu = c(10, 25, 50)))
      })
      showNotification("已启用", type = "message")
    })
    observeEvent(input$btn_soft_del, {
      row <- get_selected_row()
      if (is.null(row)) {
        showNotification("请先选择一行", type = "warning")
        return(NULL)
      }
      id <- row$id
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      dbExecute(con, "UPDATE parents SET active = 0, updated_at = ? WHERE id = ?", params = unname(list(now, id)))
      log_write("soft_delete", paste("id=", id))
      output$tbl_parents <- DT::renderDataTable({
        df <- load_parents(input$filter_active, input$search_name)
        DT::datatable(df, selection = "single", options = list(pageLength = 10, lengthMenu = c(10, 25, 50)))
      })
      showNotification("已停用", type = "message")
    })
    observeEvent(input$btn_hard_del, {
      row <- get_selected_row()
      if (is.null(row)) {
        showNotification("请先选择一行", type = "warning")
        return(NULL)
      }
      id <- row$id
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      ref <- dbGetQuery(con, "SELECT (SELECT COUNT(*) FROM crosses WHERE female_id = ?) + (SELECT COUNT(*) FROM crosses WHERE male_id = ?) AS n", params = list(id, id))$n
      if (ref > 0) {
        showNotification("存在引用，禁止物理删除", type = "error")
        log_write("hard_delete", paste("id=", id, "ref=", ref), "error")
        return(NULL)
      }
      pending_delete_id(id)
      showModal(modalDialog(
        title = "确认物理删除",
        div(paste0("即将删除 ID=", id, " 的亲本记录。该操作不可恢复。")),
        footer = tagList(
          modalButton("取消"),
          actionButton(session$ns("confirm_hard_del"), "确认删除", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$confirm_hard_del, {
      id <- pending_delete_id()
      removeModal()
      if (is.null(id)) {
        showNotification("无待删除记录", type = "error")
        return(NULL)
      }
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      dbExecute(con, "DELETE FROM parents WHERE id = ?", params = list(id))
      log_write("hard_delete", paste("id=", id))
      pending_delete_id(NULL)
      output$tbl_parents <- DT::renderDataTable({
        df <- load_parents(input$filter_active, input$search_name)
        DT::datatable(df, selection = "single", options = list(pageLength = 10, lengthMenu = c(10, 25, 50)))
      })
      showNotification("已删除", type = "message")
    })
  })
}
