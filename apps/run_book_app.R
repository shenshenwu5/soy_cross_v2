# =============================================================================
# Shiny App: 帐本生成 (Book Generation)
# Based on docs/生成帐本app框架.md
# =============================================================================

library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)

# 尝试加载 soyplant 包，如果不存在则忽略错误（后续检查函数是否存在）
tryCatch({
  library(soyplant)
}, error = function(e) {
  message("Please install soyplant： devtools::install_github('zhaoqingsonga/soyplant')")
})

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

# 如果当前 wd 是 apps 或 scripts，向上修正
if (basename(project_root) %in% c("apps", "scripts")) project_root <- dirname(project_root)

# 加载业务逻辑脚本
# 1. 加载 mod_cross.R (包含 get_crosses_by_batch, has_cross 等数据库操作函数)
mod_cross_path <- file.path(project_root, "R", "mod_cross.R")
if (file.exists(mod_cross_path)) {
  source(mod_cross_path)
} else {
  warning("警告: 未找到 R/mod_cross.R，部分数据库功能可能不可用")
}

# 2. 检查核心函数依赖 (get_combination, planting, savewb 通常来自 soyplant 包)
required_funcs <- c("get_combination", "planting", "savewb", "update_cross_names_from_df", "get_crosses_by_batch")
missing_funcs <- required_funcs[!sapply(required_funcs, exists)]
if (length(missing_funcs) > 0) {
  warning(paste("警告: 缺少核心函数:", paste(missing_funcs, collapse = ", ")))
}

# -----------------------------------------------------------------------------
# UI Definition
# -----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("杂交帐本生成系统"),
  
  # 添加自定义 CSS 样式
  tags$head(
    tags$style(HTML("
      /* 表格行高固定，字体调整 */
      .dataTable tbody tr {
        height: 20px !important;
      }
      .dataTable {
        font-size: 1.0em !important;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("参数配置"),
      hr(),
      
      # 1. 批次选择
      selectInput("gen_batch", "批次选择 (Batch)", choices = NULL),
      
      # 2. 编号参数
      textInput("gen_prefix", "组合前缀 (Prefix)", value = format(Sys.Date(), "G%y")),
      numericInput("gen_start_n", "起始编号 (Start N)", value = 1, min = 1),
      numericInput("gen_digits", "编号位数 (Digits)", value = 3, min = 1),
      
      hr(),
      h4("种植参数"),
      
      # 3. 种植参数
      textInput("gen_place", "种植地点 (Place)", value = "海南"),
      numericInput("gen_rows", "种植行数 (Rows)", value = 2, min = 1),
      numericInput("gen_rp", "重复数 (Replicates)", value = 1, min = 1),
      numericInput("gen_interval", "间隔 (Interval)", value = 999, min = 1),
      
      hr(),
      helpText("说明：配置好参数后，先点击'生成预览'检查数据，确认无误后再回写数据库或导出文件。")
    ),
    
    mainPanel(
      width = 9,
      
      # 操作按钮区
      fluidRow(
        column(12,
               actionButton("btn_calc_preview", "1. 生成预览", class = "btn-primary", icon = icon("play")),
               span(style = "margin: 0 10px;", "|"),
               actionButton("btn_save_db_name", "2. 填写杂交名称", class = "btn-danger", icon = icon("database")),
               span(style = "margin: 0 10px;", "|"),
               downloadButton("btn_export_xlsx", "3. 导出 Excel 帐本", class = "btn-success")
        )
      ),
      hr(),
      
      # 结果展示区
      tabsetPanel(
        tabPanel("组合预览 (Combination)", 
                 br(),
                 DT::dataTableOutput("tbl_preview_combi")
        ),
        tabPanel("种植预览 (Planting)", 
                 br(),
                 DT::dataTableOutput("tbl_preview_plant")
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server Logic
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 数据库路径
  db_path <- file.path(project_root, "data", "db", "soy_cross.db")
  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)
  
  # 存储计算结果
  store_gen <- reactiveValues(
    my_combi = NULL, 
    planted = NULL
  )
  
  # ---------------------------------------------------------------------------
  # 1. 初始化与批次加载
  # ---------------------------------------------------------------------------
  load_batches <- function() {
    tryCatch({
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con), add = TRUE)
      
      # 获取所有批次
      if (dbExistsTable(con, "crosses")) {
        # 按最后插入的时间排序 (使用 rowid)
        batches <- dbGetQuery(con, "SELECT batch FROM crosses GROUP BY batch ORDER BY MAX(rowid) DESC")
        return(batches$batch)
      } else {
        return(character(0))
      }
    }, error = function(e) {
      showNotification(paste("读取批次失败:", e$message), type = "error")
      return(character(0))
    })
  }
  
  observe({
    batches <- load_batches()
    if (length(batches) > 0) {
      updateSelectInput(session, "gen_batch", choices = batches, selected = batches[1])
    }
  })
  
  # ---------------------------------------------------------------------------
  # 2. 生成预览 (Calculation)
  # ---------------------------------------------------------------------------
  observeEvent(input$btn_calc_preview, {
    req(input$gen_batch, input$gen_prefix)
    
    withProgress(message = '正在计算...', value = 0, {
      
      tryCatch({
        # A. 获取原始数据
        incProgress(0.2, detail = "获取数据库记录")
        
        mycross <- NULL
        
        # 1. 获取杂交记录
        if (exists("get_crosses_by_batch")) {
          # 必须传递计算好的绝对路径 db_path，避免相对路径错误
          mycross <- get_crosses_by_batch(batch = input$gen_batch, db_path = db_path)
        } else {
          # Fallback: 手动查询
          con <- dbConnect(SQLite(), db_path)
          on.exit(dbDisconnect(con), add = TRUE)
          mycross <- dbGetQuery(con, "SELECT * FROM crosses WHERE batch = ?", params = list(input$gen_batch))
        }
        
        if (nrow(mycross) == 0) {
          showNotification("该批次无数据", type = "warning")
          return()
        }
        
        # B. 关联亲本信息
        incProgress(0.4, detail = "关联亲本信息")
        
        # 2. 关联亲本名称
        if (exists("join_cross_parents")) {
          mydata <- join_cross_parents(mycross, db_path = db_path)
        } else {
          # Fallback (虽然理论上 source mod_cross.R 后应该存在)
          con <- dbConnect(SQLite(), db_path)
          parents <- dbGetQuery(con, "SELECT id, name FROM parents")
          dbDisconnect(con)
          
          mydata <- merge(mycross, parents, by.x="female_id", by.y="id", suffixes=c("", "_f"))
          names(mydata)[names(mydata) == "name"] <- "female_name"
          mydata <- merge(mydata, parents, by.x="male_id", by.y="id", suffixes=c("", "_m"))
          names(mydata)[names(mydata) == "name"] <- "male_name"
          mydata$ma <- mydata$male_name
          mydata$pa <- mydata$female_name
        }
        
        # C. 生成组合编号
        incProgress(0.6, detail = "生成组合编号")
        
        if (exists("get_combination")) {
          # 严格按照 cross_book.R 的逻辑构建输入数据
          # 确保 ma/pa 是名称，且按降序排列
          combi_input <- data.frame(
            ma = mydata$male_name,
            pa = mydata$female_name,
            stringsAsFactors = FALSE
          )
          
          # 排序
          combi_input <- combi_input %>% 
            arrange(desc(ma), desc(pa))
            
          my_combi <- get_combination(
            combi_input,
            prefix = input$gen_prefix,
            startN = input$gen_start_n,
            only = TRUE,
            order = FALSE
          )
          store_gen$my_combi <- my_combi
        } else {
          stop("找不到 get_combination 函数")
        }
        
        # D. 生成种植排图
        incProgress(0.8, detail = "生成排图计划")
        
        if (exists("planting")) {
          # 确保有 year 字段
          if (!"year" %in% names(store_gen$my_combi)) {
            store_gen$my_combi$year <- format(Sys.Date(), "%Y")
          }
          
          planted <- planting(
            store_gen$my_combi,
            place = input$gen_place,
            rows = input$gen_rows,
            rp = input$gen_rp,
            interval = input$gen_interval,
            digits = input$gen_digits,
            s_prefix = input$gen_prefix,
            ck = NULL
          )
          store_gen$planted <- planted
        } else {
          stop("找不到 planting 函数")
        }
        
        incProgress(1.0, detail = "完成")
        showNotification("预览生成成功", type = "message")
        
      }, error = function(e) {
        showNotification(paste("生成失败:", e$message), type = "error")
      })
    })
  })
  
  # ---------------------------------------------------------------------------
  # 3. 渲染表格
  # ---------------------------------------------------------------------------
  output$tbl_preview_combi <- DT::renderDataTable({
    req(store_gen$my_combi)
    DT::datatable(store_gen$my_combi, 
                  options = list(
                    pageLength = 15, 
                    scrollX = TRUE, 
                    dom = 'frtip',
                    autoWidth = FALSE,
                    columnDefs = list(list(
                      targets = "_all",
                      render = JS(
                        "function(data, type, row, meta) {",
                        "  return type === 'display' && data != null && data.length > 10 ?",
                        "    '<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                        "}"
                      )
                    ))
                  ),
                  class = "compact stripe hover")
  })
  
  output$tbl_preview_plant <- DT::renderDataTable({
    req(store_gen$planted)
    DT::datatable(store_gen$planted, 
                  options = list(
                    pageLength = 15, 
                    scrollX = TRUE, 
                    dom = 'frtip',
                    autoWidth = FALSE,
                    columnDefs = list(list(
                      targets = "_all",
                      render = JS(
                        "function(data, type, row, meta) {",
                        "  return type === 'display' && data != null && data.length > 10 ?",
                        "    '<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                        "}"
                      )
                    ))
                  ),
                  class = "compact stripe hover")
  })
  
  # ---------------------------------------------------------------------------
  # 4. 回写数据库
  # ---------------------------------------------------------------------------
  observeEvent(input$btn_save_db_name, {
    req(store_gen$my_combi)
    
    showModal(modalDialog(
      title = "确认回写数据库",
      "确定要将生成的组合名称 (Name) 更新回数据库吗？",
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_save_db", "确认更新", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_save_db, {
    removeModal()
    
    tryCatch({
      if (exists("update_cross_names_from_df")) {
        # 提取必要字段
        cols_to_update <- intersect(c("name", "ma", "pa"), names(store_gen$my_combi))
        if (length(cols_to_update) == 0) stop("数据中缺少 name/ma/pa 字段")
        
        update_cross_names_from_df(
          data = store_gen$my_combi[, cols_to_update, drop=FALSE], 
          batch = input$gen_batch,
          db_path = db_path # 显式传递 db_path
        )
        showNotification("数据库更新成功", type = "message")
      } else {
        stop("找不到 update_cross_names_from_df 函数")
      }
    }, error = function(e) {
      showNotification(paste("更新失败:", e$message), type = "error")
    })
  })
  
  # ---------------------------------------------------------------------------
  # 5. 导出 Excel
  # ---------------------------------------------------------------------------
  output$btn_export_xlsx <- downloadHandler(
    filename = function() { 
      paste0("CrossBook_", input$gen_prefix, "_", input$gen_batch, ".xlsx") 
    },
    content = function(file) {
      req(store_gen$my_combi, store_gen$planted)
      
      tryCatch({
        if (exists("savewb")) {
          # 定义导出字段 (参考文档)
          fields <- c("fieldid", "code", "place", "stageid", "name", "rows", "line_number", "rp")
          myview_cols <- intersect(c(fields, "ma", "pa"), names(store_gen$planted))
          
          # 组合矩阵 (如果函数存在)
          mat <- NULL
          if (exists("combination_matrix")) {
            mat <- combination_matrix(store_gen$my_combi)
          }
          
          savewb(
            origin = store_gen$my_combi,
            planting = store_gen$planted,
            myview = store_gen$planted[, myview_cols, drop=FALSE],
            combi_matrix = mat,
            filename = file,
            overwrite = TRUE
          )
        } else {
          stop("找不到 savewb 函数")
        }
      }, error = function(e) {
        showNotification(paste("导出失败:", e$message), type = "error")
      })
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
