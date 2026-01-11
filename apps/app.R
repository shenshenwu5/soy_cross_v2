# =============================================================================
# 大豆杂交管理系统 (Soybean Cross Management System)
# 整合版主程序
# =============================================================================

library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)
library(rhandsontable)

# 尝试加载 soyplant 包
tryCatch({
  library(soyplant)
}, error = function(e) {
  message("Please install soyplant： devtools::install_github('zhaoqingsonga/soyplant')")
})

# === 加载配置 ===
tryCatch({
  config_path <- file.path(project_root, "config", "config.R")
  if (file.exists(config_path)) {
    source(config_path)
  }
}, error = function(e) {
  message("配置文件加载失败：", e$message)
})

# === 环境配置 ===

# 确定项目根目录
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

# 数据库路径
db_path <- file.path(project_root, "data", "db", "soy_cross.db")

# === 加载模块 ===

# 辅助函数模块
source(file.path(project_root, "R", "mod_cross.R"))
if (file.exists(file.path(project_root, "R", "mod_analysis.R"))) {
  source(file.path(project_root, "R", "mod_analysis.R"))
}

# 业务功能模块
source(file.path(project_root, "R", "mod_parent.R"))      # 亲本管理
source(file.path(project_root, "R", "mod_cross_app.R"))   # 杂交配置
source(file.path(project_root, "R", "mod_book_app.R"))    # 帐本生成
source(file.path(project_root, "R", "mod_matrix.R"))      # 矩阵视图
source(file.path(project_root, "R", "mod_analysis_app.R")) # 统计分析

# === UI 定义 ===

ui <- navbarPage(
  title = "大豆杂交管理系统",
  theme = NULL, # 可以加载 shinythemes
  id = "main_nav",
  
  # === 全局样式 ===
  header = tags$head(
    tags$style(HTML("
      /* === Global Table Styling === */
      .dataTable tbody tr { 
        height: 20px !important; 
      }
      .dataTable { 
        font-size: 1.0em !important; 
      }
      .dataTable tbody td, .dataTable tbody th {
        padding: 2px 4px !important;
        vertical-align: middle !important;
        line-height: 1.2 !important;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        max-width: 200px; /* 默认最大宽度，配合 ellipsis */
      }
      .dataTable thead th {
        padding: 6px 4px !important;
        background-color: #f8f9fa;
        color: #495057;
        font-weight: 600;
        white-space: nowrap;
      }
      .dataTable tbody tr:hover {
        background-color: #f1f3f5 !important;
      }
      .dataTable tbody tr.selected {
        background-color: #007bff !important;
        color: white !important;
      }
      /* Handsontable Overrides */
      .handsontable .existing { background-color:#e2e3e5 !important;} 
      .handsontable .diagonal { background-color:#f8d7da !important;}
    "))
  ),
  
  # 1. 亲本管理
  tabPanel("亲本管理",
    icon = icon("users"),
    parent_admin_ui("parent_mod")
  ),
  
  # 2. 杂交配置
  tabPanel("杂交配置",
    icon = icon("random"),
    cross_app_ui("cross_mod")
  ),
  
  # 3. 帐本生成
  tabPanel("帐本生成",
    icon = icon("book"),
    book_app_ui("book_mod")
  ),
  
  # 4. 矩阵视图
  tabPanel("矩阵视图",
    icon = icon("th"),
    matrix_view_ui("matrix_mod")
  ),
  
  # 5. 统计分析
  tabPanel("统计分析",
    icon = icon("chart-bar"),
    analysis_ui("analysis_mod")
  ),
  
  # 关于/帮助
  tabPanel("关于",
    icon = icon("info-circle"),
    fluidPage(
      h3("大豆杂交管理系统 v2.0"),
      p("集成了亲本管理、杂交组合设计、帐本生成及排图、数据可视化等功能。"),
      hr(),
      p("项目路径: ", project_root),
      p("数据库路径: ", db_path),
      p("最后更新: ", format(Sys.Date(), "%Y-%m-%d"))
    )
  )
)

# === Server 定义 ===

server <- function(input, output, session) {
  
  # 调用子模块
  # 注意：db_path 必须正确传递
  
  # 1. 亲本管理
  parent_admin_server("parent_mod", db_path = db_path)
  
  # 2. 杂交配置
  cross_app_server("cross_mod", db_path = db_path)
  
  # 3. 帐本生成
  book_app_server("book_mod", db_path = db_path)
  
  # 4. 矩阵视图
  matrix_view_server("matrix_mod", db_path = db_path)
  
  # 5. 统计分析
  analysis_server("analysis_mod", db_path = db_path)
  
}

# 启动应用
shinyApp(ui = ui, server = server)
