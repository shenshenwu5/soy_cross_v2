# =============================================================================
# Shiny 应用：杂交组合配置
# =============================================================================

library(shiny)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)

# 1. 项目根目录检测
tryCatch({
  script_path <- normalizePath(sys.frame(1)$ofile, mustWork = FALSE)
  if (file.exists(script_path)) {
    app_dir <- dirname(script_path)
    project_root <- dirname(app_dir)
  } else {
    project_root <- getwd()
  }
}, error = function(e) {
  project_root <<- getwd()
})

# 如果当前工作目录是 apps 或 scripts，向上修正
if (basename(project_root) %in% c("apps", "scripts")) {
  project_root <- dirname(project_root)
}

# 2. 加载依赖模块
mod_cross_path <- file.path(project_root, "R", "mod_cross.R")
if (!file.exists(mod_cross_path)) stop("❌ 找不到文件：", mod_cross_path)
source(mod_cross_path)

# 3. UI 定义
ui <- fluidPage(
  titlePanel("杂交组合配置工具"),
  cross_config_ui("cross_config_1")
)

# 4. Server 定义
server <- function(input, output, session) {
  db_path <- file.path(project_root, "data", "db", "soy_cross.db")
  
  # 确保数据库目录存在
  if (!dir.exists(dirname(db_path))) dir.create(dirname(db_path), recursive = TRUE)
  
  cross_config_server("cross_config_1", db_path = db_path)
}

# 5. 运行应用
shinyApp(ui, server)
