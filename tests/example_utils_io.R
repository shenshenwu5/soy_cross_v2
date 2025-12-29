# =============================================================================
# utils_io.R 模块使用示例
# 文件路径: scripts/example_utils_io.R
# 功能说明: 演示 I/O 辅助模块的各项功能
# =============================================================================

# ---- 环境准备 ----
cat("========== utils_io.R 使用示例 ==========\n\n")

# 加载模块
source("R/utils_io.R")

# 检查并安装必要的包
if (!requireNamespace("DBI", quietly = TRUE)) {
  install.packages("DBI")
}
if (!requireNamespace("RSQLite", quietly = TRUE)) {
  install.packages("RSQLite")
}
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

library(DBI)
library(RSQLite)


# =============================================================================
# 示例 1: 路径规范化 (normalize_path)
# =============================================================================
cat("\n【示例 1】路径规范化\n")
cat("--------------------------------------\n")

# 1.1 规范化相对路径
path1 <- normalize_path("data/db/soy_cross.db")
cat("相对路径 → 绝对路径:\n")
cat("  输入: data/db/soy_cross.db\n")
cat("  输出:", path1, "\n\n")

# 1.2 自动创建目录
path2 <- normalize_path("output/reports/2025/test.xlsx", create_dir = TRUE)
cat("自动创建父目录:\n")
cat("  路径:", path2, "\n")
cat("  目录已创建:", dir.exists(dirname(path2)), "\n\n")


# =============================================================================
# 示例 2: 数据库备份 (backup_db)
# =============================================================================
cat("\n【示例 2】数据库备份\n")
cat("--------------------------------------\n")

# 2.1 创建示例数据库
demo_db <- normalize_path("data/db/demo.db", create_dir = TRUE)

# 如果数据库不存在，先创建
if (!file.exists(demo_db)) {
  con <- dbConnect(SQLite(), demo_db)
  
  # 创建示例表
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS test (
      id INTEGER PRIMARY KEY,
      name TEXT,
      value REAL
    )
  ")
  
  # 插入示例数据
  dbExecute(con, "INSERT INTO test (name, value) VALUES ('测试1', 100.5)")
  dbExecute(con, "INSERT INTO test (name, value) VALUES ('测试2', 200.8)")
  
  dbDisconnect(con)
  cat("✅ 已创建示例数据库: ", demo_db, "\n\n")
}

# 2.2 执行备份
cat("执行数据库备份...\n")
backup_file <- backup_db(demo_db)
cat("备份文件: ", backup_file, "\n\n")

# 2.3 备份到自定义目录
cat("备份到自定义目录...\n")
backup_file2 <- backup_db(demo_db, dest_dir = "backups/manual", backup_name = "重要备份")
cat("备份文件: ", backup_file2, "\n\n")


# =============================================================================
# 示例 3: 数据框写入和读取 (write_table / read_table)
# =============================================================================
cat("\n【示例 3】数据框读写\n")
cat("--------------------------------------\n")

# 3.1 准备示例数据
demo_data <- data.frame(
  品种名称 = c("中黄301", "冀豆17", "华豆17", "徐豆18", "冀豆12"),
  生育期 = c(120, 118, 115, 122, 119),
  产量 = c(3500, 3200, 3400, 3600, 3300),
  蛋白含量 = c(42.5, 41.8, 43.2, 40.9, 42.1),
  stringsAsFactors = FALSE
)

cat("示例数据:\n")
print(head(demo_data, 3))
cat("\n")

# 3.2 写入 RDS 文件
rds_path <- "output/demo_data.rds"
write_table(demo_data, rds_path)
cat("✅ 已保存为 RDS: ", rds_path, "\n\n")

# 3.3 写入 CSV 文件
csv_path <- "output/demo_data.csv"
write_table(demo_data, csv_path)
cat("✅ 已保存为 CSV: ", csv_path, "\n\n")

# 3.4 写入 Excel 文件（单工作表）
if (requireNamespace("openxlsx", quietly = TRUE)) {
  xlsx_path <- "output/demo_data.xlsx"
  write_table(demo_data, xlsx_path)
  cat("✅ 已保存为 Excel: ", xlsx_path, "\n\n")
}

# 3.5 写入 Excel 文件（多工作表）
if (requireNamespace("openxlsx", quietly = TRUE)) {
  multi_data <- list(
    "亲本信息" = demo_data,
    "高产品种" = demo_data[demo_data$产量 > 3300, ],
    "高蛋白品种" = demo_data[demo_data$蛋白含量 > 42, ]
  )
  
  multi_xlsx_path <- "output/multi_sheet.xlsx"
  write_table(multi_data, multi_xlsx_path)
  cat("✅ 已保存为多工作表 Excel: ", multi_xlsx_path, "\n")
  cat("   - 工作表1: 亲本信息\n")
  cat("   - 工作表2: 高产品种\n")
  cat("   - 工作表3: 高蛋白品种\n\n")
}

# 3.6 读取数据
cat("读取 RDS 文件...\n")
data_rds <- read_table(rds_path)
cat("   行数:", nrow(data_rds$data), "\n\n")

cat("读取 CSV 文件...\n")
data_csv <- read_table(csv_path)
cat("   行数:", nrow(data_csv), "\n\n")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  cat("读取 Excel 文件...\n")
  data_xlsx <- read_table(xlsx_path, sheet = 1)
  cat("   行数:", nrow(data_xlsx), "\n\n")
}


# =============================================================================
# 示例 4: 杂交矩阵导出 (export_cross_matrix)
# =============================================================================
cat("\n【示例 4】杂交矩阵导出\n")
cat("--------------------------------------\n")

# 4.1 准备示例数据库
matrix_db <- normalize_path("data/db/crosses_demo.db", create_dir = TRUE)

if (!file.exists(matrix_db)) {
  con <- dbConnect(SQLite(), matrix_db)
  
  # 创建父本表
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS parents (
      id INTEGER PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      active INTEGER DEFAULT 1
    )
  ")
  
  # 创建杂交表
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS crosses (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      female_id INTEGER NOT NULL,
      male_id INTEGER NOT NULL,
      batch TEXT,
      status TEXT DEFAULT 'planned',
      FOREIGN KEY (female_id) REFERENCES parents(id),
      FOREIGN KEY (male_id) REFERENCES parents(id)
    )
  ")
  
  # 插入示例亲本
  parents <- data.frame(
    id = 1:6,
    name = c("中黄301", "冀豆17", "华豆17", "徐豆18", "冀豆12", "中豆13"),
    active = 1
  )
  dbWriteTable(con, "parents", parents, append = TRUE)
  
  # 插入示例杂交记录
  crosses <- data.frame(
    female_id = c(1, 1, 2, 2, 3, 3),
    male_id = c(4, 5, 4, 6, 5, 6),
    batch = c("2025春季", "2025春季", "2025春季", "2025秋季", "2025秋季", "2025秋季"),
    status = "planned"
  )
  dbWriteTable(con, "crosses", crosses, append = TRUE)
  
  dbDisconnect(con)
  cat("✅ 已创建示例杂交数据库\n\n")
}

# 4.2 导出所有批次的矩阵
cat("导出所有批次的杂交矩阵...\n")
matrix_all <- export_cross_matrix(
  matrix_db, 
  out_path = "output/matrix_all.xlsx"
)
cat("矩阵维度: ", nrow(matrix_all), "×", ncol(matrix_all), "\n\n")

# 4.3 导出指定批次的矩阵
cat("导出 2025春季 批次的矩阵...\n")
matrix_spring <- export_cross_matrix(
  matrix_db, 
  batch = "2025春季",
  out_path = "output/matrix_2025春季.xlsx"
)
cat("矩阵维度: ", nrow(matrix_spring), "×", ncol(matrix_spring), "\n\n")


# =============================================================================
# 示例 5: 历史矩阵导入 (import_cross_matrix)
# =============================================================================
cat("\n【示例 5】历史矩阵导入\n")
cat("--------------------------------------\n")

# 5.1 从导出的矩阵文件导入
cat("从 Excel 导入历史矩阵...\n")
if (file.exists("output/matrix_2025春季.xlsx") && 
    requireNamespace("openxlsx", quietly = TRUE)) {
  
  imported_data <- import_cross_matrix(
    "output/matrix_2025春季.xlsx",
    batch = "2025春季_重新导入"
  )
  
  cat("导入结果:\n")
  print(head(imported_data, 5))
  cat("\n总计导入组合数:", nrow(imported_data), "\n\n")
}


# =============================================================================
# 示例 6: 完整工作流 - 批量操作前的安全措施
# =============================================================================
cat("\n【示例 6】完整工作流示例\n")
cat("--------------------------------------\n")

cat("场景: 批量导入历史杂交数据到生产数据库\n\n")

# 6.1 准备工作
prod_db <- normalize_path("data/db/soy_cross.db")

# 6.2 操作前备份
cat("步骤 1: 备份生产数据库...\n")
if (file.exists(prod_db)) {
  backup_file <- backup_db(prod_db, backup_name = "导入前备份")
  cat("✅ 备份完成\n\n")
} else {
  cat("⚠️  生产数据库不存在，跳过备份\n\n")
}

# 6.3 导入历史数据（示例）
cat("步骤 2: 导入历史杂交矩阵数据...\n")
if (file.exists("output/matrix_2025春季.xlsx") && 
    requireNamespace("openxlsx", quietly = TRUE)) {
  
  # 导入矩阵数据
  imported <- import_cross_matrix(
    "output/matrix_2025春季.xlsx",
    batch = "历史批次2025春"
  )
  
  cat("✅ 导入完成，共", nrow(imported), "条记录\n\n")
  
  # 6.4 数据验证
  cat("步骤 3: 数据验证...\n")
  cat("   - 母本数量:", length(unique(imported$female_id)), "\n")
  cat("   - 父本数量:", length(unique(imported$male_id)), "\n")
  cat("   - 批次名称:", unique(imported$batch), "\n\n")
  
  # 6.5 写入数据库（这里仅演示，实际需要更多验证）
  cat("步骤 4: 数据可以写入数据库（此处仅演示，未实际执行）\n")
  cat("   提示: 实际操作时应使用事务处理确保数据一致性\n\n")
}


# =============================================================================
# 示例 7: 文件覆盖保护
# =============================================================================
cat("\n【示例 7】文件覆盖保护\n")
cat("--------------------------------------\n")

test_file <- "output/protected_file.rds"

# 7.1 第一次写入
cat("第一次写入文件...\n")
write_table(demo_data, test_file)

# 7.2 尝试再次写入（会报错）
cat("\n尝试覆盖文件（未设置 overwrite=TRUE）...\n")
tryCatch({
  write_table(demo_data, test_file, overwrite = FALSE)
}, error = function(e) {
  cat("❌ 预期错误:", conditionMessage(e), "\n\n")
})

# 7.3 允许覆盖（会先备份）
cat("允许覆盖（overwrite=TRUE，会先备份）...\n")
write_table(demo_data, test_file, overwrite = TRUE, backup_before = TRUE)
cat("✅ 文件已更新，原文件已自动备份\n\n")


# =============================================================================
# 总结
# =============================================================================
cat("\n========== 示例运行完成 ==========\n")
cat("\nutils_io.R 模块提供的核心功能:\n")
cat("  ✅ normalize_path()     - 路径规范化\n")
cat("  ✅ backup_db()          - 数据库备份\n")
cat("  ✅ write_table()        - 通用数据写入\n")
cat("  ✅ read_table()         - 通用数据读取\n")
cat("  ✅ export_cross_matrix()- 杂交矩阵导出\n")
cat("  ✅ import_cross_matrix()- 历史矩阵导入\n")
cat("\n所有操作均遵循安全策略:\n")
cat("  - 默认不覆盖文件\n")
cat("  - 自动备份重要数据\n")
cat("  - 清晰的错误提示\n")
cat("  - 统一的路径处理\n")
cat("\n生成的文件位于: output/ 目录\n")
cat("数据库备份位于: data/db/backups/ 目录\n\n")
