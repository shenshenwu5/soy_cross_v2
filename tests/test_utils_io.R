# =============================================================================
# 单元测试: utils_io.R 模块测试
# 文件路径: tests/test_utils_io.R
# 测试内容: 路径规范化、备份、读写、矩阵导入导出
# =============================================================================

# ---- 加载依赖 ----
if (!requireNamespace("testthat", quietly = TRUE)) {
  cat("ℹ️  安装 testthat 包以运行测试...\n")
  install.packages("testthat")
}

library(testthat)
source("../R/utils_io.R")

# ---- 测试环境准备 ----
# 创建临时测试目录
test_dir <- file.path(tempdir(), "utils_io_test")
if (dir.exists(test_dir)) {
  unlink(test_dir, recursive = TRUE)
}
dir.create(test_dir, recursive = TRUE)

# 创建测试数据库
test_db <- file.path(test_dir, "test.db")
con <- DBI::dbConnect(RSQLite::SQLite(), test_db)

# 初始化测试表
DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS parents (
    id INTEGER PRIMARY KEY,
    name TEXT UNIQUE NOT NULL,
    active INTEGER DEFAULT 1
  )
")

DBI::dbExecute(con, "
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

# 插入测试数据
parents_data <- data.frame(
  id = 1:5,
  name = c("中黄301", "冀豆17", "华豆17", "徐豆18", "冀豆12"),
  active = 1
)
DBI::dbWriteTable(con, "parents", parents_data, append = TRUE)

crosses_data <- data.frame(
  female_id = c(1, 1, 2, 2, 3),
  male_id = c(4, 5, 4, 5, 4),
  batch = c("2025春季", "2025春季", "2025春季", "2025秋季", "2025秋季"),
  status = "planned"
)
DBI::dbWriteTable(con, "crosses", crosses_data, append = TRUE)

DBI::dbDisconnect(con)


# =============================================================================
# 测试 1: normalize_path() - 路径规范化
# =============================================================================
cat("\n========== 测试 1: normalize_path() ==========\n")

test_that("路径规范化 - 基本功能", {
  # 相对路径转绝对路径
  path <- normalize_path("data/test.db")
  expect_true(grepl("^(/|[A-Za-z]:)", path))
  
  # Windows 反斜杠转正斜杠
  path <- normalize_path("data\\test.db")
  expect_true(grepl("/", path))
  expect_false(grepl("\\\\", path))
})

test_that("路径规范化 - 必须存在检查", {
  # 不存在的路径，must_exist = TRUE 应报错
  expect_error(
    normalize_path("nonexistent/file.txt", must_exist = TRUE),
    "路径不存在"
  )
})

test_that("路径规范化 - 自动创建目录", {
  test_path <- file.path(test_dir, "auto_create/subdir/file.txt")
  
  # 自动创建父目录
  result <- normalize_path(test_path, create_dir = TRUE)
  
  # 验证目录已创建
  expect_true(dir.exists(dirname(test_path)))
})

test_that("路径规范化 - 空路径检查", {
  expect_error(normalize_path(NULL), "路径参数不能为空")
  expect_error(normalize_path(""), "路径参数不能为空")
})


# =============================================================================
# 测试 2: backup_db() - 数据库备份
# =============================================================================
cat("\n========== 测试 2: backup_db() ==========\n")

test_that("数据库备份 - 基本功能", {
  # 执行备份
  backup_file <- backup_db(test_db)
  
  # 验证备份文件存在
  expect_true(file.exists(backup_file))
  
  # 验证文件大小一致
  original_size <- file.info(test_db)$size
  backup_size <- file.info(backup_file)$size
  expect_equal(backup_size, original_size)
})

test_that("数据库备份 - 自定义备份目录", {
  custom_backup_dir <- file.path(test_dir, "custom_backups")
  
  backup_file <- backup_db(test_db, dest_dir = custom_backup_dir, backup_name = "manual")
  
  expect_true(file.exists(backup_file))
  expect_true(grepl("custom_backups/manual", backup_file))
})

test_that("数据库备份 - 不存在的数据库", {
  expect_error(
    backup_db("nonexistent.db"),
    "路径不存在"
  )
})


# =============================================================================
# 测试 3: write_table() 和 read_table() - 通用读写
# =============================================================================
cat("\n========== 测试 3: write_table() 和 read_table() ==========\n")

# 准备测试数据
test_df <- data.frame(
  id = 1:5,
  name = c("张三", "李四", "王五", "赵六", "孙七"),
  score = c(85, 92, 78, 88, 95),
  stringsAsFactors = FALSE
)

test_that("写入和读取 RDS 文件", {
  rds_path <- file.path(test_dir, "test_data.rds")
  
  # 写入
  write_table(test_df, rds_path)
  expect_true(file.exists(rds_path))
  
  # 读取
  result <- read_table(rds_path)
  expect_equal(nrow(result$data), 5)
  expect_equal(result$data$name[1], "张三")
})

test_that("写入和读取 CSV 文件", {
  csv_path <- file.path(test_dir, "test_data.csv")
  
  # 写入
  write_table(test_df, csv_path)
  expect_true(file.exists(csv_path))
  
  # 读取
  result <- read_table(csv_path)
  expect_equal(nrow(result), 5)
  expect_equal(result$name[1], "张三")
})

test_that("写入和读取 Excel 文件（如果有 openxlsx）", {
  skip_if_not_installed("openxlsx")
  
  xlsx_path <- file.path(test_dir, "test_data.xlsx")
  
  # 写入单个数据框
  write_table(test_df, xlsx_path)
  expect_true(file.exists(xlsx_path))
  
  # 读取
  result <- read_table(xlsx_path, sheet = 1)
  expect_equal(nrow(result), 5)
})

test_that("写入多工作表 Excel", {
  skip_if_not_installed("openxlsx")
  
  xlsx_path <- file.path(test_dir, "multi_sheet.xlsx")
  
  df_list <- list(
    "成绩单" = test_df,
    "排名" = test_df[order(test_df$score, decreasing = TRUE), ]
  )
  
  write_table(df_list, xlsx_path)
  expect_true(file.exists(xlsx_path))
  
  # 读取第一个工作表
  result <- read_table(xlsx_path, sheet = "成绩单")
  expect_equal(nrow(result), 5)
})

test_that("文件覆盖保护", {
  rds_path <- file.path(test_dir, "protected.rds")
  
  # 第一次写入
  write_table(test_df, rds_path)
  
  # 第二次写入，不允许覆盖
  expect_error(
    write_table(test_df, rds_path, overwrite = FALSE),
    "文件已存在"
  )
  
  # 允许覆盖（会产生 message）
  expect_message(
    write_table(test_df, rds_path, overwrite = TRUE)
  )
})


# =============================================================================
# 测试 4: export_cross_matrix() - 导出杂交矩阵
# =============================================================================
cat("\n========== 测试 4: export_cross_matrix() ==========\n")

test_that("导出杂交矩阵 - 所有批次", {
  xlsx_path <- file.path(test_dir, "matrix_all.xlsx")
  
  mat <- export_cross_matrix(test_db, out_path = xlsx_path, format = "xlsx")
  
  # 验证文件已创建
  expect_true(file.exists(xlsx_path))
  
  # 验证矩阵维度
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), 3)  # 3个母本
  expect_equal(ncol(mat), 2)  # 2个父本
})

test_that("导出杂交矩阵 - 指定批次", {
  rds_path <- file.path(test_dir, "matrix_2025春季.rds")
  
  mat <- export_cross_matrix(test_db, batch = "2025春季", out_path = rds_path, format = "rds")
  
  expect_true(file.exists(rds_path))
  
  # 验证矩阵内容
  expect_equal(sum(!is.na(mat)), 3)  # 3个组合
})

test_that("导出杂交矩阵 - 空结果", {
  expect_warning(
    export_cross_matrix(test_db, batch = "不存在的批次"),
    "查询结果为空"
  )
})


# =============================================================================
# 测试 5: import_cross_matrix() - 导入历史矩阵
# =============================================================================
cat("\n========== 测试 5: import_cross_matrix() ==========\n")

test_that("导入历史矩阵 - Excel 格式", {
  skip_if_not_installed("openxlsx")
  
  # 先导出一个矩阵
  xlsx_path <- file.path(test_dir, "export_for_import.xlsx")
  export_cross_matrix(test_db, batch = "2025春季", out_path = xlsx_path)
  
  # 导入
  result <- import_cross_matrix(xlsx_path, batch = "2025春季导入")
  
  # 验证结果
  expect_equal(nrow(result), 3)  # 3个组合
  expect_true("female_id" %in% names(result))
  expect_true("male_id" %in% names(result))
  expect_true("batch" %in% names(result))
})

test_that("导入历史矩阵 - RDS 格式", {
  # 创建测试矩阵
  mat <- matrix(NA, nrow = 3, ncol = 2)
  rownames(mat) <- c("中黄301", "冀豆17", "华豆17")
  colnames(mat) <- c("徐豆18", "冀豆12")
  mat[1, 1] <- "批次A"
  mat[2, 2] <- "批次B"
  
  rds_path <- file.path(test_dir, "test_matrix.rds")
  saveRDS(mat, rds_path)
  
  # 导入
  result <- import_cross_matrix(rds_path, batch = "历史批次")
  
  expect_equal(nrow(result), 2)  # 2个非 NA 组合
  expect_equal(result$batch[1], "历史批次")
})

test_that("导入历史矩阵 - 跳过 NA", {
  # 创建包含 NA 的矩阵数据框
  df <- data.frame(
    母本 = c("A", "B"),
    父本1 = c("X", NA),
    父本2 = c(NA, "Y"),
    stringsAsFactors = FALSE
  )
  
  csv_path <- file.path(test_dir, "matrix_with_na.csv")
  write.csv(df, csv_path, row.names = FALSE)
  
  # 导入（跳过 NA）
  result <- import_cross_matrix(csv_path, skip_na = TRUE)
  
  expect_equal(nrow(result), 2)  # 只有2个非 NA 组合
})


# =============================================================================
# 测试 6: 错误处理和边界情况
# =============================================================================
cat("\n========== 测试 6: 错误处理和边界情况 ==========\n")

test_that("空数据框写入", {
  empty_df <- data.frame()
  rds_path <- file.path(test_dir, "empty.rds")
  
  # 空数据框也会产生 message
  expect_message(write_table(empty_df, rds_path))
})

test_that("不支持的文件格式", {
  # 需要先创建文件
  unknown_file <- file.path(test_dir, "test.unknown")
  writeLines("test", unknown_file)
  
  expect_error(
    read_table(unknown_file),
    "不支持的文件格式"
  )
})

test_that("读取不存在的文件", {
  expect_error(
    read_table("nonexistent.csv"),
    "路径不存在"
  )
})


# =============================================================================
# 清理测试环境
# =============================================================================
cat("\n========== 清理测试环境 ==========\n")

# 清理临时文件
unlink(test_dir, recursive = TRUE)

cat("✅ 所有测试完成！\n")
cat("📊 测试总结:\n")
cat("   - normalize_path(): ✅\n")
cat("   - backup_db(): ✅\n")
cat("   - write_table() / read_table(): ✅\n")
cat("   - export_cross_matrix(): ✅\n")
cat("   - import_cross_matrix(): ✅\n")
cat("   - 错误处理: ✅\n")
