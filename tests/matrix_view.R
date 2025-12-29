# =============================================================================
# 脚本名称：example_matrix_view.R
# 功能描述：矩阵视图模块使用示例（非 Shiny）
# 创建日期：2025-12-29
# =============================================================================

# 加载矩阵视图模块
source("R/mod_matrix.R")

# =============================================================================
# 示例1：创建基础矩阵框架
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例1：创建基础矩阵框架\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 创建空框架
framework <- create_matrix_framework(
  filter_active = TRUE,
  name_field = "name"
)

cat("框架信息：\n")
cat(glue::glue("  亲本数量：{framework$n_parents}\n"))
cat(glue::glue("  矩阵维度：{nrow(framework$matrix)} × {ncol(framework$matrix)}\n"))
cat("\n矩阵预览（前5×5）：\n")
print(framework$matrix[1:min(5, nrow(framework$matrix)), 
                       1:min(5, ncol(framework$matrix))])


# =============================================================================
# 示例2：填充矩阵数据（显示批次）
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例2：填充矩阵数据 - 显示批次名称\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

filled_matrix <- fill_matrix_data(
  framework = framework,
  fill_value = "batch",
  show_reciprocal = TRUE
)

cat("\n填充后矩阵预览（前10×10）：\n")
print(filled_matrix[1:min(10, nrow(filled_matrix)), 
                    1:min(10, ncol(filled_matrix))])


# =============================================================================
# 示例3：一步创建完整矩阵视图
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例3：一步创建完整矩阵视图\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

matrix_full <- create_cross_matrix_view(
  filter_active = TRUE,
  name_field = "name",
  fill_value = "batch",
  show_reciprocal = TRUE
)


# =============================================================================
# 示例4：使用不同的填充值
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例4：使用不同的填充值\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 4.1 显示状态
cat("4.1 显示状态信息：\n")
matrix_status <- create_cross_matrix_view(
  fill_value = "status",
  show_reciprocal = FALSE
)
cat("矩阵预览（前5×5）：\n")
print(matrix_status[1:min(5, nrow(matrix_status)), 
                    1:min(5, ncol(matrix_status))])

# 4.2 显示简单标记
cat("\n4.2 显示简单标记：\n")
matrix_mark <- create_cross_matrix_view(
  fill_value = "mark",
  show_reciprocal = TRUE
)
cat("矩阵预览（前5×5）：\n")
print(matrix_mark[1:min(5, nrow(matrix_mark)), 
                  1:min(5, ncol(matrix_mark))])

# 4.3 显示组合名称
cat("\n4.3 显示组合名称：\n")
matrix_name <- create_cross_matrix_view(
  fill_value = "name",
  show_reciprocal = TRUE
)
cat("矩阵预览（前5×5）：\n")
print(matrix_name[1:min(5, nrow(matrix_name)), 
                  1:min(5, ncol(matrix_name))])


# =============================================================================
# 示例5：按批次过滤
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例5：按批次过滤\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 首先查看有哪些批次
library(RSQLite)
con <- dbConnect(SQLite(), "data/db/soy_cross.db")
batches <- dbGetQuery(con, "SELECT DISTINCT batch FROM crosses")$batch
dbDisconnect(con)

cat("数据库中的批次：\n")
print(batches)

if (length(batches) > 0) {
  cat("\n筛选第一个批次的数据：\n")
  matrix_filtered <- create_cross_matrix_view(
    batch_filter = batches[1],
    fill_value = "batch"
  )
  cat("矩阵预览（前10×10）：\n")
  print(matrix_filtered[1:min(10, nrow(matrix_filtered)), 
                        1:min(10, ncol(matrix_filtered))])
}


# =============================================================================
# 示例6：矩阵统计分析
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例6：矩阵统计分析\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 创建矩阵
matrix_for_stats <- create_cross_matrix_view(
  fill_value = "batch",
  show_reciprocal = TRUE
)

# 生成统计摘要
summary_stats <- matrix_summary(matrix_for_stats)

cat("📊 矩阵统计摘要：\n\n")
print(summary_stats, row.names = FALSE)


# =============================================================================
# 示例7：使用 ID 作为标签（更紧凑）
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例7：使用 ID 作为标签\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

matrix_with_id <- create_cross_matrix_view(
  name_field = "id",  # 使用 ID 而非名称
  fill_value = "mark",
  show_reciprocal = TRUE
)

cat("使用 ID 的矩阵预览（前15×15）：\n")
print(matrix_with_id[1:min(15, nrow(matrix_with_id)), 
                     1:min(15, ncol(matrix_with_id))])


# =============================================================================
# 示例8：导出矩阵到文件
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例8：导出矩阵到文件\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 创建矩阵
export_matrix <- create_cross_matrix_view(
  fill_value = "batch",
  show_reciprocal = TRUE
)

# 导出为 CSV
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

csv_file <- file.path(output_dir, "cross_matrix.csv")
write.csv(export_matrix, csv_file, fileEncoding = "UTF-8")
cat(glue::glue("✅ 矩阵已导出到：{csv_file}\n"))

# 导出为 Excel（如果安装了 openxlsx）
if (requireNamespace("openxlsx", quietly = TRUE)) {
  xlsx_file <- file.path(output_dir, "cross_matrix.xlsx")
  openxlsx::write.xlsx(
    as.data.frame(export_matrix), 
    xlsx_file,
    rowNames = TRUE
  )
  cat(glue::glue("✅ 矩阵已导出到：{xlsx_file}\n"))
}


# =============================================================================
# 总结
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("✅ 所有示例运行完成！\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("💡 使用提示：\n")
cat("  1. 使用 create_matrix_framework() 创建空框架\n")
cat("  2. 使用 fill_matrix_data() 填充数据\n")
cat("  3. 使用 create_cross_matrix_view() 一步到位\n")
cat("  4. fill_value 可选：batch, status, count, mark, name\n")
cat("  5. name_field 可选：name（名称）或 id（ID）\n")
cat("  6. 使用 matrix_summary() 查看统计信息\n")
cat("  7. 运行 Shiny 应用：source('scripts/app_matrix_view.R')\n\n")
