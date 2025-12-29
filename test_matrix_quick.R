# 快速测试矩阵模块
source('R/mod_matrix.R')

cat("\n=== 测试1: 创建矩阵框架 ===\n")
framework <- create_matrix_framework()
cat(paste('矩阵维度:', nrow(framework$matrix), 'x', ncol(framework$matrix)), '\n')
cat('前5个亲本:\n')
print(framework$parent_names[1:5])

cat("\n=== 测试2: 填充矩阵数据 ===\n")
filled <- fill_matrix_data(framework)
cat('矩阵预览（前5x5）:\n')
print(filled[1:5, 1:5])

cat("\n=== 测试3: 矩阵统计 ===\n")
stats <- matrix_summary(filled)
print(stats)

cat("\n✅ 测试完成！\n")
