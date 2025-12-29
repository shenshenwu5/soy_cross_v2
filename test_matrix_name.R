# 测试新增的 name 填充选项
source('R/mod_matrix.R')

cat("\n=== 测试: 使用组合名称填充矩阵 ===\n")

# 创建使用组合名称填充的矩阵
matrix_with_name <- create_cross_matrix_view(
  fill_value = "name",
  show_reciprocal = TRUE
)

cat("\n矩阵预览（前10x10）:\n")
print(matrix_with_name[1:10, 1:10])

cat("\n=== 测试: 对比不同填充值 ===\n")

# 对比批次名称
cat("\n1. 批次名称填充:\n")
matrix_batch <- create_cross_matrix_view(fill_value = "batch")
print(matrix_batch[1:5, 1:5])

# 对比组合名称
cat("\n2. 组合名称填充:\n")
matrix_name <- create_cross_matrix_view(fill_value = "name")
print(matrix_name[1:5, 1:5])

cat("\n✅ 测试完成！\n")
