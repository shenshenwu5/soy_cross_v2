# =============================================================================
# 测试脚本：验证 mod_cross.R 修复
# =============================================================================

# 加载模块
source("R/mod_cross.R")

# 测试1：使用实际存在的亲本名称
cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("测试1：使用数据库中实际存在的亲本名称\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 从数据库获取一些亲本名称
library(RSQLite)
con <- dbConnect(SQLite(), "data/db/soy_cross.db")
all_parents <- dbGetQuery(con, "SELECT name FROM parents LIMIT 20")$name
dbDisconnect(con)

cat("数据库中的亲本（前20个）：\n")
print(all_parents)

# 选择前5个作为母本，后5个作为父本
mothers_test <- all_parents[1:3]
fathers_test <- all_parents[4:6]

cat("\n选择的母本：", paste(mothers_test, collapse = ", "), "\n")
cat("选择的父本：", paste(fathers_test, collapse = ", "), "\n\n")

# 创建杂交计划
tryCatch({
  result <- create_cross_plan(
    batch_name = "测试批次_2025",
    mothers = mothers_test,
    fathers = fathers_test,
    include_reciprocal = TRUE,
    status = "planned",
    use_id = FALSE
  )
  
  cat("\n✅ 测试成功！\n")
  cat("\n计划摘要：\n")
  print(result$summary)
  
  if (nrow(result$new_crosses) > 0) {
    cat("\n新创建的组合：\n")
    print(result$new_crosses)
  }
  
}, error = function(e) {
  cat("\n❌ 测试失败！\n")
  cat("错误信息：", e$message, "\n")
})

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("测试完成\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
