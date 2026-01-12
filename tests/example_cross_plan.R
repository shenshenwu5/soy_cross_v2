# =============================================================================
# 脚本名称：example_cross_plan.R
# 功能描述：杂交计划模块使用示例
# 创建日期：2025-12-29
# =============================================================================

# 加载杂交计划模块
source("R/mod_cross.R")

# 尝试加载配置
if (file.exists("config/config.R")) {
  source("config/config.R")
  db_path <- SoyCross$config$paths$db_path
} else {
  db_path <- "data/db/soy_cross.db"
}

get_crosses_by_batch("一")
# =============================================================================
# 示例1：使用亲本名称创建杂交计划
# =============================================================================

cat("\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例1：使用亲本名称创建杂交计划\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 定义母本和父本列表
mothers_example1 <- c("中黄301", "南农66", "皖宿112")
fathers_example1 <- c("天辰6号", "油6019", "南农47")

# 创建杂交计划
result1 <- create_cross_plan(
  batch_name = "2025春季批次",
  mothers = mothers_example1,
  fathers = fathers_example1,
  include_reciprocal = TRUE,
  status = "planned",
  use_id = FALSE
)

#
clear_cross_batches_db(
  batch_names = c("2025春季批次"),
  preview = FALSE,
  db_path = db_path
)



# 查看结果
cat("\n📊 计划摘要：\n")
print(result1$summary)

cat("\n✅ 新创建的组合（前10条）：\n")
print(head(result1$new_crosses, 10))

if (nrow(result1$skipped) > 0) {
  cat("\n⏭️  跳过的已存在组合：\n")
  print(result1$skipped)
}


# =============================================================================
# 示例2：使用亲本ID创建杂交计划（更高效）
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例2：使用亲本ID创建杂交计划\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 定义母本和父本ID列表
mothers_example2 <- c("P0001", "P0007", "P0014")
fathers_example2 <- c("P0101", "P0102", "P0103")

# 创建杂交计划
result2 <- create_cross_plan(
  batch_name = "2025夏季批次",
  mothers = mothers_example2,
  fathers = fathers_example2,
  include_reciprocal = TRUE,
  status = "planned",
  use_id = TRUE  # 使用ID模式
)


# =============================================================================
# 示例3：仅创建正交，不包含反交
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例3：仅创建正交组合\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

mothers_example3 <- c("中黄340", "华豆17")
fathers_example3 <- c("徐豆31", "徐豆32")

result3 <- create_cross_plan(
  batch_name = "2025秋季批次",
  mothers = mothers_example3,
  fathers = fathers_example3,
  include_reciprocal = FALSE,  # 不包含反交
  status = "planned",
  use_id = FALSE
)


# =============================================================================
# 示例4：查询批次统计信息
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例4：查询批次统计信息\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 查询所有批次的统计
all_batches <- summarize_cross_batches_db()
cat("📊 所有批次统计：\n")
print(all_batches)

# 查询特定批次
specific_batches <- summarize_cross_batches_db(
  batch_filter = c("2025春季批次", "2025夏季批次")
)
cat("\n📊 特定批次统计：\n")
print(specific_batches)


# =============================================================================
# 示例5：检查特定组合是否存在
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例5：检查特定组合是否存在\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 使用名称检查
exists1 <- has_cross("中黄301", "天辰6号", use_id = FALSE)
cat(glue::glue("中黄301/天辰6号 是否存在：{exists1}\n"))

# 使用ID检查
exists2 <- has_cross("P0001", "P0101", use_id = TRUE)
cat(glue::glue("P0001/P0101 是否存在：{exists2}\n"))


# =============================================================================
# 示例6：大批量杂交计划（真实场景）
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("示例6：大批量杂交计划\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

# 从数据库读取符合条件的亲本
library(RSQLite)
con <- dbConnect(SQLite(), db_path)

# 选择转基因亲本作为母本
mothers_bulk <- dbGetQuery(con, "
  SELECT name FROM parents 
  WHERE 转基因 = 'G2' 
  LIMIT 10
")$name

# 选择常规亲本作为父本
fathers_bulk <- dbGetQuery(con, "
  SELECT name FROM parents 
  WHERE 转基因 = '否' 
  LIMIT 15
")$name

dbDisconnect(con)

cat(glue::glue("母本数量：{length(mothers_bulk)}\n"))
cat(glue::glue("父本数量：{length(fathers_bulk)}\n"))
cat(glue::glue("预计生成：{length(mothers_bulk) * length(fathers_bulk)} 个正交组合\n\n"))

# 创建大批量计划
result6 <- create_cross_plan(
  batch_name = "2025转基因×常规批次",
  mothers = mothers_bulk,
  fathers = fathers_bulk,
  include_reciprocal = TRUE,
  status = "planned",
  use_id = FALSE
)


# =============================================================================
# 总结
# =============================================================================

cat("\n\n", paste(rep("=", 70), collapse = ""), "\n")
cat("✅ 所有示例运行完成！\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("💡 使用提示：\n")
cat("  1. 使用亲本ID比名称查询更高效\n")
cat("  2. 批次名称建议使用有意义的命名，便于后续管理\n")
cat("  3. 系统会自动跳过已存在的组合，避免重复\n")
cat("  4. 反交默认开启，如不需要可设置 include_reciprocal = FALSE\n")
cat("  5. 可使用 has_cross() 提前检查组合是否存在\n")
cat("  6. 使用 summarize_cross_batches_db() 查看批次统计\n\n")





##亲本1
Nfother_names<-select_parent(转基因=="否",
                             str_detect(审定编号, "2023")|str_detect(审定编号, "2024"),
                             str_detect(适宜区域, "南片")|
                               str_detect(适宜区域, "淮北")|
                               str_detect(适宜区域, "河南")|
                               str_detect(审定编号, "皖审")
)

Nfother_names<-union(Nfother_names,c("菏育6号"))
#亲本2
mather_names<-c("天辰6号","油6019","南农47","冀农科022","冀农科091",
                "中黄340","华豆17","徐豆31","徐豆32","GM25H056","GM25H057")

p1<-c("中黄301","南农66","皖宿112","华豆17","赣农科120",
      "NAM0416","郓豆1号","中豆57","23WW011344","23WW011350","23WW011442",
      "23WW011449","23WW011720","菏育6号","中黄340")
#p2<-read.table("clipboard",header=FALSE)
p2<-c("GLHJD_SY03","GLHJD_SY13")
#第二步-配置转基因杂交组合
set.seed(2356)
mycross<-run_cross_plan(n = 28,
                        p1,
                        p2,
                        content_value = MYPRE
)
