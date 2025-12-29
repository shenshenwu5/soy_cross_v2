# 加载模块
source("R/mod_analysis.R")

# --- 查询特定亲本详情 ---
# 替换 "中黄13" 为您想查询的亲本名称
result <- analyze_parent_details("中黄13")
get_parent_usage_stats()

find_unused_partners("中黄13","male")
