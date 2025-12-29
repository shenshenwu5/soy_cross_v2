#devtools::load_all("E:/FangCloudSync/R_WD360/Project/soyplant")
library(soyplant)
library(openxlsx)
library(dplyr)

mycross<-get_crosses_by_batch("一")
mycross<-join_cross_parents(mycross)
# 生成账本字段定义
fields <- c("fieldid", "code", "place", "stageid", "name", "rows", "line_number", "rp")

# 组合前缀与文件路径
myfilename <- paste0("output/",MYPRE,"test.xlsx",sep="")

# 构建组合数据
mydata <- data.frame(
  ma = mycross$male_名称,
  pa = mycross$female_名称,
  memo = paste(mycross$male_特征特性, mycross$female_特征特性, sep = "+")
)
#增加排序
mydata <- mydata %>%
  arrange(desc(ma), desc(pa))


# 一：生成组合编码
my_combi <- get_combination(
  mydata,
  prefix = MYPRE,
  startN = 1,
  only = TRUE,
  order = FALSE
)

# 添加年份
my_combi$year <- 2025




# 二：生成种植计划
planted <- my_combi |>
  planting(
    interval = 999,
    s_prefix = MYPRE,
    place = "武汉",
    rp = 1,
    digits = 3,
    ck = NULL,
    rows = 2,
    #startN=1
  )

# 三：保存 Excel 工作簿
savewb(
  origin = my_combi,
  planting = planted,
  myview = planted[, c(fields, "ma", "pa")],
  combi_matrix = combination_matrix(my_combi),
  filename = myfilename,
  overwrite = FALSE
)
