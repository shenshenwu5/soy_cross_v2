#devtools::load_all("E:/FangCloudSync/R_WD360/Project/soyplant")
library(soyplant)
#devtools::install_github("zhaoqingsonga/soyplant")
library(openxlsx)
source("R/mainfunction.R")
library(dplyr)
library(stringr)
#输出二维矩阵
mycross<-export_cross_matrix()

#配置杂交组合，
#第一步亲本筛选
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
MYPRE<-"G25b1"
set.seed(2356)
mycross<-run_cross_plan(n = 28,
                        p1,
                        p2,
                        content_value = MYPRE
)

#批次统计
summarize_cross_batches()

#批次筛选
mycross<-filter_cross_by_batches(c(MYPRE))

#批次删除
#clear_cross_batches(c("G25b1"),preview = FALSE)


#统计做杂交情况
stac_parents<-merge_mother_father_stats()


