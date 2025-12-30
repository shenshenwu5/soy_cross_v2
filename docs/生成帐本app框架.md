# 帐本生成 App 框架设计

## 1. 功能概述

本模块旨在集成现有的核心算法函数（`soyplant` 包或 `mod_cross.R` 模块），在 Shiny 应用中实现**组合编号生成**、**田间种植排图**、**数据库回写**及**电子帐本导出**的完整流程。

**核心原则**：不重复造轮子，直接调用已调试好的现有函数。

## 2. 依赖环境与函数来源

确保以下函数在 R 环境中可用。

### 2.1 核心算法 (来自 `soyplant` 包)
*需确保安装并加载 `soyplant` 包，调用时建议使用 `soyplant::` 前缀以明确来源。*

- `soyplant::get_combination(data, prefix, startN, only, order)`: 生成组合编号。
- `soyplant::planting(mydata, interval, s_prefix, place, rp, digits, ck, rows)`: 生成种植排图。
- `soyplant::savewb(origin, planting, myview, combi_matrix, filename, overwrite)`: 导出 Excel 帐本。
- `soyplant::combination_matrix(data)`: 生成组合矩阵（用于导出）。

### 2.2 数据库与业务逻辑 (来自 `R/mod_cross.R`)
*需 `source("R/mod_cross.R")`。*

- `get_crosses_by_batch(batch)`: 获取指定批次的杂交记录。
- `join_cross_parents(crosses_data)`: 关联亲本详细信息（自动处理 `female_`/`male_` 前缀及 `ma`/`pa` 兼容字段）。
- `update_cross_names_from_df(data, batch)`: 将生成的组合名称回写到数据库。

### 2.3 依赖包
```r
library(shiny)
library(dplyr)
library(DT)
library(soyplant) # 自研包
source("R/mod_cross.R")
```

## 3. 业务逻辑流程 (示例代码)

以下代码展示了从数据库读取到生成帐本的完整逻辑链条，已在本地调试通过。

```r
# 1. 准备环境与参数
library(soyplant)
library(dplyr)
source("R/mod_cross.R")

# 参数设置
MYBATCH <- "2025春季"
MYPRE   <- "G25c6"
PLACE   <- "武汉"
ROWS    <- 2
RP      <- 1
INTERVAL<- 999
DIGITS  <- 3
START_N <- 1

# 2. 获取数据并关联亲本
# 获取基础杂交记录
mycross <- get_crosses_by_batch(MYBATCH)

# 关联亲本信息
# join_cross_parents 会自动处理:
# - 添加 female_XX, male_XX 亲本性状列
# - 添加 ma, pa 字段用于兼容 get_combination
# - 按 female_Trait, male_Trait 交替排序性状列
mydata <- join_cross_parents(mycross)

# 检查数据
if (nrow(mydata) == 0) stop("❌ 未获取到数据")

# (可选) 根据父本母本名称排序，确保编号有序
mydata <- mydata %>% arrange(desc(ma), desc(pa))

# 3. 生成组合编号 (Step 1: Combination)
my_combi <- soyplant::get_combination(
  mydata,          # join_cross_parents 已准备好 ma/pa 列
  prefix = MYPRE,
  startN = START_N,
  only = TRUE,
  order = FALSE
)

# 4. 回写数据库 (Step 2: Database Update)
# 将生成的 name 更新回 crosses 表
# update_cross_names_from_df 需要 name, ma, pa 列
update_cross_names_from_df(my_combi[, c("name", "ma", "pa")], MYBATCH)

# 5. 生成种植排图 (Step 3: Planting)
# 补充年份信息（如果需要）
my_combi$year <- format(Sys.Date(), "%Y")

planted <- my_combi %>%
  soyplant::planting(
    interval = INTERVAL,
    s_prefix = MYPRE,
    place = PLACE,
    rp = RP,
    digits = DIGITS,
    ck = NULL,
    rows = ROWS
  )

# 6. 导出 Excel (Step 4: Export)
# 定义视图列
fields <- c("fieldid", "code", "place", "stageid", "name", "rows", "line_number", "rp")
myview_cols <- intersect(c(fields, "ma", "pa"), names(planted))

filename <- paste0("output/", MYPRE, "_book.xlsx")
if(!dir.exists("output")) dir.create("output")

soyplant::savewb(
  origin = my_combi,
  planting = planted,
  myview = planted[, myview_cols],
  combi_matrix = soyplant::combination_matrix(my_combi),
  filename = filename,
  overwrite = TRUE
)
```

## 4. UI 界面设计建议

在 `apps/run_book_app.R` (独立应用) 或 `apps/run_cross_app.R` (集成应用) 中实现。

### 4.1 参数配置区
| 参数名 | 变量名 | 默认值/说明 |
| :--- | :--- | :--- |
| **批次选择** | `input$gen_batch` | 动态读取数据库中的批次 |
| **组合前缀** | `input$gen_prefix` | 如 "G25c6" |
| **起始编号** | `input$gen_start_n` | 1 |
| **编号位数** | `input$gen_digits` | 3 |
| **种植地点** | `input$gen_place` | "武汉" |
| **种植行数** | `input$gen_rows` | 2 |
| **重复数** | `input$gen_rp` | 1 |
| **间隔** | `input$gen_interval` | 999 |

### 4.2 操作区
设计三个主要操作按钮：
1.  **生成预览** (`btn_calc_preview`): 调用 `get_combination` 和 `planting`，在界面表格展示结果，**不**写库，**不**导出。
2.  **回写数据库** (`btn_save_db_name`): 确认无误后，调用 `update_cross_names_from_df` 将组合名写入数据库。
3.  **导出 Excel** (`btn_export_xlsx`): 调用 `savewb` 下载文件。

### 4.3 结果展示
使用 `DT::dataTableOutput` 分别展示：
- **组合预览**: `my_combi` 数据 (重点检查编号是否正确)
- **排图预览**: `planted` 数据 (重点检查田间排列是否符合预期)
