# -------------------------------------------------------------------------
# 脚本名称：clean_parents_traits.R
# 功能描述：从 parents.特征特性 文本解析并填充结构化性状列
# -------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(stringr)
  library(jsonlite)
})

db_path <- "data/db/soy_cross.db"

if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)

# 1) 备份数据库
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_file <- file.path(dirname(db_path), paste0("soy_cross_backup_", ts, ".db"))
file.copy(db_path, backup_file, overwrite = TRUE)
message("📦 已备份数据库至：", backup_file)

con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)
if (!dbIsValid(con)) stop("❌ 数据库连接无效")
dbExecute(con, "PRAGMA foreign_keys = ON")
dbExecute(con, "PRAGMA busy_timeout = 5000")

sanitize_columns <- function(con, tbl) {
  if (!(tbl %in% dbListTables(con))) return(invisible(FALSE))
  fields <- dbListFields(con, tbl)
  to_fix <- fields[grepl("\\s", fields)]
  if (length(to_fix) == 0) return(invisible(FALSE))
  dbWithTransaction(con, {
    for (old in to_fix) {
      new <- gsub("\\s+", "", old)
      if (new != old && !(new %in% fields)) {
        sql <- paste0('ALTER TABLE "', tbl, '" RENAME COLUMN "', old, '" TO "', new, '"')
        dbExecute(con, sql)
        message("🔧 重命名字段：", old, " -> ", new)
      } else {
        message("⚠️ 跳过重命名：", old)
      }
    }
  })
  invisible(TRUE)
}

sanitize_columns(con, "parents")

# 预处理：移除 parents 表字段名中的空格
sanitize_columns <- function(con, tbl) {
  fields <- dbListFields(con, tbl)
  to_fix <- fields[grepl("\\s", fields)]
  if (length(to_fix) == 0) return(invisible(FALSE))
  dbBegin(con)
  for (old in to_fix) {
    new <- gsub("\\s+", "", old)
    if (new != old && !(new %in% fields)) {
      sql <- paste0('ALTER TABLE "', tbl, '" RENAME COLUMN "', old, '" TO "', new, '"')
      dbExecute(con, sql)
      message("🔧 重命名字段：", old, " -> ", new)
    } else {
      message("⚠️ 跳过重命名：", old)
    }
  }
  dbCommit(con)
  invisible(TRUE)
}

sanitize_columns(con, "parents")

# 2) 确保结构化列存在
need_cols <- c(
  "days_to_maturity INTEGER",
  "plant_height_cm INTEGER",
  "growth_habit TEXT",
  "leaf_shape TEXT",
  "flower_color TEXT",
  "pubescence_color TEXT",
  "hundred_seed_weight_g REAL",
  "seed_coat_color TEXT",
  "hilum_color TEXT",
  "protein_percent REAL",
  "oil_percent REAL",
  "traits_json TEXT"
)

existing <- dbGetQuery(con, "PRAGMA table_info(parents)")$name
for (def in need_cols) {
  col <- str_split_fixed(def, " ", 2)[,1]
  if (!col %in% existing) {
    dbExecute(con, paste0("ALTER TABLE parents ADD COLUMN ", def))
    message("➕ 添加列：", def)
  }
}

# 3) 读取 parents 原始数据
if (!"parents" %in% dbListTables(con)) stop("❌ 缺少# 3) 读取 parents 原始数据（健壮处理各列别名）
parents_df <- dbReadTable(con, "parents")
if (!"id" %in% names(parents_df)) stop("❌ parents 表缺少 id 列")
get_col <- function(df, candidates, default = NA_character_) {
  for (nm in candidates) if (nm %in% names(df)) return(df[[nm]])
  rep(default, nrow(df))
}
name_vec <- get_col(parents_df, c("name", "名称"))
traits_vec <- get_col(parents_df, c("特征特性", "特点", "traits", "trait"))
parents <- tibble(id = parents_df$id, name = name_vec, traits_text = traits_vec) 4) 解析函数集合
parse_traits <- function(text) {
  s <- ifelse(is.na(text), "", text)
  s <- str_squish(s)

  # helpers
  take_num <- function(pat) {
    m <- str_match(s, pat)[,2]
    suppressWarnings(as.numeric(m))
  }
  take_text <- function(pat) {
    m <- str_match(s, pat)[,2]
    ifelse(is.na(m), NA_character_, m)
  }

  days <- take_num("(\\d+(?:\\.\\d+)?)\\s*天")
  height <- take_num("高\\s*(\\d+(?:\\.\\d+)?)")
  habit <- take_text("(有限|无限|亚有限)")
  leaf  <- take_text("(椭圆叶|卵圆叶|披针叶|圆形叶|椭圆形|卵圆形)")
  flower <- take_text("(白花|紫花)")
  pubesc <- take_text("((?:灰色|棕色)茸毛)")
  hundred_w <- take_num("百粒重\\s*(\\d+(?:\\.\\d+)?)")
  coat <- take_text("([黄褐黑青黄淡褐]+)种皮")
  hilum <- take_text("([黄褐黑淡褐]+)(?:种)?脐")

  # 蛋白/油脂：常见格式 "42.29-18.47" 或 "41.81-22.25"
  nums <- str_match(s, "(\\d+(?:\\.\\d+)?)\\s*[-~/～]\\s*(\\d+(?:\\.\\d+)?)")
  protein <- suppressWarnings(as.numeric(nums[,2]))
  oil     <- suppressWarnings(as.numeric(nums[,3]))

  # 未解析的片段
  known_patterns <- c(
    "\\d+(?:\\.\\d+)?\\s*天", "高\\s*\\d+(?:\\.\\d+)?",
    "(有限|无限|亚有限)", "(椭圆叶|卵圆叶|披针叶|圆形叶|椭圆形|卵圆形)",
    "(白花|紫花)", "(灰色茸毛|棕色茸毛)",
    "百粒重\\s*\\d+(?:\\.\\d+)?",
    "[黄褐黑青黄淡褐]+种皮",
    "[黄褐黑淡褐]+(?:种)?脐",
    "\\d+(?:\\.\\d+)?\\s*[-~/～]\\s*\\d+(?:\\.\\d+)?"
  )
  leftover <- s
  for (pat in known_patterns) {
    leftover <- str_replace_all(leftover, pat, "")
  }
  leftover <- str_squish(leftover)
  traits_json <- if (nzchar(leftover)) toJSON(list(unparsed = leftover), auto_unbox = TRUE) else NA_character_

  list(
    days_to_maturity = days,
    plant_height_cm = height,
    growth_habit = habit,
    leaf_shape = leaf,
    flower_color = flower,
    pubescence_color = pubesc,
    hundred_seed_weight_g = hundred_w,
    seed_coat_color = coat,
    hilum_color = hilum,
    protein_percent = protein,
    oil_percent = oil,
    traits_json = traits_json
  )
}

parsed <- lapply(parents$traits_text, parse_traits)
parsed_df <- bind_rows(parsed)

result <- bind_cols(
  tibble(id = parents$id),
  parsed_df
)

# 5) 写入（逐行 UPDATE，事务保护）
n_total <- nrow(result)
n_filled <- 0L

update_sql <- "
UPDATE parents SET
  days_to_maturity = ?,
  plant_height_cm = ?,
  growth_habit = ?,
  leaf_shape = ?,
  flower_color = ?,
  pubescence_color = ?,
  hundred_seed_weight_g = ?,
  seed_coat_color = ?,
  hilum_color = ?,
  protein_percent = ?,
  oil_percent = ?,
  traits_json = ?
WHERE id = ?;
"

dbWithTransaction(con, {
  for (i in seq_len(n_total)) {
    r <- result[i,]
    dbExecute(con, update_sql, params = list(
      r$days_to_maturity,
      r$plant_height_cm,
      r$growth_habit,
      r$leaf_shape,
      r$flower_color,
      r$pubescence_color,
      r$hundred_seed_weight_g,
      r$seed_coat_color,
      r$hilum_color,
      r$protein_percent,
      r$oil_percent,
      r$traits_json,
      r$id
    ))
    n_filled <- n_filled + 1L
  }
})

message("✅ 清洗完成：", n_filled, "/", n_total, " 行已更新。")

# 6) 简要统计回显
fields <- dbListFields(con, "parents")
cols <- c("id", if ("name" %in% fields) "name", "days_to_maturity", "plant_height_cm", "growth_habit",
          "leaf_shape", "flower_color", "pubescence_color", "hundred_seed_weight_g", "seed_coat_color", "hilum_color",
          "protein_percent", "oil_percent")
sql <- paste0("SELECT ", paste(cols, collapse = ", "),
              " FROM parents WHERE days_to_maturity IS NOT NULL OR plant_height_cm IS NOT NULL OR hundred_seed_weight_g IS NOT NULL LIMIT 10")
preview <- dbGetQuery(con, sql)
message("👀 解析预览（最多10行）：")
print(preview)