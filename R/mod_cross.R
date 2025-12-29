# =============================================================================
# 模块名称：mod_cross.R
# 功能描述：杂交计划模块 - 管理杂交组合的创建与计划
# 创建日期：2025-12-29
# =============================================================================

library(RSQLite)
library(DBI)
library(dplyr)
library(glue)

# -----------------------------------------------------------------------------
# 核心功能：创建杂交计划
# -----------------------------------------------------------------------------

#' 创建杂交计划
#'
#' @description
#' 根据目标批次名、母本列表和父本列表生成杂交计划。
#' 自动检查数据库中是否已存在相同组合，避免重复创建。
#' 
#' @param batch_name 字符串，批次名称（如 "2025春季批次"）
#' @param mothers 字符向量，母本名称列表或母本ID列表
#' @param fathers 字符向量，父本名称列表或父本ID列表
#' @param db_path 字符串，数据库路径，默认 "data/db/soy_cross.db"
#' @param include_reciprocal 逻辑值，是否自动添加反交，默认 TRUE
#' @param status 字符串，初始状态，默认 "planned"
#' @param use_id 逻辑值，mothers/fathers是否为ID（TRUE）还是名称（FALSE），默认FALSE
#'
#' @return 列表，包含以下元素：
#'   \item{summary}{数据框，计划摘要统计}
#'   \item{new_crosses}{数据框，新创建的杂交组合}
#'   \item{skipped}{数据框，跳过的已存在组合}
#'   \item{db_updated}{逻辑值，数据库是否已更新}
#'
#' @export
#'
#' @examples
#' # 使用亲本名称
#' result <- create_cross_plan(
#'   batch_name = "2025春季",
#'   mothers = c("中黄301", "南农66"),
#'   fathers = c("天辰6号", "油6019")
#' )
#' 
#' # 使用亲本ID
#' result <- create_cross_plan(
#'   batch_name = "2025春季",
#'   mothers = c("P0001", "P0002"),
#'   fathers = c("P0101", "P0102"),
#'   use_id = TRUE
#' )
create_cross_plan <- function(
    batch_name,
    mothers,
    fathers,
    db_path = "data/db/soy_cross.db",
    include_reciprocal = TRUE,
    status = "planned",
    use_id = FALSE
) {
  
  # === 参数验证 ===
  if (missing(batch_name) || is.null(batch_name) || !nzchar(batch_name)) {
    stop("❌ 参数错误：batch_name 不能为空")
  }
  if (missing(mothers) || length(mothers) == 0) {
    stop("❌ 参数错误：mothers 列表不能为空")
  }
  if (missing(fathers) || length(fathers) == 0) {
    stop("❌ 参数错误：fathers 列表不能为空")
  }
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  # === 连接数据库 ===
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # === 验证表是否存在 ===
  tables <- dbListTables(con)
  if (!"crosses" %in% tables) {
    stop("❌ 数据库中不存在 crosses 表")
  }
  if (!"parents" %in% tables) {
    stop("❌ 数据库中不存在 parents 表")
  }
  
  # === 转换名称为ID（如果需要）===
  if (!use_id) {
    mother_ids <- get_parent_ids_from_names(con, mothers)
    father_ids <- get_parent_ids_from_names(con, fathers)
  } else {
    mother_ids <- mothers
    father_ids <- fathers
  }
  
  # === 生成所有候选组合对 ===
  message("📋 正在生成候选组合...")
  candidates <- expand.grid(
    female_id = mother_ids,
    male_id = father_ids,
    stringsAsFactors = FALSE
  )
  
  # 过滤掉自交（母本=父本）
  candidates <- candidates %>%
    filter(female_id != male_id)
  
  total_candidates <- nrow(candidates)
  message(glue("   共生成 {total_candidates} 个候选组合对（已排除自交）"))
  
  # === 检查已存在的组合 ===
  message("🔍 正在检查数据库中已存在的组合...")
  existing_crosses <- check_existing_crosses(con, candidates)
  
  # 区分：新组合 vs 已存在
  new_crosses <- candidates %>%
    anti_join(existing_crosses, by = c("female_id", "male_id"))
  
  skipped <- candidates %>%
    semi_join(existing_crosses, by = c("female_id", "male_id"))
  
  n_new <- nrow(new_crosses)
  n_skip <- nrow(skipped)
  
  message(glue("   ✅ 新组合：{n_new} 个"))
  message(glue("   ⏭️  已存在：{n_skip} 个（跳过）"))
  
  # === 准备插入数据 ===
  db_updated <- FALSE
  inserted_data <- NULL
  
  if (n_new > 0) {
    message("💾 正在插入新组合到数据库...")
    
    # 构建插入数据
    to_insert <- new_crosses %>%
      mutate(
        id = generate_cross_id(female_id, male_id),
        batch = batch_name,
        name = paste0(female_id, "-", male_id),
        seed_count = NA_integer_,
        is_reciprocal = 0L,
        status = status,
        created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ) %>%
      select(id, female_id, male_id, batch, name, seed_count, 
             is_reciprocal, status, created_at, updated_at)
    
    # 插入数据库
    tryCatch({
      dbBegin(con)
      dbAppendTable(con, "crosses", to_insert)
      dbCommit(con)
      db_updated <- TRUE
      inserted_data <- to_insert
      message(glue("   ✅ 成功插入 {n_new} 条正交记录"))
    }, error = function(e) {
      dbRollback(con)
      stop("❌ 插入失败：", e$message)
    })
    
    # === 处理反交 ===
    if (include_reciprocal && n_new > 0) {
      message("🔄 正在添加反交组合...")
      
      reciprocal_data <- new_crosses %>%
        mutate(
          id = generate_cross_id(male_id, female_id),
          batch = batch_name,
          name = paste0(male_id, "-", female_id),
          seed_count = NA_integer_,
          is_reciprocal = 1L,
          status = status,
          created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ) %>%
        rename(female_id = male_id, male_id = female_id) %>%
        select(id, female_id, male_id, batch, name, seed_count, 
               is_reciprocal, status, created_at, updated_at)
      
      # 检查反交是否已存在
      existing_reciprocal <- check_existing_crosses(
        con, 
        reciprocal_data %>% select(female_id, male_id)
      )
      
      reciprocal_to_insert <- reciprocal_data %>%
        anti_join(existing_reciprocal, by = c("female_id", "male_id"))
      
      n_recip <- nrow(reciprocal_to_insert)
      
      if (n_recip > 0) {
        tryCatch({
          dbBegin(con)
          dbAppendTable(con, "crosses", reciprocal_to_insert)
          dbCommit(con)
          message(glue("   ✅ 成功插入 {n_recip} 条反交记录"))
          
          # 合并到插入数据
          inserted_data <- bind_rows(inserted_data, reciprocal_to_insert)
        }, error = function(e) {
          dbRollback(con)
          warning("⚠️  反交插入失败：", e$message)
        })
      } else {
        message("   ⏭️  反交组合已全部存在，跳过")
      }
    }
  } else {
    message("ℹ️  所有组合均已存在，无需插入")
  }
  
  # === 生成摘要 ===
  summary_df <- data.frame(
    batch_name = batch_name,
    total_candidates = total_candidates,
    new_crosses = n_new,
    skipped_existing = n_skip,
    reciprocal_added = if(include_reciprocal) nrow(inserted_data) - n_new else 0L,
    db_updated = db_updated,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  
  # === 返回结果 ===
  result <- list(
    summary = summary_df,
    new_crosses = if(!is.null(inserted_data)) inserted_data else data.frame(),
    skipped = if(n_skip > 0) skipped else data.frame(),
    db_updated = db_updated
  )
  
  message("\n" , paste(rep("=", 60), collapse = ""))
  message("📊 计划摘要：")
  message(glue("   批次名称：{batch_name}"))
  message(glue("   候选组合：{total_candidates} 个"))
  message(glue("   新增正交：{n_new} 个"))
  message(glue("   跳过已存在：{n_skip} 个"))
  if (include_reciprocal) {
    message(glue("   新增反交：{summary_df$reciprocal_added} 个"))
  }
  message(glue("   数据库已更新：{ifelse(db_updated, '是', '否')}"))
  message(paste(rep("=", 60), collapse = ""), "\n")
  
  return(result)
}


# -----------------------------------------------------------------------------
# 辅助函数
# -----------------------------------------------------------------------------

#' 根据亲本名称获取ID
#' @keywords internal
get_parent_ids_from_names <- function(con, parent_names) {
  if (length(parent_names) == 0) {
    return(character(0))
  }
  
  # 构建查询
  placeholders <- paste(rep("?", length(parent_names)), collapse = ", ")
  sql <- glue("SELECT id, name FROM parents WHERE name IN ({placeholders})")
  
  result <- dbGetQuery(con, sql, params = as.list(parent_names))
  
  # 检查是否所有名称都找到了
  found_names <- result$name
  missing_names <- setdiff(parent_names, found_names)
  
  if (length(missing_names) > 0) {
    warning(glue("⚠️  以下亲本名称在数据库中不存在：{paste(missing_names, collapse = ', ')}"))
  }
  
  return(result$id)
}

#' 检查已存在的杂交组合
#' @keywords internal
check_existing_crosses <- function(con, candidates) {
  if (nrow(candidates) == 0) {
    return(data.frame(female_id = character(), male_id = character()))
  }
  
  # 构建临时表进行批量查询
  temp_table <- candidates
  dbWriteTable(con, "temp_candidates", temp_table, overwrite = TRUE, temporary = TRUE)
  
  sql <- "
    SELECT DISTINCT c.female_id, c.male_id
    FROM crosses c
    INNER JOIN temp_candidates t
      ON c.female_id = t.female_id AND c.male_id = t.male_id
  "
  
  existing <- dbGetQuery(con, sql)
  
  # 清理临时表
  dbRemoveTable(con, "temp_candidates")
  
  return(existing)
}

#' 生成杂交组合ID
#' @keywords internal
generate_cross_id <- function(female_id, male_id) {
  paste0(female_id, "_", male_id)
}


# -----------------------------------------------------------------------------
# 查询函数
# -----------------------------------------------------------------------------

#' 查询批次统计信息
#'
#' @description
#' 统计数据库中各批次的杂交组合数量
#'
#' @param db_path 字符串，数据库路径
#' @param batch_filter 可选，字符向量，仅统计指定批次
#'
#' @return 数据框，包含各批次的统计信息
#' @export
summarize_cross_batches_db <- function(
    db_path = "data/db/soy_cross.db",
    batch_filter = NULL
) {
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  sql <- "
    SELECT 
      batch,
      COUNT(*) as total_crosses,
      SUM(CASE WHEN is_reciprocal = 0 THEN 1 ELSE 0 END) as direct_crosses,
      SUM(CASE WHEN is_reciprocal = 1 THEN 1 ELSE 0 END) as reciprocal_crosses,
      COUNT(DISTINCT status) as status_count,
      GROUP_CONCAT(DISTINCT status) as statuses
    FROM crosses
  "
  
  if (!is.null(batch_filter) && length(batch_filter) > 0) {
    placeholders <- paste(rep("?", length(batch_filter)), collapse = ", ")
    sql <- paste0(sql, " WHERE batch IN (", placeholders, ")")
    result <- dbGetQuery(con, sql, params = as.list(batch_filter))
  } else {
    sql <- paste0(sql, " GROUP BY batch ORDER BY batch DESC")
    result <- dbGetQuery(con, sql)
  }
  
  return(result)
}


#' 检查特定组合是否存在
#'
#' @description
#' 检查数据库中是否存在指定的母本-父本组合
#'
#' @param female 字符串，母本名称或ID
#' @param male 字符串，父本名称或ID
#' @param db_path 字符串，数据库路径
#' @param use_id 逻辑值，是否使用ID查询（TRUE）还是名称（FALSE）
#'
#' @return 逻辑值，TRUE表示存在，FALSE表示不存在
#' @export
has_cross <- function(
    female,
    male,
    db_path = "data/db/soy_cross.db",
    use_id = FALSE
) {
  if (!file.exists(db_path)) {
    stop("❌ 数据库文件不存在：", db_path)
  }
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  
  if (!use_id) {
    # 先转换名称为ID
    female_id <- get_parent_ids_from_names(con, female)
    male_id <- get_parent_ids_from_names(con, male)
    
    if (length(female_id) == 0 || length(male_id) == 0) {
      return(FALSE)
    }
  } else {
    female_id <- female
    male_id <- male
  }
  
  sql <- "SELECT COUNT(*) as n FROM crosses WHERE female_id = ? AND male_id = ?"
  result <- dbGetQuery(con, sql, params = list(female_id, male_id))
  
  return(result$n > 0)
}

#' 创建固定数量的杂交计划（精确 N 条）
#'
#' @description
#' 在给定候选母本/父本集合的前提下，生成不重复的组合，并仅插入前 N 条。
#' 默认不添加反交，以确保总数精确为 N；如需反交，可设 include_reciprocal = TRUE（此时总插入数 > N）。
#'
#' @param batch_name 批次名
#' @param mothers 母本名称或ID向量
#' @param fathers 父本名称或ID向量
#' @param n 整数，期望插入的组合数量（正交）
#' @param db_path 数据库路径，默认 "data/db/soy_cross.db"
#' @param include_reciprocal 是否为所选组合添加反交，默认 FALSE
#' @param status 初始状态，默认 "planned"
#' @param use_id TRUE 则 mothers/fathers 为 ID；FALSE 则为名称并自动转换
#' @param strategy 选择策略："balanced" 或 "sequential"
#'
#' @return 列表：summary/new_crosses/skipped/db_updated
#' @export
create_cross_plan_n <- function(
    batch_name,
    mothers,
    fathers,
    n,
    db_path = "data/db/soy_cross.db",
    include_reciprocal = FALSE,
    status = "planned",
    use_id = FALSE,
    strategy = c("balanced", "sequential")
) {
  strategy <- match.arg(strategy)
  if (missing(batch_name) || !nzchar(batch_name)) stop("❌ 参数错误：batch_name 不能为空")
  if (missing(mothers) || length(mothers) == 0) stop("❌ 参数错误：mothers 列表不能为空")
  if (missing(fathers) || length(fathers) == 0) stop("❌ 参数错误：fathers 列表不能为空")
  if (missing(n) || !is.numeric(n) || n <= 0) stop("❌ 参数错误：n 必须为正整数")
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  tables <- dbListTables(con)
  if (!all(c("crosses","parents") %in% tables)) stop("❌ 数据库缺少 crosses 或 parents 表")

  if (!use_id) {
    mother_ids <- get_parent_ids_from_names(con, mothers)
    father_ids <- get_parent_ids_from_names(con, fathers)
  } else {
    mother_ids <- mothers
    father_ids <- fathers
  }
  candidates <- expand.grid(female_id = mother_ids, male_id = father_ids, stringsAsFactors = FALSE) %>%
    dplyr::filter(female_id != male_id)

  existing <- check_existing_crosses(con, candidates)
  new_candidates <- candidates %>% anti_join(existing, by = c("female_id","male_id"))
  if (nrow(new_candidates) == 0) {
    message("ℹ️ 可用新组合为 0，未插入")
    return(list(summary = data.frame(batch_name, requested_n = n, inserted_n = 0, skipped_existing = nrow(candidates), db_updated = FALSE), new_crosses = data.frame(), skipped = candidates, db_updated = FALSE))
  }

  selected <- {
    if (strategy == "balanced") new_candidates %>% arrange(female_id, male_id) %>% slice_head(n = min(n, nrow(new_candidates)))
    else new_candidates %>% slice_head(n = min(n, nrow(new_candidates)))
  }

  to_insert <- selected %>% mutate(
    id = generate_cross_id(female_id, male_id),
    batch = batch_name,
    name = paste0(female_id, "-", male_id),
    seed_count = NA_integer_,
    is_reciprocal = 0L,
    status = status,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ) %>% select(id, female_id, male_id, batch, name, seed_count, is_reciprocal, status, created_at, updated_at)

  db_updated <- FALSE
  inserted_data <- NULL
  tryCatch({
    dbBegin(con)
    dbAppendTable(con, "crosses", to_insert)
    dbCommit(con)
    db_updated <- TRUE
    inserted_data <- to_insert
    message(glue::glue("✅ 已插入 {nrow(to_insert)} 条正交记录"))
  }, error = function(e) {
    dbRollback(con)
    stop("❌ 插入失败：", e$message)
  })

  recip_count <- 0L
  if (include_reciprocal && nrow(selected) > 0) {
    reciprocal_data <- selected %>% mutate(
      id = generate_cross_id(male_id, female_id),
      batch = batch_name,
      name = paste0(male_id, "-", female_id),
      seed_count = NA_integer_,
      is_reciprocal = 1L,
      status = status,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ) %>% rename(female_id = male_id, male_id = female_id) %>%
      select(id, female_id, male_id, batch, name, seed_count, is_reciprocal, status, created_at, updated_at)

    existing_recip <- check_existing_crosses(con, reciprocal_data %>% select(female_id, male_id))
    reciprocal_to_insert <- reciprocal_data %>% anti_join(existing_recip, by = c("female_id","male_id"))
    recip_count <- nrow(reciprocal_to_insert)
    if (recip_count > 0) {
      tryCatch({
        dbBegin(con)
        dbAppendTable(con, "crosses", reciprocal_to_insert)
        dbCommit(con)
        inserted_data <- dplyr::bind_rows(inserted_data, reciprocal_to_insert)
        message(glue::glue("🔄 已插入 {recip_count} 条反交记录"))
      }, error = function(e) {
        dbRollback(con)
        warning("⚠️  反交插入失败：", e$message)
      })
    }
  }

  skipped <- candidates %>% semi_join(existing, by = c("female_id","male_id"))
  summary_df <- data.frame(
    batch_name = batch_name,
    requested_n = n,
    inserted_n = nrow(to_insert),
    reciprocal_added = recip_count,
    total_candidates = nrow(candidates),
    available_new = nrow(new_candidates),
    skipped_existing = nrow(skipped),
    db_updated = db_updated,
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  list(summary = summary_df, new_crosses = inserted_data %||% data.frame(), skipped = skipped, db_updated = db_updated)
}
