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

#' 批量更新杂交组合名称
#'
#' @description
#' 根据输入的数据框批量更新 crosses 表中的 name 字段。
#' 输入的 pa (母本) 和 ma (父本) 可以是名称或 ID，函数会自动识别并转换。
#' 匹配规则：batch + female_id + male_id。
#'
#' @param data 数据框，必须包含 ma (父本), pa (母本), name (新名称) 列
#' @param batch 字符串，目标批次名称
#' @param db_path 数据库路径
#' @param is_id 逻辑值，指定 ma/pa 是否已经是 ID。默认 FALSE（即视为名称）。
#'
#' @return 更新记录数的整数
#' @export
update_cross_names_from_df <- function(
    data,
    batch,
    db_path = "data/db/soy_cross.db",
    is_id = FALSE
) {
  if (missing(data) || !is.data.frame(data)) stop("❌ 参数错误：data 必须是一个数据框")
  if (missing(batch) || !nzchar(batch)) stop("❌ 参数错误：batch 不能为空")
  required_cols <- c("ma", "pa", "name")
  if (!all(required_cols %in% names(data))) {
    stop("❌ 数据框必须包含列：", paste(required_cols, collapse = ", "))
  }
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  # 确保列为字符型，避免因子带来的问题
  data$ma <- as.character(data$ma)
  data$pa <- as.character(data$pa)
  data$name <- as.character(data$name)
  
  # 如果输入的是名称，需要转换为 ID
  if (!is_id) {
    message("ℹ️ 正在将亲本名称转换为 ID...")
    
    # 获取 parents 表的所有 id 和 name 映射
    parents_map <- dbGetQuery(con, "SELECT id, name FROM parents")
    
    # 转换 pa (母本)
    data <- data %>%
      left_join(parents_map, by = c("pa" = "name")) %>%
      rename(female_id = id)
      
    # 转换 ma (父本)
    # 此时 parents_map 中的 id 列在 join 后会默认变为 id，但为了保险起见，明确指定后缀
    # 或者，由于前一次 join 已经使用了 id 并 rename 成了 female_id，所以当前 data 中没有 id 列
    # join 后新加入的 id 列即为 ma 的 id
    data <- data %>%
      left_join(parents_map, by = c("ma" = "name"))
    
    # 此时列名中应该包含 id（来自第二次 join）
    if ("id" %in% names(data)) {
        data <- data %>% rename(male_id = id)
    } else {
        # 防御性编程：如果 dplyr 行为变化，可能是 id.y
        if ("id.y" %in% names(data)) {
             data <- data %>% rename(male_id = id.y)
        } else {
             # 极端情况，尝试按位置或打印列名调试，这里先假设为 id
             stop("❌ 无法识别父本ID列，当前列名：", paste(names(data), collapse=", "))
        }
    }
      
    # 检查是否有未找到 ID 的亲本
    missing_female <- is.na(data$female_id)
    missing_male <- is.na(data$male_id)
    
    if (any(missing_female | missing_male)) {
      n_miss <- sum(missing_female | missing_male)
      warning(glue("⚠️ 有 {n_miss} 条记录无法找到对应的亲本ID，将被跳过"))
      data <- data %>% filter(!is.na(female_id) & !is.na(male_id))
    }
  } else {
    data <- data %>%
      rename(female_id = pa, male_id = ma)
  }
  
  if (nrow(data) == 0) {
    message("ℹ️ 无有效数据可更新")
    return(0)
  }
  
  # 准备更新参数列表
  # SQL: UPDATE crosses SET name = ?, updated_at = ? WHERE batch = ? AND female_id = ? AND male_id = ?
  current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  params_list <- list()
  
  # 遍历数据框构建参数
  # 包含正交和反交的更新
  idx <- 1
  for (i in seq_len(nrow(data))) {
    # 正交：输入数据中的 pa=female_id, ma=male_id
    params_list[[idx]] <- list(
      data$name[i],
      current_time,
      batch,
      data$female_id[i], 
      data$male_id[i]
    )
    idx <- idx + 1
    
    # 反交：自动处理
    # 规则：pa=male_id, ma=female_id
    # 名称：将正交名称中的 "F0" 替换为 "RF0"
    recip_name <- sub("F0", "RF0", data$name[i])
    if (recip_name == data$name[i]) {
       # 如果名称中没有 F0，尝试在开头加 R
       recip_name <- paste0("R", data$name[i])
    }
    
    params_list[[idx]] <- list(
      recip_name,
      current_time,
      batch,
      data$male_id[i],   # 反交的母本是正交的父本
      data$female_id[i]  # 反交的父本是正交的母本
    )
    idx <- idx + 1
  }
  
  # 使用事务批量执行
  sql <- "UPDATE crosses SET name = ?, updated_at = ? WHERE batch = ? AND female_id = ? AND male_id = ?"
  
  total_updated <- 0
  dbBegin(con)
  tryCatch({
    for (params in params_list) {
      res <- dbExecute(con, sql, params = params)
      total_updated <- total_updated + res
    }
    dbCommit(con)
    message(glue("✅ 成功更新 {total_updated} 条记录的名称"))
  }, error = function(e) {
    dbRollback(con)
    stop("❌ 批量更新失败：", e$message)
  })
  
  return(total_updated)
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
      GROUP_CONCAT(DISTINCT status) as statuses,
      MAX(updated_at) as last_updated
    FROM crosses
  "
  
  if (!is.null(batch_filter) && length(batch_filter) > 0) {
    placeholders <- paste(rep("?", length(batch_filter)), collapse = ", ")
    sql <- paste0(sql, " WHERE batch IN (", placeholders, ") GROUP BY batch ORDER BY last_updated DESC")
    result <- dbGetQuery(con, sql, params = as.list(batch_filter))
  } else {
    sql <- paste0(sql, " GROUP BY batch ORDER BY last_updated DESC")
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

clear_cross_batches_db <- function(
    batch_names,
    preview = TRUE,
    db_path = "data/db/soy_cross.db",
    ask = TRUE
) {
  if (missing(batch_names) || length(batch_names) == 0) stop("❌ 参数错误：batch_names 不能为空")
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  result <- data.frame(batch = character(0), n = integer(0), last_updated = character(0), first_created = character(0), stringsAsFactors = FALSE)
  for (b in batch_names) {
    info <- dbGetQuery(con, "SELECT COUNT(*) AS n, MAX(updated_at) AS last_updated, MIN(created_at) AS first_created FROM crosses WHERE batch = ?", params = list(b))
    result <- rbind(result, data.frame(
      batch = b,
      n = as.integer(info$n[1]),
      last_updated = as.character(info$last_updated[1]),
      first_created = as.character(info$first_created[1]),
      stringsAsFactors = FALSE
    ))
  }
  suppressWarnings({ ts <- as.POSIXct(result$last_updated, tz = "UTC") })
  ord <- order(ts, decreasing = TRUE, na.last = TRUE)
  result <- result[ord, , drop = FALSE]
  if (preview) return(result)
  total <- sum(result$n)
  if (total == 0) {
    message("ℹ️ 指定批次无可删除记录")
    return(data.frame(batch = result$batch, deleted = integer(length(result$batch)), stringsAsFactors = FALSE))
  }
  if (ask && interactive()) {
    message("⚠️ 即将删除以下批次（按更新时间降序）：")
    print(result)
    resp <- readline(prompt = "确认删除? 输入 yes 或 y 继续：")
    if (!tolower(trimws(resp)) %in% c("y", "yes")) {
      message("✅ 已取消删除")
      return(result)
    }
  }
  dbBegin(con)
  ok <- TRUE
  tryCatch({
    for (b in result$batch) {
      dbExecute(con, "DELETE FROM crosses WHERE batch = ?", params = list(b))
    }
    dbCommit(con)
  }, error = function(e) {
    ok <<- FALSE
    dbRollback(con)
    stop("❌ 批次删除失败：", e$message)
  })
  if (ok) {
    return(data.frame(batch = result$batch, deleted = result$n, stringsAsFactors = FALSE))
  }
}

list_cross_batches_db <- function(
    db_path = "data/db/soy_cross.db"
) {
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  res <- dbGetQuery(con, "SELECT DISTINCT batch FROM crosses WHERE batch IS NOT NULL ORDER BY batch ASC")
  return(res)
}

get_crosses_by_batch <- function(
    batch,
    db_path = "data/db/soy_cross.db",
    include_reciprocal = TRUE
) {
  if (missing(batch) || !nzchar(batch)) stop("❌ 参数错误：batch 不能为空")
  if (!file.exists(db_path)) stop("❌ 数据库文件不存在：", db_path)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  
  sql <- "SELECT * FROM crosses WHERE batch = ?"
  params <- list(batch)
  
  if (!include_reciprocal) {
    sql <- paste0(sql, " AND is_reciprocal = 0")
  }
  
  sql <- paste0(sql, " ORDER BY updated_at DESC")
  result <- dbGetQuery(con, sql, params = params)
  return(result)
}

#' 关联亲本信息到杂交组合
#'
#' @description
#' 将杂交组合数据框与 parents 表关联，获取母本和父本的详细信息。
#' 亲本字段会自动添加 "female_" 和 "male_" 前缀。
#'
#' @param crosses_data 数据框，必须包含 female_id 和 male_id 列
#' @param db_path 数据库路径
#' @param fields 字符向量，指定要关联的亲本字段（不含id）。如果为 NULL，则关联所有字段。
#'
# -----------------------------------------------------------------------------
# Shiny 模块：杂交组合配置
# -----------------------------------------------------------------------------

#' 杂交组合配置 UI
#' @export
cross_config_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      # 步骤 1 & 2：亲本选择（左右分栏）
      column(4,
        h4("1. 选择母本"),
        DT::dataTableOutput(ns("tbl_females"))
      ),
      column(4,
        h4("2. 选择父本"),
        DT::dataTableOutput(ns("tbl_males"))
      ),
      # 步骤 3 & 4：配置与执行
      column(4,
        h4("3. 配置参数"),
        textInput(ns("input_batch"), "批次名称", value = format(Sys.Date(), "%Y春季")),
        checkboxInput(ns("check_reciprocal"), "自动生成反交", TRUE),
        hr(),
        h4("4. 确认与生成"),
        numericInput(ns("input_limit"), "限制数量 (可选)", value = NA, min = 1),
        uiOutput(ns("ui_summary")),
        actionButton(ns("btn_run"), "🚀 生成杂交计划", class = "btn-primary btn-lg btn-block")
      )
    )
  )
}

#' 杂交组合配置 Server
#' @export
cross_config_server <- function(id, db_path = "data/db/soy_cross.db") {
  # 尝试加载分析模块以支持冲突检测
  if (file.exists("R/mod_analysis.R")) source("R/mod_analysis.R")

  moduleServer(id, function(input, output, session) {

    # 1. 数据加载 (获取所有字段以支持特征显示)
    parents <- reactive({
      con <- dbConnect(SQLite(), db_path)
      on.exit(dbDisconnect(con))
      # 获取所有列，后续根据列名动态展示
      dbGetQuery(con, "SELECT * FROM parents WHERE active=1")
    })

    # 辅助函数：获取展示列
    get_display_cols <- function(df) {
      base_cols <- c("id", "name")
      # 尝试查找特征列
      extra_cols <- intersect(names(df), c("traits", "description", "features", "remarks"))
      if (length(extra_cols) > 0) base_cols <- c(base_cols, extra_cols)
      base_cols
    }

    # 2. 渲染母本表格
    output$tbl_females <- DT::renderDataTable({
      df <- parents()
      cols <- get_display_cols(df)
      DT::datatable(df[, cols, drop = FALSE], selection = "multiple", options = list(pageLength = 10))
    })

    # 3. 准备父本数据 (含冲突检测)
    males_data_reactive <- reactive({
      df <- parents()
      
      # 获取选中的母本
      selected_rows <- input$tbl_females_rows_selected
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        df$is_conflict <- FALSE
        return(df)
      }

      # 获取选中母本的名称 (find_unused_partners 需要名称)
      # 注意：DT 的行号对应 parents() 的行号
      selected_mothers <- df$name[selected_rows]
      
      # 冲突检测逻辑：
      # 我们要高亮那些【已经】与选中母本配过组的父本。
      # find_unused_partners 返回【未】配组的。
      # 所以：冲突 = 所有父本 - 交集(每个母本的未配组对象)
      # 或者更直接：对于每个选中的母本，找出其已配组对象，取并集。
      
      # 为了严格遵循 "调用 mod_analysis::find_unused_partners" 的要求：
      if (exists("find_unused_partners")) {
        # 计算每个母本的未配组父本 ID
        unused_list <- lapply(selected_mothers, function(m_name) {
          tryCatch({
             # role="female" 表示 m_name 是母本，我们要找未配的父本
             find_unused_partners(m_name, role = "female", db_path = db_path)$id
          }, error = function(e) {
             # 如果出错（如找不到亲本），假设没有未配组的（即全部冲突）或全部可用？
             # 安全起见，假设全部可用，避免误报冲突
             df$id 
          })
        })
        
        # 取交集：只有在所有选中母本中都“未配组”的父本，才是真正的“无冲突”
        # 只要与任一选中母本配过组，即视为冲突（高亮提示）
        # Wait. 
        # Case 1: Select M1. Used with P1. Unused with P2. -> P1 Conflict.
        # Case 2: Select M1, M2. 
        # M1 used with P1. M2 used with P2.
        # If I select P1: (M1, P1) is repeat. (M2, P1) is new. -> Conflict? Yes, partial conflict.
        # If I select P2: (M1, P2) is new. (M2, P2) is repeat. -> Conflict? Yes.
        # So, if a father is used by ANY of the selected mothers, it should be highlighted.
        # Used_by_Any = Union(Used_by_M1, Used_by_M2...)
        # Used_by_M = All - Unused_by_M
        # So Used_by_Any = Union( (All - Unused_M1), (All - Unused_M2) )
        # = All - Intersection(Unused_M1, Unused_M2...)
        
        common_unused_ids <- Reduce(intersect, unused_list)
        df$is_conflict <- ! (df$id %in% common_unused_ids)
        
      } else {
        # Fallback if function not found
        df$is_conflict <- FALSE
      }
      
      df
    })

    # 4. 渲染父本表格 (显示 name 列和 is_conflict 列)
    output$tbl_males <- DT::renderDataTable({
      df <- males_data_reactive()
      cols <- get_display_cols(df)
      
      has_conflict <- "is_conflict" %in% names(df)
      
      # 如果有冲突列，将其加入到显示列中
      if (has_conflict && !("is_conflict" %in% cols)) {
        cols <- c(cols, "is_conflict")
      }
      
      # 只显示指定的列（包括 name 和 is_conflict）
      dt <- DT::datatable(
        df[, cols, drop = FALSE],
        selection = "multiple", 
        options = list(pageLength = 10)
      )
      
      # 应用样式：根据 is_conflict 列改变行背景颜色
      if (has_conflict) {
        dt <- dt %>% DT::formatStyle(
          'id', valueColumns = 'is_conflict', 
          target = 'row',
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#ffeeba', 'white')), 
          title = DT::styleEqual(c(TRUE, FALSE), c('该父本已与选中的某位母本配过组', ''))
        )
      }
      
      dt
    })

    # 3. 实时摘要
    output$ui_summary <- renderUI({
      n_f <- length(input$tbl_females_rows_selected)
      n_m <- length(input$tbl_males_rows_selected)
      n_total <- n_f * n_m
      if (input$check_reciprocal) n_total <- n_total * 2

      tagList(
        p(glue::glue("已选母本: {n_f}")),
        p(glue::glue("已选父本: {n_m}")),
        p(glue::glue("预计组合: {n_total}"), style = "font-weight: bold; color: blue;")
      )
    })

    # 4. 执行逻辑
    observeEvent(input$btn_run, {
      req(input$input_batch)

      # 获取选中行的 ID
      p_data <- parents()
      f_ids <- p_data$id[input$tbl_females_rows_selected]
      m_ids <- p_data$id[input$tbl_males_rows_selected]

      if (length(f_ids) == 0 || length(m_ids) == 0) {
        showNotification("请至少选择一个母本和一个父本", type = "error")
        return()
      }

      withProgress(message = '正在生成计划...', {
        tryCatch({
          if (is.na(input$input_limit)) {
            # 全量生成
            res <- create_cross_plan(
              batch_name = input$input_batch,
              mothers = f_ids,
              fathers = m_ids,
              include_reciprocal = input$check_reciprocal,
              use_id = TRUE,
              db_path = db_path
            )
          } else {
            # 限量生成
            res <- create_cross_plan_n(
              batch_name = input$input_batch,
              mothers = f_ids,
              fathers = m_ids,
              n = input$input_limit,
              include_reciprocal = input$check_reciprocal,
              use_id = TRUE,
              db_path = db_path
            )
          }

          showNotification(glue::glue("成功！新增 {res$summary$new_crosses} 个组合"), type = "message")

        }, error = function(e) {
          showNotification(paste("错误:", e$message), type = "error")
        })
      })
    })
  })
}

