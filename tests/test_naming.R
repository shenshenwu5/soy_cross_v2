
library(testthat)
library(RSQLite)
library(DBI)
library(dplyr)

# 设置项目根目录
project_root <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2"
source(file.path(project_root, "R/mod_cross.R"))

test_that("generate_cross_codes works correctly", {
  df <- data.frame(
    female_id = c("F1", "F2"),
    male_id = c("M1", "M2"),
    stringsAsFactors = FALSE
  )
  
  res <- generate_cross_codes(df, prefix = "ZH25", start_num = 1, digits = 3)
  
  expect_equal(nrow(res), 2)
  expect_equal(res$name[1], "ZH25001")
  expect_equal(res$name[2], "ZH25002")
  expect_equal(res$pa[1], "F1")
  expect_equal(res$ma[1], "M1")
})

test_that("integration with database", {
  # 创建临时数据库
  db_path <- tempfile(fileext = ".db")
  con <- dbConnect(SQLite(), db_path)
  
  # 创建表
  dbExecute(con, "CREATE TABLE parents (id TEXT, name TEXT, active INTEGER)")
  dbExecute(con, "INSERT INTO parents VALUES ('P1', 'Parent1', 1)")
  dbExecute(con, "INSERT INTO parents VALUES ('P2', 'Parent2', 1)")
  
  dbExecute(con, "CREATE TABLE crosses (
    id TEXT, female_id TEXT, male_id TEXT, batch TEXT, 
    name TEXT, seed_count INTEGER, is_reciprocal INTEGER, 
    status TEXT, created_at TEXT, updated_at TEXT
  )")
  
  dbDisconnect(con)
  
  # 1. 创建计划
  pairs <- data.frame(female_id = "P1", male_id = "P2", stringsAsFactors = FALSE)
  res <- create_specific_cross_plan(
    batch_name = "Batch1",
    pairs = pairs,
    db_path = db_path,
    include_reciprocal = FALSE
  )
  
  expect_equal(nrow(res$new_crosses), 1)
  expect_equal(res$new_crosses$name[1], "P1-P2") # 默认名称
  
  # 2. 生成代号
  coded_df <- generate_cross_codes(res$new_crosses, prefix = "CODE", start_num = 10)
  expect_equal(coded_df$name[1], "CODE010")
  
  # 3. 更新数据库
  updated_count <- update_cross_names_from_df(
    coded_df,
    batch = "Batch1",
    db_path = db_path,
    is_id = TRUE
  )
  
  expect_equal(updated_count, 1)
  
  # 验证数据库内容
  con <- dbConnect(SQLite(), db_path)
  new_name <- dbGetQuery(con, "SELECT name FROM crosses WHERE female_id='P1' AND male_id='P2'")$name
  dbDisconnect(con)
  
  expect_equal(new_name, "CODE010")
  
  unlink(db_path)
})
