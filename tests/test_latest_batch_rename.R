library(testthat)
library(RSQLite)
library(DBI)

project_root <- "e:/FangCloudSync/R_WD360/Project/soy_cross_v2"
source(file.path(project_root, "R/mod_cross.R"))

test_that("only latest batch can be renamed", {
  db_path <- tempfile(fileext = ".db")
  con <- dbConnect(SQLite(), db_path)
  
  dbExecute(con, "CREATE TABLE parents (id TEXT, name TEXT, active INTEGER)")
  dbExecute(con, "INSERT INTO parents VALUES ('P1', 'Parent1', 1)")
  dbExecute(con, "INSERT INTO parents VALUES ('P2', 'Parent2', 1)")
  
  dbExecute(con, "CREATE TABLE crosses (
    id TEXT, female_id TEXT, male_id TEXT, batch TEXT, 
    name TEXT, memo TEXT, seed_count INTEGER, is_reciprocal INTEGER, 
    status TEXT, created_at TEXT, updated_at TEXT
  )")
  
  # Insert two batches: A earlier, B later
  t1 <- "2025-01-01 10:00:00"
  t2 <- "2025-01-02 12:00:00"
  
  dbExecute(con, "INSERT INTO crosses VALUES ('A1','P1','P2','BatchA','',NULL,NULL,0,'planned',?,?)", params = list(t1, t1))
  dbExecute(con, "INSERT INTO crosses VALUES ('B1','P1','P2','BatchB','',NULL,NULL,0,'planned',?,?)", params = list(t2, t2))
  
  dbDisconnect(con)
  
  latest <- get_latest_batch(db_path = db_path)
  expect_equal(latest, "BatchB")
  
  expect_error(update_cross_names(batch = "BatchA", prefix = "X", start_n = 1, digits = 3, db_path = db_path))
  
  res <- update_cross_names(batch = "BatchB", prefix = "X", start_n = 1, digits = 3, db_path = db_path)
  expect_true(is.data.frame(res))
  expect_true(any(res$name %in% c("X001F0", "X001RF0")))
  
  unlink(db_path)
})

