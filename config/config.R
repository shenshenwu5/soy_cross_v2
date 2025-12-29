suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

SoyCross <- new.env(parent = emptyenv())

SoyCross$config <- list(
  paths = list(
    db_path = "data/db/soy_cross.db",
    backup_dir = "data/db",
    output_dir = "output",
    logs_dir = "logs"
  ),
  sqlite = list(
    busy_timeout_ms = 10000,
    enable_foreign_keys = TRUE
  ),
  cross = list(
    id_rule = "{female_id}_{male_id}",
    name_rule = "{female_id}-{male_id}",
    unique_keys = c("female_id", "male_id", "batch")
  ),
  field = list(
    plot_code_pattern = "{batch}-{seq}",
    default_layout = list(
      beds = 10,
      rows_per_bed = 20,
      replicates = 1,
      start_seq = 1
    ),
    label_include = c("plot_code", "name", "female_id", "male_id", "batch")
  ),
  analysis = list(
    top_n_parents = 20
  ),
  io = list(
    default_export_format = "xlsx",
    overwrite = FALSE
  ),
  safety = list(
    auto_backup = TRUE
  )
)

SoyCross$db_connect <- function() {
  db <- dbConnect(RSQLite::SQLite(), dbname = SoyCross$config$paths$db_path)
  dbExecute(db, sprintf("PRAGMA busy_timeout = %d", SoyCross$config$sqlite$busy_timeout_ms))
  if (isTRUE(SoyCross$config$sqlite$enable_foreign_keys)) dbExecute(db, "PRAGMA foreign_keys = ON")
  db
}

SoyCross$backup_db <- function() {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  dest <- file.path(SoyCross$config$paths$backup_dir, paste0("soy_cross_backup_", ts, ".db"))
  dir.create(SoyCross$config$paths$backup_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(SoyCross$config$paths$db_path, dest, overwrite = TRUE)
  dest
}

SoyCross$sanitize_table_columns <- function(con, tbl) {
  if (!(tbl %in% dbListTables(con))) return(invisible(FALSE))
  fields <- dbListFields(con, tbl)
  to_fix <- fields[grepl("\\s", fields)]
  if (!length(to_fix)) return(invisible(FALSE))
  dbWithTransaction(con, {
    for (old in to_fix) {
      new <- gsub("\\s+", "", old)
      if (new != old && !(new %in% fields)) {
        sql <- paste0('ALTER TABLE "', tbl, '" RENAME COLUMN "', old, '" TO "', new, '"')
        dbExecute(con, sql)
      }
    }
  })
  TRUE
}

SoyCross$get_config <- function() SoyCross$config