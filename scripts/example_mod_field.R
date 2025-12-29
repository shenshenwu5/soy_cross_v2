suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
})

if (file.exists("config/config.R")) source("config/config.R")
source("E:/FangCloudSync/R_WD360/Project/soy_cross/R/mod_field.R")

con <- if (exists("SoyCross", envir = .GlobalEnv) && exists("db_connect", envir = get("SoyCross", .GlobalEnv))) {
  get("SoyCross", .GlobalEnv)$db_connect()
} else {
  dbConnect(RSQLite::SQLite(), dbname = "data/db/soy_cross.db")
}
on.exit(DBI::dbDisconnect(con), add = TRUE)

batches <- dbGetQuery(con, "SELECT DISTINCT batch FROM crosses WHERE batch IS NOT NULL ORDER BY batch")
if (!nrow(batches)) stop("无可用批次")
batch <- tail(batches$batch, 1)

plan <- field_plan_create(
  batch = batch,
  layout_opts = list(beds = 8, rows_per_bed = 15, cols_per_row = 2, replicates = 1, start_seq = 1, planting_date = Sys.Date())
)
if (!nrow(plan)) stop("该批次无记录")

dir.create("output", showWarnings = FALSE, recursive = TRUE)
out_path <- if (requireNamespace("openxlsx", quietly = TRUE)) {
  file.path("output", paste0(batch, "_field_book.xlsx"))
} else {
  file.path("output", paste0(batch, "_field_book.csv"))
}
field_book_export(plan, out_path)

field_update_status(plan, status = "planted")

labels <- field_labels(plan)
print(head(labels, 10))
