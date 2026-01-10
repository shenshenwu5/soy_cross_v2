library(RSQLite)
library(DBI)

db_path <- "data/db/soy_cross.db"
con <- dbConnect(SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

fields <- dbListFields(con, "crosses")
if (!"memo" %in% fields) {
  dbExecute(con, "ALTER TABLE crosses ADD COLUMN memo TEXT")
  message("added memo column")
} else {
  message("memo column exists")
}
