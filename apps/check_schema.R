library(DBI)
library(RSQLite)
db_path <- "../data/db/soy_cross.db"
if (!file.exists(db_path)) {
  cat("DB not found")
} else {
  con <- dbConnect(SQLite(), db_path)
  cat(paste(dbListFields(con, "parents"), collapse = "\n"))
  dbDisconnect(con)
}
