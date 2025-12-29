suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(tibble)
})

.sc_get_con <- function(con = NULL) {
  if (!is.null(con)) return(con)
  if (exists("SoyCross", envir = .GlobalEnv) && exists("db_connect", envir = get("SoyCross", .GlobalEnv))) {
    return(get("SoyCross", .GlobalEnv)$db_connect())
  }
  dbConnect(RSQLite::SQLite(), dbname = "data/db/soy_cross.db")
}

field_plan_create <- function(batch, layout_opts = list(), con = NULL) {
  own_con <- is.null(con)
  con <- .sc_get_con(con)
  on.exit(if (own_con) DBI::dbDisconnect(con), add = TRUE)
  qbatch <- dbQuoteString(con, batch)
  sql <- paste0("SELECT id AS cross_id, female_id, male_id, name, batch FROM crosses WHERE batch = ", qbatch, " ORDER BY id")
  base <- dbGetQuery(con, sql)
  if (!nrow(base)) return(tibble(cross_id=character(), female_id=character(), male_id=character(), name=character(), plot_code=character(), replicate=integer(), bed_no=integer(), row_no=integer(), col_no=integer(), planting_date=as.Date(character())))
  beds <- if (!is.null(layout_opts$beds)) layout_opts$beds else 10
  rows_per_bed <- if (!is.null(layout_opts$rows_per_bed)) layout_opts$rows_per_bed else 20
  cols_per_row <- if (!is.null(layout_opts$cols_per_row)) layout_opts$cols_per_row else 1
  replicates <- if (!is.null(layout_opts$replicates)) layout_opts$replicates else 1
  start_seq <- if (!is.null(layout_opts$start_seq)) layout_opts$start_seq else 1
  planting_date <- if (!is.null(layout_opts$planting_date)) layout_opts$planting_date else NA
  rep_idx <- rep(seq_len(nrow(base)), each = replicates)
  df <- base[rep_idx, , drop = FALSE]
  df$replicate <- rep(seq_len(replicates), times = nrow(base))
  n <- nrow(df)
  seq_all <- seq.int(from = start_seq, length.out = n)
  capacity <- rows_per_bed * cols_per_row
  bed_no <- ((seq_all - 1) %/% capacity) + 1
  idx_in_bed <- ((seq_all - 1) %% capacity) + 1
  row_no <- ((idx_in_bed - 1) %/% cols_per_row) + 1
  col_no <- ((idx_in_bed - 1) %% cols_per_row) + 1
  plot_code <- paste0(df$batch, "-", seq_all)
  out <- tibble(
    cross_id = df$cross_id,
    female_id = df$female_id,
    male_id = df$male_id,
    name = df$name,
    plot_code = plot_code,
    replicate = as.integer(df$replicate),
    bed_no = as.integer(bed_no),
    row_no = as.integer(row_no),
    col_no = as.integer(col_no),
    planting_date = as.Date(planting_date)
  )
  out
}

field_book_export <- function(plan_tbl, out_path) {
  ext <- tolower(tools::file_ext(out_path))
  if (file.exists(out_path) && ext %in% c("xlsx","rds","csv")) stop("export target exists")
  layout_tbl <- plan_tbl
  comb_tbl <- plan_tbl %>% distinct(cross_id, female_id, male_id, name, batch = sub("-.*$", "", plot_code))
  labels_tbl <- field_labels(plan_tbl)
  if (ext == "xlsx" && requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "组合表"); openxlsx::writeData(wb, "组合表", comb_tbl)
    openxlsx::addWorksheet(wb, "布局表"); openxlsx::writeData(wb, "布局表", layout_tbl)
    openxlsx::addWorksheet(wb, "标签表"); openxlsx::writeData(wb, "标签表", labels_tbl)
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
    return(invisible(out_path))
  }
  if (ext == "rds") {
    saveRDS(list(comb = comb_tbl, layout = layout_tbl, labels = labels_tbl), out_path)
    return(invisible(out_path))
  }
  if (ext == "csv") {
    base <- sub("\\.csv$", "", out_path)
    write.csv(comb_tbl, paste0(base, "_comb.csv"), row.names = FALSE)
    write.csv(layout_tbl, paste0(base, "_layout.csv"), row.names = FALSE)
    write.csv(labels_tbl, paste0(base, "_labels.csv"), row.names = FALSE)
    return(invisible(out_path))
  }
  stop("unsupported export format")
}

field_import_book <- function(in_path) {
  ext <- tolower(tools::file_ext(in_path))
  if (ext == "xlsx" && requireNamespace("openxlsx", quietly = TRUE)) {
    sheets <- openxlsx::getSheetNames(in_path)
    nm <- if ("布局表" %in% sheets) "布局表" else sheets[1]
    df <- openxlsx::read.xlsx(in_path, sheet = nm)
    df <- as_tibble(df)
    return(df)
  }
  if (ext == "rds") {
    obj <- readRDS(in_path)
    if (is.list(obj) && !is.null(obj$layout)) return(as_tibble(obj$layout))
    return(as_tibble(obj))
  }
  if (ext == "csv") {
    df <- read.csv(in_path, stringsAsFactors = FALSE)
    return(as_tibble(df))
  }
  stop("unsupported import format")
}

field_update_status <- function(plan_tbl, status = "planted", con = NULL) {
  own_con <- is.null(con)
  con <- .sc_get_con(con)
  on.exit(if (own_con) DBI::dbDisconnect(con), add = TRUE)
  ids <- unique(plan_tbl$cross_id)
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  has_pd <- "planting_date" %in% colnames(plan_tbl) && ("planting_date" %in% dbListFields(con, "crosses"))
  if (has_pd) {
    sql <- "UPDATE crosses SET status = ?, updated_at = ?, planting_date = ? WHERE id = ?"
    DBI::dbWithTransaction(con, {
      for (i in seq_along(ids)) {
        pd <- plan_tbl$planting_date[match(ids[i], plan_tbl$cross_id)]
        DBI::dbExecute(con, sql, params = list(status, ts, as.character(pd), ids[i]))
      }
    })
  } else {
    sql <- "UPDATE crosses SET status = ?, updated_at = ? WHERE id = ?"
    DBI::dbWithTransaction(con, {
      for (i in seq_along(ids)) {
        DBI::dbExecute(con, sql, params = list(status, ts, ids[i]))
      }
    })
  }
  invisible(length(ids))
}

field_labels <- function(plan_tbl) {
  out <- plan_tbl %>%
    select(plot_code, name, female_id, male_id) %>%
    mutate(batch = sub("-.*$", "", plot_code)) %>%
    distinct()
  out
}