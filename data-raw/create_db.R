# Setup ------------------------------------------------------------------------
library(arrow)
library(DBI)
library(dplyr)
library(purrr)
library(RSQLite)
library(stringr)

# Combine the cleaned data files created by each data-raw/ script into a single
# SQLite DB. This DB is the main backend of PTAXSIM and is downloaded from S3.
# It can be cached by the user for repeated use
db_path <- tempfile(fileext = ".db")
conn <- dbConnect(RSQLite::SQLite(), db_path)

# Pull the cleaned data from S3
remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")

# https://stackoverflow.com/questions/18914283/how-to-execute-more
# -than-one-rsqlite-statement-at-once-or-how-to-dump-a-whole-fi
sql_from_file <- function(file) {
  sql <- readLines(file)
  sql <- unlist(stringr::str_split(paste(sql, collapse = " "), ";"))
  sql <- sql[grep("^ *$", sql, invert = TRUE)]
  sql
}


db_send_queries <- function(conn, sql) {
  dummyfunction <- function(sql, conn) {
    DBI::dbExecute(conn, sql)
  }
  purrr::walk(sql, dummyfunction, conn)
}




# Create tables ----------------------------------------------------------------

# Create the table definitions from file
db_send_queries(conn, sql_from_file("data-raw/create_db.sql"))




# Load data --------------------------------------------------------------------

# Load tables contained in a single file
files <- c(
  "agency", "agency_info", "agency_fund", "agency_fund_info",
  "cpi", "eq_factor", "tif", "tif_distribution"
)
for (file in files) {
  message("Now loading: ", file)
  df <- read_parquet(file.path(remote_bucket, file, "part-0.parquet"))
  DBI::dbAppendTable(conn, file, df)
}

# Load tables spread over multiple files
datasets <- c("pin", "tax_code")
for (dataset in datasets) {
  message("Now loading: ", dataset)
  df <- collect(arrow::open_dataset(file.path(remote_bucket, dataset)))
  DBI::dbAppendTable(conn, dataset, df)
}




# Clean up ---------------------------------------------------------------------

# Vacuum to save space and compress a bit
dbExecute(conn, "VACUUM;")
dbDisconnect(conn)

# Compress with zstd, since SQLite db files are uncompressed
system(paste("zstd", "-7", db_path))

# The final compressed DB file is manually uploaded to S3
