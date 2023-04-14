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


# Set the database version. This gets incremented manually whenever the database
# changes. This is checked against Config/Requires_DB_Version in the DESCRIPTION
# file via check_db_version(). Schema is:
# "MAX_YEAR_OF_DATA.BREAKING_CHANGE.NONBREAKING_CHANGE"
db_version <- "2021.0.1"

# Set the package version required to use this database. This is checked against
# Version in the DESCRIPTION file. Basically, we have a two-way check that both
# the package version and DB version are synced. Schema is SemVer.
requires_pkg_version <- "0.5.4"




# Create tables ----------------------------------------------------------------

# Create the table definitions from file
db_send_queries(conn, sql_from_file("data-raw/create_db.sql"))




# Populate metadata ------------------------------------------------------------

# Pull info directly from description file
desc <- readr::read_file("DESCRIPTION")

desc_version_pkg <- desc %>%
  str_extract("(?<=Version: )[0-9]*\\.[0-9]*\\.[0-9]*")

desc_first_name <- desc %>%
  str_extract("(?<=given \\= \").*(?=\", family)")
desc_last_name <- desc %>%
  str_extract("(?<=family \\= \").*(?=\", email)")
desc_name <- paste(desc_first_name, desc_last_name)

desc_email <- desc %>%
  str_extract("(?<=email \\= \").*gov")

desc_url_package <- desc %>%
  str_extract("(?<=URL: ).*(?=,)")

db_base_url <- "https://ccao-data-public-us-east-1.s3.amazonaws.com/ptaxsim/"
db_full_url <- paste0(db_base_url, "ptaxsim-", db_version, ".db.bz2")

# Load agency files to get min and max year
agency_df <- read_parquet(file.path(remote_bucket, "agency", "part-0.parquet"))
min_year <- min(as.integer(agency_df$year))
max_year <- max(as.integer(agency_df$year))

# Check max data year against DB version year
db_version_year <- as.integer(substr(db_version, 1, 4))
if (db_version_year != max_year) {
  stop(
    "Database version year is not equal to max year in the data. Check ",
    "data and version number synchonization!"
  )
}

# Create tibble of metadata table and add to DB
metadata_df <- tibble(
  db_version = db_version,
  requires_pkg_version = requires_pkg_version,
  created_with_pkg_version = desc_version_pkg,
  created_at = as.character(format(Sys.time(), tz = "UTC")),
  created_by = Sys.info()[["user"]],
  author_name = desc_name,
  author_email = desc_email,
  source_url_database = db_full_url,
  source_url_package = desc_url_package,
  data_year_min = min_year,
  data_year_max = max_year
)

DBI::dbAppendTable(conn, "metadata", metadata_df)




# Load data --------------------------------------------------------------------

# Load tables contained in a single file
files <- c(
  "agency", "agency_info", "agency_fund", "agency_fund_info",
  "cpi", "eq_factor", "tif", "tif_crosswalk", "tif_distribution"
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

# Compress with bzip2, since SQLite db files are uncompressed
system(paste("bzip2", "-7", db_path))

# The final compressed DB file is manually uploaded to S3
