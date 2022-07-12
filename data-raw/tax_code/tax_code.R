library(arrow)
library(dplyr)
library(purrr)
library(readxl)
library(snakecase)
library(stringr)

# Tax code/agency rates available from the Clerk's Office.
# URL here: https://www.cookcountyclerkil.gov/service/tax-extension-and-rates

# This script combines the Tax Code Agency Rate Reports from 2006 onward

remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path <- file.path(remote_bucket, "tax_code")

# Get a list of all agency rate report spreadsheets
file_names <- list.files(
  path = "data-raw/tax_code",
  pattern = "*.xls*",
  full.names = TRUE
)

# Load each file and cleanup columns, then combine into single df
tax_code <- map_dfr(file_names, function(file) {

  # Extract year from file name
  year_ext <- str_extract(file, "\\d{4}")

  # Load file based on extension
  if (tools::file_ext(file) == "xls") {
    df <- readxl::read_xls(file)
  } else if (tools::file_ext(file) == "xlsx") {
    df <- readxl::read_xlsx(file)
  }

  df %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    select(-contains("year")) %>%
    rename_with(
      ~ str_replace(.x, "taxcode", "tax_code"),
      starts_with("taxcode")
    ) %>%
    mutate(
      year = as.character(year_ext),
      agency_rate = as.numeric(agency_rate),
      tax_code_rate = as.numeric(tax_code_rate)
    )
})

# Clean up resulting combined data frame
tax_code <- tax_code %>%
  select(
    year,
    agency_num = agency, agency_rate,
    tax_code_num = tax_code, tax_code_rate
  ) %>%
  arrange(year, agency_num, tax_code_num)

arrow::write_dataset(
  dataset = tax_code,
  path = remote_path,
  format = "parquet",
  partitioning = "year",
  hive_style = TRUE,
  compression = "zstd"
)
