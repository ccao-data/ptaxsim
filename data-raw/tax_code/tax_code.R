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
  print(file)
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
    rename_with(~ str_remove(.x, "_24"), ends_with("_24")) %>%
    rename_with(~ rep("tax_code", length(.x)), any_of(c(
      "taxcode", "code"
    ))) %>%
    rename_with(~ rep("agency_name", length(.x)), any_of(c(
      "authority_name"
    ))) %>%
    rename_with(~ rep("agency_rate", length(.x)), any_of(c(
      "ag_rate", "auth_rate"
    ))) %>%
    rename_with(~ rep("tax_code_rate", length(.x)), any_of(c(
      "code_rate", "taxcode_rate"
    ))) %>%
    mutate(
      year = as.character(year_ext),
      agency_rate = as.numeric(agency_rate),
      tax_code_rate = as.numeric(tax_code_rate)
    )
})

# Clean up resulting combined data frame
tax_code <- tax_code %>%
  filter(!grepl("TIF", agency_name)) %>%
  select(
    year,
    agency_name,
    agency_num = agency, agency_rate,
    tax_code_num = tax_code, tax_code_rate
  ) %>%
  arrange(year, agency_num, tax_code_num) %>%
  distinct()


arrow::write_dataset(
  dataset = tax_code,
  path = remote_path,
  format = "parquet",
  partitioning = "year",
  hive_style = TRUE,
  compression = "zstd"
)
