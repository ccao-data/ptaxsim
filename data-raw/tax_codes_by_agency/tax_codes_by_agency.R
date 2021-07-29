library(dplyr)
library(snakecase)
library(purrr)
library(stringr)
library(readxl)
library(data.table)

# Tax code/agency rates available from the Clerk's Office.
# URL here: https://www.cookcountyclerkil.gov/service/tax-extension-and-rates

# This script combines the Tax Code Agency Rate Reports from 2014 onward

# Get a list of all agency rate report spreadsheets
file_names <- list.files(
  path = "data-raw/tax_codes_by_agency/",
  pattern = "*.xls*",
  full.names = TRUE
)

# Load each file and cleanup columns, then combine into single df
tax_codes_by_agency <- map_dfr(file_names, function(file) {

  # Extract year from file name
  year <- str_extract(file, "\\d{4}")

  # Load file based on extension
  if (tools::file_ext(file) == "xls") {
    df <- readxl::read_xls(file)
  } else if (tools::file_ext(file) == "xlsx") {
    df <- readxl::read_xlsx(file)
  }

  df %>%
    mutate(year = year) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    select(-contains("year")) %>%
    mutate(year = year) %>%
    rename_with(~ str_replace(.x, "taxcode", "tax_code"), starts_with("taxcode"))
})

# Clean up resulting combined data frame
tax_codes_by_agency <- tax_codes_by_agency %>%
  mutate(agency_name = str_trim(str_squish(agency_name))) %>%
  relocate(year) %>%
  arrange(year, tax_code) %>%
  mutate(year = as.integer(year)) %>%
  select(year, tax_code, agency)

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(tax_codes_by_agency)
setkey(tax_codes_by_agency, year, tax_code, agency)

# Save data to package
usethis::use_data(tax_codes_by_agency, overwrite = TRUE)
