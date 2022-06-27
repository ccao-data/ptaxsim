library(arrow)
library(bit64)
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(snakecase)
library(stringr)
library(tidyr)

calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# The levy of each jurisdiction is reported by the Cook County Clerk's Office.
# URL here: https://www.cookcountyclerkil.gov/service/tax-extension-and-rates

# Using the provided agency rate reports (from 2006 onward) for the sake of
# simplicity. These files also contain the total Cook County Equalized
# Assessed Value by agency

remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path_agencies <- file.path(remote_bucket, "agencies")
remote_path_agencies_detail <- file.path(remote_bucket, "agencies_detail")

# Get a list of all levy report spreadsheets
file_names <- list.files(
  path = "data-raw/agencies//",
  pattern = "*.xls*",
  full.names = TRUE
)

# List numeric column names from spreadsheets
file_num_cols <- c(
  "year", "cpi", "lim_numerator", "lim_denominator",
  "lim_rate", "reduction_percent"
)




# Detail -----------------------------------------------------------------------

# Load the detail sheet from each agency file. This includes the levy and rate
# for each fund
agencies_detail <- map_dfr(file_names, function(file) {
  message("Reading: ", file)
  readxl::read_xlsx(file, sheet = 2) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    mutate(
      across(
        c(contains("_total"), contains("_eav"), any_of(file_num_cols)),
        as.numeric
      ),
      across(
        c(contains("agency"), contains("reduction_type")),
        as.character
      )
    ) %>%
    # Renaming columns since they change names across years
    rename_with(~ str_remove(.x, "tax_"), any_of("tax_year")) %>%
    rename_with(~ str_remove(.x, "_18"), ends_with("_18")) %>%
    rename_with(~ str_remove(.x, "_num"), starts_with("agency")) %>%
    rename_with(~"fund_levy", any_of(c(
      "final_levy", "fund_levy"
    ))) %>%
    rename_with(~"fund_rate", any_of(c(
      "fund_rate", "final_rate", "fund_final_rate", "final_fund_rate"
    ))) %>%
    select(
      year,
      agency_num = agency, fund_num = fund,
      fund_name, fund_levy, fund_rate
    ) %>%
    mutate(across(c(year), as.character))
}) %>%
  mutate(
    agency_num = str_pad(agency_num, 9, "left", "0"),
    fund_num = str_pad(fund_num, 3, "left", "0"),
    fund_rate = ifelse(
      agency_num == "050200000" & und_num == "202" & year == 2006,
      0,
      fund_rate
    )
  ) %>%
  group_by(agency_num, fund_num) %>%
  mutate(fund_name = calc_mode(fund_name)) %>%
  ungroup() %>%
  arrange(year, agency_num, fund_num) %>%
  mutate(
    across(c(fund_levy), as.integer64),
    across(c(fund_rate), as.double)
  )




# Overview ---------------------------------------------------------------------

# Load the overview of each agency file. This includes the agency name, total
# EAV, and final extension
agencies <- map_dfr(file_names, function(file) {
  message("Reading: ", file)
  temp <- readxl::read_xlsx(file) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    mutate(
      across(
        c(contains("_total"), contains("_eav"), any_of(file_num_cols)),
        as.numeric
      ),
      across(
        c(contains("agency"), contains("reduction_type")),
        as.character
      )
    ) %>%
    # Renaming columns since they change names across years
    rename_with(
      ~ str_remove(.x, "tax_"),
      any_of("tax_year")
    ) %>%
    rename_with(
      ~ str_remove(.x, "_18"),
      ends_with("_18")
    ) %>%
    rename_with(
      ~ str_remove(.x, "_num"),
      starts_with("agency")
    ) %>%
    rename_with(
      ~ str_replace(.x, "county", "cook"),
      any_of("county_eav")
    ) %>%
    rename_with(
      ~ str_replace(.x, "final", "grand_total"),
      ends_with("final_ext")
    ) %>%
    rename_with(
      ~ str_replace(.x, "grand_total_final", "total"),
      ends_with("levy")
    ) %>%
    mutate(across(year, as.character)) %>%
    select(
      year,
      agency_num = agency, agency_name, home_rule_ind,
      total_eav = cook_eav, any_of("total_levy"),
      total_ext = grand_total_ext
    )
}) %>%
  mutate(
    agency_num = str_pad(agency_num, 9, "left", "0"),
    agency_name = str_trim(str_squish(agency_name)),
    home_rule_ind = ifelse(home_rule_ind %in% c("Y", "No PTELL"), TRUE, FALSE),
    home_rule_ind = replace_na(home_rule_ind, FALSE),
    # One row is missing a Cook EAV value. Fill manually from prior year
    total_eav = ifelse(agency_num == "030580002" & year == "2006", 0, total_eav)
  ) %>%
  group_by(agency_num) %>%
  # Replace agency name with modal name (some are misspelled)
  mutate(agency_name = calc_mode(agency_name)) %>%
  ungroup() %>%
  arrange(year, agency_num)

# Tax year 2013 is missing the total levy column from its overview sheet, but
# we can fill it in by joining the total from each detail sheet
agencies <- agencies %>%
  left_join(
    agencies_detail %>%
      filter(year == "2013") %>%
      group_by(agency_num) %>%
      summarize(total_levy_fil = sum(fund_levy)),
    by = "agency_num"
  ) %>%
  mutate(total_levy = ifelse(year == 2013, total_levy_fil, total_levy)) %>%
  select(-total_levy_fil) %>%
  mutate(
    across(c(year), as.character),
    across(c(total_eav, total_levy), as.integer64),
    across(c(total_ext), as.double)
  )

# Write both data sets to S3
arrow::write_dataset(
  dataset = agencies,
  path = remote_path_agencies,
  format = "parquet",
  partitioning = "year",
  hive_style = TRUE,
  compression = "zstd"
)
arrow::write_dataset(
  dataset = agencies_detail,
  path = remote_path_agencies_detail,
  format = "parquet",
  partitioning = "year",
  hive_style = TRUE,
  compression = "zstd"
)
