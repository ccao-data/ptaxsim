library(dplyr)
library(tidyr)
library(snakecase)
library(purrr)
library(stringr)
library(readxl)
library(data.table)

# TIF information is maintained by Cook County Clerk's Office.
# URL here: https://www.cookcountyclerkil.gov/service/tif-reports

# This script combines the TIF distribution summary reports from 2014 onward
# with topline TIF summaries (which show cancellation and start date)

# Get a list of all TIF distribution report spreadsheets
dist_file_names <- list.files(
  path = "data-raw/tifs/",
  pattern = "*Distribution*",
  full.names = TRUE
)

# Load each file and cleanup columns, then combine into single df
tifs <- map_dfr(dist_file_names, function(file) {

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
    rename_with(~ str_remove(.x, "_\\d{4}"), contains(year)) %>%
    mutate(
      across(
        c(starts_with("tax_code_"), starts_with("tif_total_")),
        as.numeric
      )
    ) %>%
    rename_with(~ str_remove(.x, "_tif"), starts_with("tax_code_tif_"))
})

# Get a list of all TIF summary report spreadsheets
summ_file_names <- list.files(
  path = "data-raw/tifs/",
  pattern = "*Cook*",
  full.names = TRUE
)

# Load each file and cleanup columns, then combine into single df
tifs_summary <- map_dfr(summ_file_names, function(file) {

  # Extract year from file name
  year <- as.numeric(str_extract(file, "\\d{4}"))

  # Load file based on extension
  if (tools::file_ext(file) == "xls") {
    df <- readxl::read_xls(file)
  } else if (tools::file_ext(file) == "xlsx") {
    df <- readxl::read_xlsx(file)
  }

  df %>%
    mutate(year = year) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    rename_with(~ str_replace(.x, str_c(year, "_"), "this_year_")) %>%
    rename_with(~ str_replace(.x, str_c(year - 1, "_"), "prev_year_"))
})

# Clean up summary file and keep only columns of interest
tifs_summary <- tifs_summary %>%
  mutate(
    tif_cancelled_this_year = str_detect(new_cancelled, "Cancelled"),
    tif_new_this_year = str_detect(new_cancelled, "New"),
  ) %>%
  mutate(
    across(c(tif_cancelled_this_year, tif_new_this_year), ~ replace_na(.x, FALSE)),
    across(c(this_year_revenue, prev_year_revenue), ~ replace_na(.x, 0)),
  ) %>%
  select(
    year, agency, first_year, this_year_revenue, prev_year_revenue,
    tif_new_this_year, tif_cancelled_this_year
  )

# Combine TIF distribution and summary data and clean up
tifs <- tifs %>%
  rename(agency = tif_agency, tax_code = tif_tax_code) %>%
  mutate(
    tif_name = str_trim(str_squish(tif_name)),
    year = as.integer(year)
  ) %>%
  relocate(c(year, tax_code)) %>%
  left_join(tifs_summary, by = c("year", "agency")) %>%
  # Some TIFs are missing agency/summary data for some years
  group_by(year, tif_name) %>%
  fill(first_year:tif_cancelled_this_year, .direction = "downup") %>%
  ungroup() %>%
  mutate(across(c(year, first_year), as.integer))

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(tifs)
setkey(tifs, year, tax_code, agency)

# Save data to package
usethis::use_data(tifs, overwrite = TRUE)
