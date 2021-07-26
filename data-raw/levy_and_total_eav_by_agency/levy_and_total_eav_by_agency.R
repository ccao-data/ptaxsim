library(dplyr)
library(snakecase)
library(purrr)
library(stringr)
library(readxl)
library(readr)
library(data.table)

# Mini function to get the mode of a vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# The levy of each jurisdiction is reported by the Cook County Clerk's Office.
# URL here: https://www.cookcountyclerkil.gov/service/tax-extension-and-rates

# Using the provided agency rate reports (from 2014 onward) for the sake of
# simplicity. These files also contain the total Cook County Equalized
# Assessed Value by agency

# Get a list of all levy report spreadsheets
file_names <- list.files(
  path = "data-raw/levy_and_total_eav_by_agency//",
  pattern = "*.xls*",
  full.names = TRUE
)

# List numeric column names from spreadsheets
file_num_cols <- c(
  "cpi", "lim_numerator", "lim_denominator", "lim_rate", "reduction_percent"
)

# Load each file and cleanup columns, then combine into single df
levy_and_total_eav_by_agency <- map_dfr(file_names, function(file) {
  readxl::read_xlsx(file) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    mutate(
      across(
        c(contains("_total"), contains("_eav"), any_of(file_num_cols)),
        as.numeric
      ),
      reduction_type = as.character(reduction_type)
    ) %>%
    rename_with(~ str_remove(.x, "tax_"), any_of("tax_year")) %>%
    rename_with(~ str_remove(.x, "_18"), any_of(ends_with("_18")))
})

# Clean up resulting combined data frame
levy_and_total_eav_by_agency <- levy_and_total_eav_by_agency %>%
  select(
    year, agency, agency_name, home_rule_ind,
    total_eav = cook_eav, total_levy = grand_total_ext
  ) %>%
  mutate(
    agency_name = str_trim(str_squish(agency_name)),
    year = as.integer(year),
    home_rule_ind = ifelse(home_rule_ind == "Y", TRUE, FALSE),
    home_rule_ind = replace_na(home_rule_ind, FALSE)
  ) %>%
  group_by(agency) %>%
  # Replace agency name with modal name (some are misspelled)
  mutate(agency_name = Mode(agency_name)) %>%
  ungroup()

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(levy_and_total_eav_by_agency)
setkey(levy_and_total_eav_by_agency, year, agency)

# Save each data set to package
usethis::use_data(levy_and_total_eav_by_agency, overwrite = TRUE)
