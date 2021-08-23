library(dplyr)
library(tidyr)
library(tabulizer)
library(miniUI)
library(stringr)
library(purrr)
library(data.table)

# The goal of this script is to create a data frame of the taxing district
# level tax amounts for each sample tax bill in data-raw/
# To do this, we first scan the PDFs with tabulizer, match the output tables
# with the output from tax_bill(), then clean up

# Get a list of all PDFs in sample_tax_bills/
list_pdf_inputs <- list.files(
  path = "data-raw/sample_tax_bills/",
  pattern = "*.pdf",
  full.names = TRUE
)

row_to_names <- function(df) {
  names(df) <- as.character(unlist(df[1, ]))
  df[-1, ]
}


# Different tax bills can have different table sizes depending on the number of
# taxing district. As such, the table bottom boundary will be different for each
# bill. Here we manually specify the area of table using an interactive widget
extract_tax_bill <- function(file) {
  base_file <- basename(file)

  # Scan table into memory
  tbl <- extract_areas(file = file, pages = 1)[[1]] %>%
    as_tibble() %>%
    row_to_names() %>%
    set_names(
      c("agency_name", "tax", "rate", "percent", "pension", "prev_tax")
    )

  # Create a list with metadata for output
  out <- list(
    year = str_sub(base_file, 1, 4),
    pin = as.character(str_sub(base_file, 10, 23)),
    class = as.character(str_sub(base_file, 6, 8)),
    tbl = tbl
  )

  return(out)
}

# Collect all scanned tables + meta data in a data frame
bills <- map(list_pdf_inputs, extract_tax_bill)
bills_df <- bind_rows(bills)

# Clean up scanned table
bills_df <- bills_df %>%
  do.call(data.frame, .) %>%
  rename_at(vars(starts_with("tbl.")), ~ str_remove(.x, "tbl.")) %>%
  as_tibble() %>%
  mutate(tax = na_if(tax, "")) %>%
  filter(!is.na(tax)) %>%
  filter(!stringr::str_detect(agency_name, " Total")) %>%
  mutate(across(c(year, tax:prev_tax), readr::parse_number))

# Load agency name lookup from file
agency_match <- readr::read_csv(
  "data-raw/sample_tax_bills/agency_name_match.csv"
)

# Join agency ID numbers to bills table
bills_df <- bills_df %>%
  left_join(agency_match, by = "agency_name") %>%
  relocate(agency, .before = "agency_name")

# Consolidate Cook County breakouts into single line-item
bills_df <- bills_df %>%
  mutate(cook = str_detect(
    agency_name,
    "Cook County Public Safety|Cook County Health Facilities|County of Cook"
  )) %>%
  group_by(pin, year, cook) %>%
  mutate(across(tax:prev_tax, ~ ifelse(cook, sum(.x), .x))) %>%
  ungroup() %>%
  select(-cook)

# Separate TIF amounts into their own column
bills_df <- bills_df %>%
  group_by(pin, year) %>%
  mutate(tif_total = sum(tax * str_detect(agency_name, "TIF"))) %>%
  filter(!str_detect(agency_name, "TIF")) %>%
  ungroup() %>%
  filter(!is.na(agency))

# Write detail results to file for safekeeping
bills_df %>%
  readr::write_csv("data-raw/sample_tax_bills/sample_tax_bills_detail.csv")

# Load data from file
sample_tax_bills_detail <- readr::read_csv(
  "data-raw/sample_tax_bills/sample_tax_bills_detail.csv"
)

# Convert columns to expected type
sample_tax_bills_detail <- sample_tax_bills_detail %>%
  mutate(
    year = as.integer(year),
    class = str_pad(class, 3, "left", "0"),
    pension = replace_na(pension, 0),
    agency = str_pad(agency, 9, "left", "0")
  )

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(sample_tax_bills_detail)
setkey(sample_tax_bills_detail, year, pin)

# Write data to R package
usethis::use_data(sample_tax_bills_detail, overwrite = TRUE)
