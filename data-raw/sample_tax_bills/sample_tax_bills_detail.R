library(dplyr)
library(tidyr)
library(pdftools)
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
  path = "data-raw/sample_tax_bills",
  pattern = "*.pdf",
  full.names = TRUE
)

row_to_names <- function(df) {
  names(df) <- as.character(unlist(df[1, ]))
  df[-1, ]
}


# Different tax bills can have different table sizes depending on the number of
# taxing district.

extract_tax_bill <- function(file) {
  base_file <- basename(file)
  tbl <- pdf_text(file) %>%
    paste(., collapse = "\n") %>%
    str_extract(., regex("MISCELLANEOUS TAXES.*", dotall = TRUE)) %>%
    str_split(., "\n") %>%
    unlist() %>%
    tibble(vals = `.`) %>%
    mutate(vals = str_replace_all(vals, "[:space:]{2,}", "\t")) %>%
    separate_wider_delim(
      col = vals,
      names = c(
        "agency_name", "final_tax", "rate", "percent",
        "pension", "prev_tax"
      ),
      delim = "\t", too_few = "align_start", too_many = "drop"
    ) %>%
    mutate(
      agency_name = str_squish(agency_name),
      flag = is.na(prev_tax),
      prev_tax = if_else(flag,
                         pension,
                         prev_tax),
      pension = if_else(flag,
                        NA,
                        pension)
    ) %>%
    select(-flag) %>%
    filter(
      agency_name != "",
      !str_detect(
        agency_name,
        paste0(
          "TAXES|Assess|Property|EAV|Local Tax|",
          "Total Tax|Do not|Equalizer|cookcountyclerk.com|",
          "Pursuant|meaning of|If paying later|\\d{15}+|By \\d{2}/"
        )
      )
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
  mutate(final_tax = na_if(final_tax, "")) %>%
  filter(!is.na(final_tax)) %>%
  filter(!stringr::str_detect(agency_name, " Total")) %>%
  mutate(across(c(year, final_tax:prev_tax), readr::parse_number))

# Load agency name lookup from file
agency_match <- readr::read_csv(
  "data-raw/sample_tax_bills/agency_name_match.csv"
)

# Join agency ID numbers to bills table
bills_df <- bills_df %>%
  left_join(agency_match, by = "agency_name") %>%
  relocate(agency_num, .before = "agency_name")

# Consolidate Cook County and TIF breakouts into single line-item
bills_df <- bills_df %>%
  mutate(cook = str_detect(
    agency_name,
    "Cook County Public Safety|Cook County Health Facilities|County of Cook"
  )) %>%
  group_by(pin, year, cook) %>%
  mutate(across(final_tax:prev_tax, ~ ifelse(cook, sum(.x), .x))) %>%
  ungroup() %>%
  group_by(pin, year, agency_num) %>%
  mutate(across(final_tax:prev_tax, sum)) %>%
  select(-cook) %>%
  filter(!is.na(agency_num), name_priority == 1) %>%
  select(-name_priority) %>%
  ungroup()

# Round numeric values to nearest hundredth
bills_df <- bills_df %>%
  mutate(
    across(c(final_tax, percent, pension, prev_tax), ~ round(.x, 2)),
    across(c(rate), ~ round(.x, 3)),
  )

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
    agency_num = str_pad(agency_num, 9, "left", "0")
  )

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(sample_tax_bills_detail)
setkey(sample_tax_bills_detail, year, pin)

# Write data to R package
usethis::use_data(sample_tax_bills_detail, overwrite = TRUE)
