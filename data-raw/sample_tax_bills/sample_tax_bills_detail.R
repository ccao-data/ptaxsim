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

# Get a list of all PDFs in the sample_tax_bills directory.
# We curate this set of files manually. When adding a new tax year, you should
# download a new set of sample tax bills for that year from the Treasurer's tax
# bill portal, with an eye toward decent coverage of classes, geographic
# areas, and exemptions. For an idea of what decent coverage looks like, refer
# to prior years of sample bills
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
        prev_tax
      ),
      pension = if_else(flag,
        NA,
        pension
      )
    ) %>%
    select(-flag) %>%
    filter(
      agency_name != "",
      !str_detect(
        agency_name,
        paste0(
          "TAXES|Assess|Property|EAV|Local Tax|",
          "Total Tax|Do not|Equalizer|cookcountyclerk.com|",
          "Pursuant|meaning of|If paying later|\\d{15}+|By \\d{2}/|",
          "Visit COOKCOUNTYCLERKIL.GOV"
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

# Load agency name lookup from file. This lookup maps agency names to numbers.
#
# We maintain this file by hand. When adding a new year of sample bills, you
# may encounter agencies that are not yet present in this list. To
# add those agencies, perform the join to `bills_df` in the code block below
# and then filter the resulting `bills_df` dataframe for agencies with nulls
# for `agency_num`. Then, use the Clerk's agency rate report and TIF report
# for that year to find the agency number for each missing agency.
#
# The `agency_name_match.csv` file has the following schema:
#
#   1. agency_name: The name of the agency, exactly matching its representation
#      on the tax bill. If the same agency has different name representations
#      on tax bills across years, usually because the Clerk and Treasurer have
#      changed the format of the name for that agency, then you should add a
#      duplicate row for the agency with the new name but with identical values
#      for the rest of the fields.
#        - If an agency fund or transit TIF distribution has its own line item
#          on the tax bill, you should create a new row for that line item in
#          this crosswalk, but make sure to give it a lower priority in the
#          `num_priority` column (see the docs for that column below). That way
#          the code below will ensure we roll up those fund values into the
#          overall values for the agency.
#
#   2. agency_num: The Clerk's unique identifier for the agency. You can find
#      this value in the Clerk's agency rate report and/or TIF report.
#
#   3. name_priority: Integer priority for this name, in descending priority
#      order (e.g. 1 is higher priority than 2). We only use this value when
#      rolling up funds and transit TIF distributions into their parent agency.
#      In these cases, it's important that the parent agency have priority 1.
#      All other agencies should have priorities other than 1, but it doesn't
#      matter what those priorities are relative to each other, since we only
#      use the `name_priority` field to determine which agency is the parent.
agency_match <- readr::read_csv(
  "data-raw/sample_tax_bills/agency_name_match.csv"
)

# Join agency ID numbers to bills table
bills_df <- bills_df %>%
  # Agency name join key should be case insensitive, since case can change
  # across years
  mutate(agency_name_lower = str_to_lower(agency_name)) %>%
  left_join(
    agency_match %>%
      mutate(agency_name_lower = str_to_lower(agency_name)) %>%
      select(-agency_name),
    by = "agency_name_lower"
  ) %>%
  select(-agency_name_lower) %>%
  relocate(agency_num, .before = "agency_name")

# Consolidate funds and TIF breakouts into a single line-item for the parent
# agency
bills_df <- bills_df %>%
  group_by(pin, year, agency_num) %>%
  mutate(across(final_tax:prev_tax, sum)) %>%
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
