library(arrow)
library(dplyr)
library(miniUI)
library(pdftools)
library(tidyr)
library(stringr)

row_to_names <- function(df) {
  names(df) <- as.character(unlist(df[1, ]))
  df[-1, ]
}


# The goal of this script is to create a data frame of Consumer Price Indices
# CPI-U used by PTELL to calculate/cap property tax extensions
# We can load the historical CPIs from a PDF provided by the State of Illinois
# https://tax.illinois.gov/content/dam/soi/en/web/tax/localgovernments/property/documents/cpihistory.pdf

# Paths for local raw data storage and remote storage on S3
remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path <- file.path(remote_bucket, "cpi", "part-0.parquet")

cpi <- pdftools::pdf_text(pdf = "data-raw/cpi/cpihistory.pdf") %>%
  str_extract(., regex("1991.*", dotall = TRUE)) %>%
  str_remove_all(., "\\(5 % for Cook\\)") %>%
  str_split(., "\n") %>%
  unlist() %>%
  tibble(vals = `.`) %>%
  mutate(vals = str_squish(vals)) %>%
  separate_wider_delim(
    col = vals,
    names = c("year", "cpi", "pct", "ptell_cook", "levy_year", "year_paid"),
    delim = " ", too_few = "align_start", too_many = "drop"
  )

cpi <- cpi %>%
  mutate(
    across(c(year, levy_year), as.character),
    across(c(cpi), as.numeric),
    across(c(ptell_cook), readr::parse_number),
    ptell_cook = ptell_cook / 100
  ) %>%
  filter(year != "1991", year != "", year != "CPI") %>%
  arrange(year)

# Write to S3
arrow::write_parquet(
  x = cpi,
  sink = remote_path,
  compression = "zstd"
)
