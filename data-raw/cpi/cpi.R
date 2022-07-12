library(arrow)
library(dplyr)
library(miniUI)
library(tabulizer)
library(tidyr)
library(stringr)

row_to_names <- function(df) {
  names(df) <- as.character(unlist(df[1, ]))
  df[-1, ]
}


# The goal of this script is to create a data frame of Consumer Price Indices
# CPI-U used by PTELL to calculate/cap property tax extensions
# We can load the historical CPIs from a PDF provided by the State of Illinois

# Paths for local raw data storage and remote storage on S3
remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path <- file.path(remote_bucket, "cpi", "part-0.parquet")

# Extract the table only (no headers), then manually assign header
cpi_ext <- extract_areas(file = "data-raw/cpi/cpihistory.pdf")[[1]]
cpi <- as_tibble(cpi_ext[, c(1, 2, 4, 5, 6)])
cpi <- setNames(cpi, c("year", "cpi", "ptell_cook", "comments", "levy_year"))

# Merge Cook rate into main column
cpi <- cpi %>%
  mutate(
    across(c(year, levy_year), as.character),
    across(c(cpi), as.numeric),
    across(c(ptell_cook, comments), readr::parse_number),
    ptell_cook = ifelse(!is.na(comments), comments, ptell_cook),
    ptell_cook = ptell_cook / 100
  ) %>%
  select(-comments) %>%
  filter(year != "1991") %>%
  arrange(year)

# Write to S3
arrow::write_parquet(
  x = cpi,
  sink = remote_path,
  compression = "zstd"
)
