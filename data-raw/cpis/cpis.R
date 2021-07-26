library(dplyr)
library(readr)
library(tidyr)
library(tabulizer)
library(miniUI)
library(stringr)
library(data.table)

# The goal of this script is to create a data frame of Consumer Price Indices
# CPI-U used by PTELL to calculate/cap property tax extensions
# We can load the historical CPIs from a PDF provided by the State of Illinois

row_to_names <- function(df) {
  names(df) <- as.character(unlist(df[1, ]))
  df[-1, ]
}

# Extract the table only (no headers), then manually assign header
cpis_ext <- extract_areas(file = "data-raw/cpis/cpihistory.pdf")[[1]]
cpis <- as_tibble(cpis_ext[, c(1, 2, 4, 5, 6)])
cpis <- setNames(cpis, c("year", "cpi", "ptell_cook", "comments", "levy_year"))

# Merge Cook rate into main column
cpis <- cpis %>%
  mutate(
    across(c(year, levy_year), as.integer),
    across(c(cpi), as.numeric),
    across(c(ptell_cook, comments), readr::parse_number),
    ptell_cook = ifelse(!is.na(comments), comments, ptell_cook),
    ptell_cook = ptell_cook / 100
  ) %>%
  select(-comments) %>%
  filter(year != 1991)

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(cpis)
setkey(cpis, levy_year)

# Write data to R package
usethis::use_data(cpis, overwrite = TRUE)
