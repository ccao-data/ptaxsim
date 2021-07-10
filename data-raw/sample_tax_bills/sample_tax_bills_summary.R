library(dplyr)
library(stringr)
library(readr)

# Load sample tax bills summary data from file
sample_tax_bills_summary <- readr::read_csv(
  "data-raw/sample_tax_bills/sample_tax_bills_summary.csv",
  col_types = cols(pin = "c", tax_code = "c", class = "c")
) %>%
  mutate(pin = str_pad(pin, 14, "left", "0"))

# Load summary data into R package
usethis::use_data(sample_tax_bills_summary, overwrite = TRUE)
