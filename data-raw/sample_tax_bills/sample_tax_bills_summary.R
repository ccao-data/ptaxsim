library(dplyr)
library(stringr)
library(readr)
library(data.table)

# Load sample tax bills summary data from file.
#
# We maintain this file by hand, and we need to update it whenever
# we add new tax bills to `data-raw/sample_tax_bills/` each year. In the
# future, it might be nice to script a process for extracting these summaries
# automatically
sample_tax_bills_summary <- readr::read_csv(
  "data-raw/sample_tax_bills/sample_tax_bills_summary.csv",
  col_types = cols(pin = "c", tax_code = "c", class = "c")
) %>%
  mutate(
    pin = str_pad(pin, 14, "left", "0"),
    year = as.integer(year),
    eav = as.integer(eav),
    township_name = snakecase::to_title_case(township_name)
  )

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(sample_tax_bills_summary)
setkey(sample_tax_bills_summary, year, pin)

# Load summary data into R package
usethis::use_data(sample_tax_bills_summary, overwrite = TRUE)
