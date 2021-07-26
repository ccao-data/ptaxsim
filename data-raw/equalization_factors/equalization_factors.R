library(readr)
library(data.table)

equalization_factors <- readr::read_csv(
  file = "data-raw/equalization_factors/equalization_factors.csv",
  col_types = cols(year = col_integer(), equalization_factor = col_double())
)

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(equalization_factors)
setkey(equalization_factors, year)

usethis::use_data(equalization_factors, overwrite = TRUE)
