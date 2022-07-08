library(arrow)
library(dplyr)
library(readr)

remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path <- file.path(remote_bucket, "eq_factors", "part-0.parquet")

eq_factors <- readr::read_csv(
  file = "data-raw/eq_factors/eq_factors.csv",
  col_types = cols(year = col_character(), equalization_factor = col_double())
) %>%
  arrange(year)

# Write to S3
arrow::write_parquet(
  x = eq_factors,
  sink = remote_path,
  compression = "zstd"
)
