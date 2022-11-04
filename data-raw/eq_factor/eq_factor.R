library(arrow)
library(dplyr)
library(readr)

remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path <- file.path(remote_bucket, "eq_factor", "part-0.parquet")

eq_factor <- readr::read_csv(
  file = "data-raw/eq_factor/eq_factor.csv",
  col_types = cols(
    year = col_character(),
    tentative_eq_factor = col_double(),
    final_eq_factor = col_double()
  )
) %>%
  arrange(year)

# Write to S3
arrow::write_parquet(
  x = eq_factor,
  sink = remote_path,
  compression = "zstd"
)
