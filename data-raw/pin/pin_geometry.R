library(arrow)
library(aws.s3)
library(DBI)
library(dplyr)
library(geoarrow)
library(glue)
library(noctua)
library(odbc)
library(purrr)
library(readr)
library(sf)
library(stringr)
library(tidyr)

# Paths for remote storage on S3
remote_bucket <- "s3://z-dev-ccao-data-ptaxsim-us-east-1"
remote_path_pin_geometry_raw <- file.path(
  remote_bucket,
  "pin_geometry_raw",
  "part-0.parquet"
)

# Grab the maximum year of agency data so we can limit the scope of the parcel
# file query
agency_df <- read_parquet(file.path(remote_bucket, "agency", "part-0.parquet"))
max_year <- max(as.integer(agency_df$year))

# Grab parcel shapes from S3. These files are originally from the data portal
# and Cook Central. We have to load each parquet file individually instead of
# loading them all as a Dataset because some issue with the file metadata causes
# an esoteric error when geoarrow tries to collect the files as a Dataset
pin_geometry_df_full <- aws.s3::get_bucket_df(
  bucket = "ccao-data-warehouse-us-east-1",
  prefix = "spatial/parcel/year=",
  max = Inf
) %>%
  filter(str_detect(Key, "\\.parquet$")) %>%
  mutate(year_from_path = as.integer(str_extract(Key, "(?<=year=)\\d{4}"))) %>%
  filter(year_from_path >= 2006 & year_from_path <= max_year) %>%
  mutate(full_path = paste0("s3://ccao-data-warehouse-us-east-1/", Key)) %>%
  select(full_path, year_from_path) %>%
  pmap_dfr(\(full_path, year_from_path) {
    message("Reading: ", full_path)
    geoarrow::read_geoparquet_sf(full_path) %>%
      # Year is a partition so it is not included in the file by default, and
      # we need to add it back in
      mutate(year = year_from_path) %>%
      select(year, x = lon, y = lat, pin10, geometry)
  })

# Write the raw PIN geometry to the filesystem
geoarrow::write_geoparquet(
  pin_geometry_df_full,
  sink = "pin_geometry_raw.parquet",
  compression = "zstd"
)

# For each PIN10, keep only records where the shape/area of the PIN have changed
# and record the start and end year for each unique shape/area
pin_geometry_df_raw <- pin_geometry_df_full %>%
  rename(x = lon, y = lat) %>%
  mutate(area = st_area(geometry)) %>%
  group_by(pin10) %>%
  arrange(pin10, year) %>%
  mutate(across(c(area, x, y), lag, .names = "lag_{.col}")) %>%
  mutate(
    diff_area = !(abs(area - lag_area) < units::set_units(0.001, "m^2")),
    diff_cent = !(abs(x - lag_x) < 0.00001 & abs(y - lag_y) < 0.00001),
    pin_group = cumsum(
      (diff_area & diff_cent) |
        (is.na(diff_area) & is.na(diff_cent))
    )
  ) %>%
  group_by(pin10, pin_group) %>%
  mutate(
    start_year = min(year),
    end_year = max(year)
  ) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(pin10, start_year, end_year, longitude = x, latitude = y, geometry) %>%
  arrange(pin10, start_year)

# Write the transformed PIN geometry to the filesystem
geoarrow::write_geoparquet(
  pin_geometry_df_raw,
  sink = "pin_geometry.parquet",
  compression = "zstd"
)
