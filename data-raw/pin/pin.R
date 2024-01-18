library(arrow)
library(DBI)
library(dplyr)
library(geoarrow)
library(noctua)
library(odbc)
library(sf)
library(tidyr)

# Paths for remote storage on S3
remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path_pin <- file.path(remote_bucket, "pin")
remote_path_pin_geometry_raw <- file.path(
  remote_bucket,
  "pin_geometry_raw",
  "part-0.parquet"
)




# pin --------------------------------------------------------------------------

# Get data frame of all AVs, tax codes, and exemptions per PIN since 2006. These
# values come from the legacy CCAO database, which mirrors the county mainframe
ccaodata <- dbConnect(
  odbc::odbc(),
  .connection_string = Sys.getenv("DB_CONFIG_CCAODATA")
)

# Establish a connection the Data Department's Athena data warehouse. We'll use
# values from here to fill in any missing values from the legacy system
ccaoathena <- dbConnect(noctua::athena())

# Pull AV and class from the Clerk and HEAD tables, giving preference to values
# from the Clerk table in case of mismatch (except for property class).
# These tables are pulled from the AS/400 and will be pulled from iasWorld
# in the future. The corresponding AS/400 files are:
# CLERKVALUES = Library: FOIPRD; File: CLRK_VALUE; Member Y<year>
# TAXBILLAMOUNTS = Library: TXPRD, File: TRESBILL; Member: Y<year>
pin <- dbGetQuery(
  ccaodata, "
  SELECT
      C.TAX_YEAR AS year,
      C.PIN AS pin,
      CASE
          WHEN BR.HD_CLASS IS NOT NULL THEN BR.HD_CLASS
          ELSE C.CL_CLS
      END AS class,
      C.CL_TXCD AS tax_code_num,
      BILLS.TB_TOT_TAX_AMT AS tax_bill_total,
      (T.HD_ASS_LND + T.HD_ASS_BLD) AS av_mailed,
      (TB.HD_ASS_LND + TB.HD_ASS_BLD) AS av_certified,
      (BR.HD_ASS_LND + BR.HD_ASS_BLD) AS av_board,
      C.CL_ASSD_VAL AS av_clerk,
      C.CL_HOMOWN_EXE_AMT AS exe_homeowner,
      C.CL_HOMSTD_EXE_AMT AS exe_senior,
      C.CL_FREEZE_EXE_AMT AS exe_freeze,
      C.CL_EXOWNEXVAL AS exe_longtime_homeowner,
      C.CL_DIS_VET AS exe_disabled,
      C.CL_RET_VET AS exe_vet_returning,
      C.CL_DIS_VET_LT75K AS exe_vet_dis_lt50,
      C.CL_DIS_VET_GE75K AS exe_vet_dis_50_69,
      C.CL_VET_EXE_AMT AS exe_vet_dis_ge70,
      C.CL_ABATE_AMT AS exe_abate
  FROM CLERKVALUES C
  LEFT JOIN AS_HEADT T
      ON C.PIN = T.PIN
      AND C.TAX_YEAR = T.TAX_YEAR
  LEFT JOIN AS_HEADTB TB
      ON C.PIN = TB.PIN
      AND C.TAX_YEAR = TB.TAX_YEAR
  LEFT JOIN AS_HEADBR BR
      ON C.PIN = BR.PIN
      AND C.TAX_YEAR = BR.TAX_YEAR
  LEFT JOIN TAXBILLAMOUNTS BILLS
      ON C.PIN = BILLS.PIN
      AND C.TAX_YEAR = BILLS.TAX_YEAR
  WHERE C.TAX_YEAR >= 2006
  ORDER BY C.TAX_YEAR, C.PIN, C.CL_ASSD_VAL DESC
  "
) %>%
  mutate(
    across(c(year, pin, tax_code_num, class), as.character),
    across(c(starts_with("av_"), starts_with("exe_")), as.integer),
    exe_homeowner = ifelse(exe_homeowner < 0L, 0L, exe_homeowner),
    tax_bill_total = tidyr::replace_na(tax_bill_total, 0)
  )

# Pull AVs from Athena to fill in any missingness from the legacy system
pin_athena <- dbGetQuery(
  ccaoathena,
  "
  SELECT DISTINCT
      pin,
      year,
      mailed_tot,
      certified_tot,
      board_tot
  FROM default.vw_pin_value
  WHERE year >= '2006'
  "
) %>%
  mutate(
    across(c(year, pin), as.character),
    across(c(ends_with("_tot")), as.integer)
  )

pin_fill <- pin %>%
  # There are a few (less than 100) rows with Clerk AVs split for the same PIN.
  # Sum to get 1 record per PIN, then keep the record with the highest AV
  group_by(year, pin) %>%
  mutate(av_clerk = sum(av_clerk)) %>%
  ungroup() %>%
  distinct(year, pin, .keep_all = TRUE) %>%
  left_join(pin_athena, by = c("year", "pin")) %>%
  mutate(
    av_board = ifelse(is.na(av_board), board_tot, av_board),
    av_certified = ifelse(is.na(av_certified), certified_tot, av_certified),
    av_mailed = ifelse(is.na(av_mailed), mailed_tot, av_mailed)
  ) %>%
  # A few (less than 500) values are missing from the mailed assessment stage
  # AV column. We can replace any missing mailed value with certified value
  # from the same year. Only 2 board/certified values are missing, and both are
  # exempt property
  mutate(
    av_board = ifelse(is.na(av_board), 0L, av_board),
    av_certified = ifelse(is.na(av_certified), 0L, av_certified),
    av_mailed = ifelse(is.na(av_mailed), av_certified, av_mailed)
  ) %>%
  select(-ends_with("_tot"))

# Write to S3
arrow::write_dataset(
  dataset = pin_fill,
  path = remote_path_pin,
  format = "parquet",
  partitioning = "year",
  hive_style = TRUE,
  compression = "zstd"
)




# pin_geometry -----------------------------------------------------------------

# Grab the maximum year of agency data so we can limit the scope of the parcel
# file query
agency_df <- read_parquet(file.path(remote_bucket, "agency", "part-0.parquet"))
max_year <- max(as.integer(agency_df$year))

# Grab parcel shapes from S3. These files are originally from the data portal
# and Cook Central
remote_bucket_geometry <- "s3://ccao-data-warehouse-us-east-1/spatial/parcel"
pin_geometry_df_full <- arrow::open_dataset(remote_bucket_geometry) %>%
  filter(year >= 2006 & year <= max_year) %>%
  select(year, x = lon, y = lat, pin10, geometry) %>%
  geoarrow_collect_sf()

# For each PIN10, keep only records where the shape/area of the PIN have changed
# and record the start and end year for each unique shape/area
pin_geometry_df_raw <- pin_geometry_df_full %>%
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

# Write the raw PIN geometry to S3
geoarrow::write_geoparquet(
  pin_geometry_df_raw,
  sink = remote_path_pin_geometry_raw,
  compression = "zstd"
)
