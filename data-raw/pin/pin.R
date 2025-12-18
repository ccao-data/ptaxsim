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
library(tidyr)

# Paths for remote storage on S3
remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_bucket_raw <- "s3://ccao-data-raw-us-east-1/"
remote_path_pin <- file.path(remote_bucket, "pin")
remote_path_pin_geometry_raw <- file.path(
  remote_bucket,
  "pin_geometry_raw",
  "part-0.parquet"
)

# Start and end years of data to query, inclusive.
# Set these to the same value if you want to update only one year of data
# start_year <- 2006
start_year <- 2024
end_year <- 2024


# pin --------------------------------------------------------------------------

# Get data frame of all AVs, tax codes, and exemptions per PIN from 2006 to
# 2023. These values come from the legacy CCAO database, which mirrors the
# county mainframe
ccaodata <- dbConnect(
  odbc::odbc(),
  .connection_string = Sys.getenv("DB_CONFIG_CCAODATA")
)

# Pull AV and class from the Clerk and HEAD tables, giving preference to values
# from the Clerk table in case of mismatch (except for property class).
# These tables are pulled from the AS/400 for 2006-2023.
# The corresponding AS/400 files are:
# CLERKVALUES = Library: FOIPRD; File: CLRK_VALUE; Member Y<year>
# TAXBILLAMOUNTS = Library: TXPRD, File: TRESBILL; Member: Y<year>
pin_legacy <- dbGetQuery(
  ccaodata,
  glue_sql("
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
    WHERE C.TAX_YEAR >= {start_year}
      AND C.TAX_YEAR <= 2023
    ORDER BY C.TAX_YEAR, C.PIN, C.CL_ASSD_VAL DESC
  ", .con = ccaodata)
) %>%
  mutate(
    across(c(year, pin, tax_code_num, class), as.character),
    across(c(starts_with("av_"), starts_with("exe_")), as.integer),
    exe_homeowner = ifelse(exe_homeowner < 0L, 0L, exe_homeowner),
    tax_bill_total = tidyr::replace_na(tax_bill_total, 0)
  )

# Establish a connection the Data Department's Athena data warehouse. We'll use
# values from here to fill in any missing values from the legacy system, and
# from the 2024 tax roll export.
ccaoathena <- dbConnect(noctua::athena(), rstudio_conn_tab = FALSE)

# Pull AVs and exemption data from Athena to fill in any missingness from the
# legacy system and the tax roll export.
# TODO:
#  - Exclude CofEs from exemptions
#  - Add tax_code_num
#  - Add tax_bill_total
pin_athena <- dbGetQuery(
  ccaoathena,
  glue_sql("
    SELECT DISTINCT
        roll.pin,
        roll.year,
        val.board_class AS class,
        roll.mailed_taxable_av,
        roll.certified_taxable_av,
        roll.board_taxable_av,
        val.certified_hie,
        val.board_hie,
        roll.exe_homeowner,
        roll.exe_senior,
        roll.exe_freeze,
        roll.exe_longtime_homeowner,
        roll.exe_vet_dis_lt50,
        roll.exe_vet_dis_50_69,
        roll.exe_vet_dis_ge70,
        roll.exe_vet_dis_100,
        roll.exe_vet_returning,
        roll.exe_disabled
    FROM default.vw_pin_tax_roll AS roll
    LEFT JOIN default.vw_pin_value AS val
      ON roll.pin = val.pin
      AND roll.year = val.year
    WHERE roll.year >= '{start_year}'
      AND roll.year <= '{end_year}'
  ", .con = ccaoathena)
) %>%
  # Small tweaks to line up Athena data with tax roll export
  mutate(
    across(c(starts_with("board_"), ends_with("_hie")), as.integer),
    # Prefer Board HIE over Certified HIE, but fall back to Certified to
    # handle a view edge cases where the Board HIE is mysteriously 0
    hie = case_when(
      board_hie != 0 ~ board_hie,
      certified_hie != 0 ~ certified_hie,
      TRUE ~ board_hie
    ),
    across(starts_with("exe_"), \(x) replace_na(x, 0L)),
    # exe_abate is deprecated starting in 2024, but we need to preserve
    # it as an empty column for backwards-compatibility
    exe_abate = 0L,
    exe_vet_dis =
      exe_vet_dis_lt50 +
      exe_vet_dis_50_69 +
      exe_vet_dis_ge70 +
      exe_vet_dis_100,
    class = ifelse(class == "EX", "0", class),
    class = stringr::str_remove_all(class, "[^0-9]"),
    av_tot = board_taxable_av - hie,
    # Temporarily fill missing values
    tax_code_num = NA_character_,
    tax_bill_total = NA_integer_
  )

# Load post-2024 data from the tax roll export in S3.
# We upload these manually to the raw S3 bucket when we receive them
# after tax bills have mailed
pin_tax_roll_csv_files <- get_bucket_df(
  bucket = remote_bucket_raw,
  prefix = "tax/pin_tax_roll",
  max = Inf
) %>%
  # Remove directory references
  filter(Size > 0)

pin_tax_roll <- map_dfr(pin_tax_roll_csv_files$Key, \(f) {
  s3read_using(
    object = f,
    bucket = remote_bucket_raw,
    FUN = readr::read_csv,
    col_types = cols(
      pin = col_character(),
      tax_year = col_character(),
      class = col_character(),
      tax_code = col_character(),
      tax_rate = col_double(),
      assessed_value = col_integer(),
      equalized_assessed_value = col_integer(),
      ex_homeowner_eav = col_integer(),
      ex_senior_eav = col_integer(),
      ex_senior_freeze_eav = col_integer(),
      ex_longtime_homeowner_eav = col_integer(),
      ex_disabled_vet_eav = col_integer(),
      ex_returning_vet_eav = col_integer(),
      ex_disabled_person_eav = col_integer(),
      tax_billed_1 = col_double(),
      tax_billed_2 = col_double(),
      tax_billed_tot = col_double()
    )
  ) %>%
    mutate(exe_abate = 0L) %>%
    rename(
      year = tax_year,
      tax_code_num = tax_code,
      av_clerk = assessed_value,
      exe_homeowner = ex_homeowner_eav,
      exe_senior = ex_senior_eav,
      exe_freeze = ex_senior_freeze_eav,
      exe_longtime_homeowner = ex_longtime_homeowner_eav,
      exe_disabled = ex_disabled_person_eav,
      exe_vet_dis = ex_disabled_vet_eav,
      exe_vet_returning = ex_returning_vet_eav,
      tax_bill_total = tax_billed_tot
    ) %>%
    arrange(year, pin, desc(av_clerk))
})

# Generate a few dataframes containing data quality checks.
# These are not necessary for generating the data, but are
# useful for double-checking the different sources of data
pin_in_athena_not_in_tax_roll <- pin_athena %>%
  anti_join(
    pin_tax_roll,
    by = c("year", "pin")
  )

pin_in_tax_roll_not_in_athena <- pin_tax_roll %>%
  anti_join(
    pin_athena,
    by = c("year", "pin")
  )

# Join tax roll to Athena to preserve any data that is missing from either
# data source
pin_fill <- pin_athena %>%
  full_join(
    pin_tax_roll,
    by = c("year", "pin"),
    suffix = c("_athena", "_tax_roll")
  )

# Diagnostic check to look for mismatches between Athena and tax roll export
pin_mismatch <- pin_fill %>%
  mutate(
    chk_pin_not_in_athena = is.na(class_athena),
    chk_pin_not_in_tax_roll = is.na(class_tax_roll),
    chk_class = class_tax_roll != class_athena,
    chk_av_clerk = av_clerk != av_tot,
    chk_exe_homeowner = exe_homeowner_tax_roll != exe_homeowner_athena,
    chk_exe_senior = exe_senior_tax_roll != exe_senior_athena,
    chk_exe_freeze = exe_freeze_tax_roll != exe_freeze_athena,
    chk_exe_longtime_homeowner =
      exe_longtime_homeowner_tax_roll != exe_longtime_homeowner_athena,
    chk_exe_disabled = exe_disabled_tax_roll != exe_disabled_athena,
    chk_exe_vet_dis = exe_vet_dis_tax_roll != exe_vet_dis_athena,
    chk_exe_vet_returning = exe_vet_returning_tax_roll != exe_vet_returning_athena,
    chk_all = if_any(starts_with("chk_"), \(x) x)
  ) %>%
  filter(chk_all) %>%
  select(
    pin, year,
    # Rearrange each check so the order is Athena -> tax_roll -> check
    chk_pin_not_in_athena, chk_pin_not_in_tax_roll,
    class_tax_roll, class_athena, chk_class,
    av_clerk, av_tot, chk_av_clerk,
    exe_homeowner_tax_roll, exe_homeowner_athena, chk_exe_homeowner,
    exe_senior_tax_roll, exe_senior_athena, chk_exe_senior,
    exe_freeze_tax_roll, exe_freeze_athena, chk_exe_freeze,
    exe_longtime_homeowner_tax_roll,
    exe_longtime_homeowner_athena,
    chk_exe_longtime_homeowner,
    exe_disabled_tax_roll, exe_disabled_athena, chk_exe_disabled,
    exe_vet_dis_tax_roll, exe_vet_dis_athena, chk_exe_vet_dis,
    exe_vet_returning_tax_roll, exe_vet_returning_athena, chk_exe_vet_returning
  )

# Generate summary table showing counts of each type of mismatch
pin_mismatch_summ <- pin_mismatch %>%
  summarise(
    total_mismatch = n(),
    not_in_athena = sum(chk_pin_not_in_athena, na.rm = TRUE),
    not_in_tax_roll = sum(chk_pin_not_in_tax_roll, na.rm = TRUE),
    class = sum(chk_class, na.rm = TRUE),
    av_clerk = sum(chk_av_clerk, na.rm = TRUE),
    exe_homeowner = sum(chk_exe_homeowner, na.rm = TRUE),
    exe_senior = sum(chk_exe_senior, na.rm = TRUE),
    exe_freeze = sum(chk_exe_freeze, na.rm = TRUE),
    exe_longtime_homeowner = sum(chk_exe_longtime_homeowner, na.rm = TRUE),
    exe_disabled = sum(chk_exe_disabled, na.rm = TRUE),
    exe_vet_dis = sum(chk_exe_vet_dis, na.rm = TRUE),
    exe_vet_returning = sum(chk_exe_vet_returning, na.rm = TRUE)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "mismatch_type",
    values_to = "n_mismatches"
  ) %>%
  arrange(desc(n_mismatches))

# Fill missing values in the legacy (pre-2023) PIN data with values from Athena
pin_legacy_fill <- pin_legacy %>%
  # There are a few (less than 100) rows with Clerk AVs split for the same PIN.
  # Sum to get 1 record per PIN, then keep the record with the highest AV
  group_by(year, pin) %>%
  mutate(av_clerk = sum(av_clerk)) %>%
  ungroup() %>%
  distinct(year, pin, .keep_all = TRUE) %>%
  left_join(
    pin_athena %>%
      select(
        pin,
        year,
        mailed_tot = mailed_taxable_av,
        certified_tot = certified_taxable_av,
        board_tot = board_taxable_av
      ),
    by = c("year", "pin")
  ) %>%
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

# Join pre- and post-2024 data
pin <- pin_fill %>%
  mutate(
    class = coalesce(class_tax_roll, class_athena),
    tax_code_num = coalesce(tax_code_num_tax_roll, tax_code_num_athena),
    tax_bill_total = coalesce(tax_bill_total_tax_roll, tax_bill_total_athena),
    av_mailed = mailed_taxable_av,  # Stage AVs are only in Athena
    av_certified = certified_taxable_av,
    av_board = board_taxable_av,
    av_clerk = coalesce(av_clerk, av_tot),
    exe_homeowner = coalesce(exe_homeowner_tax_roll, exe_homeowner_athena),
    exe_senior = coalesce(exe_senior_tax_roll, exe_senior_athena),
    exe_freeze = coalesce(exe_freeze_tax_roll, exe_freeze_athena),
    exe_longtime_homeowner = coalesce(
      exe_longtime_homeowner_tax_roll, exe_longtime_homeowner_athena
    ),
    exe_disabled = coalesce(exe_disabled_tax_roll, exe_disabled_athena),
    exe_vet_dis = coalesce(exe_vet_dis_tax_roll, exe_vet_dis_athena),
    exe_vet_returning = coalesce(
      exe_vet_returning_tax_roll, exe_vet_returning_athena
    ),
    exe_abate = coalesce(exe_abate_tax_roll, exe_abate_athena)
  ) %>%
  select(
    pin,
    year,
    class,
    tax_code_num,
    tax_bill_total,
    av_mailed,
    av_certified,
    av_board,
    av_clerk,
    exe_homeowner,
    exe_senior,
    exe_freeze,
    exe_longtime_homeowner,
    exe_disabled,
    exe_vet_dis,
    exe_vet_returning,
    exe_abate
  )

# pin <- bind_rows(pin, pin_legacy_fill)

# Write to S3
arrow::write_dataset(
  dataset = pin,
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
