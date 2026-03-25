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
start_year <- 2006
end_year <- 2024


# pin --------------------------------------------------------------------------

# Get data frame of all AVs, tax codes, and exemptions per PIN from 2006 to
# 2023. These values come from the legacy CCAO database, which mirrors the
# county mainframe.
# Only query this data if we are pulling data for years up to 2023
if (start_year <= 2023) {
  ccaodata <- dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("DB_CONFIG_CCAODATA")
  )

  # Pull AV and class from the Clerk and HEAD tables, giving preference to
  # values from the Clerk table in case of mismatch (except for property class).
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
      tax_bill_total = tidyr::replace_na(tax_bill_total, 0),
      # This exemption is new in 2024 and does not exist in the legacy data
      exe_vet_dis_100 = 0L
    )
} else {
  # If we are only pulling data post-2024, create an empty tibble for the
  # legacy data
  pin_legacy <- tibble(
    year = character(),
    pin = character(),
    class = character(),
    tax_code_num = character(),
    tax_bill_total = numeric(),
    av_mailed = integer(),
    av_certified = integer(),
    av_board = integer(),
    av_clerk = integer(),
    exe_homeowner = integer(),
    exe_senior = integer(),
    exe_freeze = integer(),
    exe_longtime_homeowner = integer(),
    exe_disabled = integer(),
    exe_vet_returning = integer(),
    exe_vet_dis_lt50 = integer(),
    exe_vet_dis_50_69 = integer(),
    exe_vet_dis_ge70 = integer(),
    exe_vet_dis_100 = integer(),
    exe_abate = integer()
  )
}

# Establish a connection the Data Department's Athena data warehouse. We'll use
# values from here to fill in any missing values from the legacy system, and
# from the 2024 tax roll export.
ccaoathena <- dbConnect(noctua::athena(), rstudio_conn_tab = FALSE)

# Pull AVs from Athena to fill in any missingness from the legacy system.
pin_athena <- dbGetQuery(
  ccaoathena,
  glue_sql("
  SELECT DISTINCT
      pin,
      year,
      mailed_tot,
      certified_tot,
      board_tot
  FROM default.vw_pin_value
  WHERE year >= '{start_year}'
    AND year <= '{end_year}'
  ", .con = ccaoathena)
) %>%
  mutate(
    across(c(year, pin), as.character),
    across(c(ends_with("_tot")), as.integer)
  )

# Pull veteran disability exemption breakdowns from Athena to fill in
# missingness from the tax roll export, which does not include specific
# disability levels
pin_exe_vetdis_athena <- dbGetQuery(
  ccaoathena,
  glue_sql("
  SELECT pin, year, exemption_type, exemption_amount
  FROM default.vw_pin_exe_long
  WHERE NOT COALESCE(is_cofe, FALSE)
    AND exemption_type IN (
      'exe_vet_dis_lt50',
      'exe_vet_dis_50_69',
      'exe_vet_dis_ge70',
      'exe_vet_dis_100'
    )
    AND COALESCE(exemption_amount, 0) > 0
    AND year >= '2024'
    AND year <= '{end_year}'
  ", .con = ccaoathena),
) %>%
  # Pivot wider so we have one column per exemption type
  mutate(
    across(c(year, pin), as.character),
    across(starts_with("exe_"), as.integer)
  ) %>%
  pivot_wider(
    names_from = exemption_type,
    values_from = exemption_amount,
    values_fill = 0L
  ) %>%
  mutate(
    exe_vet_dis_tot = exe_vet_dis_lt50 +
      exe_vet_dis_50_69 +
      exe_vet_dis_ge70 +
      exe_vet_dis_100
  ) %>%
  relocate(
    pin,
    year,
    exe_vet_dis_lt50,
    exe_vet_dis_50_69,
    exe_vet_dis_ge70,
    exe_vet_dis_100
  ) %>%
  rename_with(~ paste0(.x, "_athena"), starts_with("exe_"))

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
    # Preserve exe_abate for backwards-compatibility, even though it isn't
    # defined in the tax roll export
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
    # Drop some fields that are only present post-2024
    select(
      -tax_rate, -equalized_assessed_value, -tax_billed_1, -tax_billed_2
    ) %>%
    arrange(year, pin, desc(av_clerk))
})

# Fill missing values in the legacy (pre-2023) PIN data with values from Athena
pin_legacy_fill <- pin_legacy %>%
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

# Fill missing values in post-2024 data with values from Athena
pin_tax_roll_fill <- pin_tax_roll %>%
  left_join(pin_athena, by = c("year", "pin")) %>%
  rename(
    av_mailed = mailed_tot,
    av_certified = certified_tot,
    av_board = board_tot
  ) %>%
  # Fill missing vetdis exemptions in post-2024 data, since the tax roll
  # export reports total vetdis exemptions and does not break them out by
  # disability level
  left_join(pin_exe_vetdis_athena, by = c("year", "pin")) %>%
  mutate(across(ends_with("_athena"), \(x) replace_na(x, 0L))) %>%
  mutate(
    exe_vet_dis_lt50 = ifelse(
      # If the vetdis total from the tax bill export matches the sum of all
      # individual vetdis exemptions from Athena (ias), then we can be confident
      # filling the individual vetdis exemptions directly from Athena
      exe_vet_dis == exe_vet_dis_tot_athena,
      exe_vet_dis_lt50_athena,
      ifelse(
        # If the total from the tax bill export does _not_ match the sum from
        # Athena, then one of two cases is true, according to our investigation:
        #
        #  1. The tax bill export has a vetdis total that is >0 but different
        #     from the Athena sum. If Athena has a value >0 for this particular
        #     vetdis exemption, then we use the total from the tax bill export
        #     for this exemption, because we assume the Athena data just has the
        #     wrong amount (in theory, it is not possible for multiple vetdis
        #     exemption types to be >0 for the same PIN). If instead there are
        #     no vetdis exemptions with a value >0, then we fill the value into
        #     the >70% vetdis exemption type, which is the most common vetdis
        #     exemption type in the Athena data
        #
        #  2. The tax bill export has a vetdis total of 0, but Athena has
        #     a sum >0 for vetdis exemptions. In this case, we assume the tax
        #     bill export is correct, and we fill 0 for all individual
        #     exemption types.
        exe_vet_dis > 0 & exe_vet_dis_lt50_athena > 0,
        exe_vet_dis,
        0L
      )
    ),
    exe_vet_dis_50_69 = ifelse(
      exe_vet_dis == exe_vet_dis_tot_athena,
      exe_vet_dis_50_69_athena,
      ifelse(
        exe_vet_dis > 0 & exe_vet_dis_50_69_athena > 0,
        exe_vet_dis,
        0L
      )
    ),
    exe_vet_dis_ge70 = ifelse(
      exe_vet_dis == exe_vet_dis_tot_athena,
      exe_vet_dis_ge70_athena,
      case_when(
        exe_vet_dis > 0 & exe_vet_dis_ge70_athena > 0 ~ exe_vet_dis,
        # This is the most common type of vetdis exemption, so fill it with
        # the total from the tax bill export if no vetdis exemption types
        # have a value >0 in the Athena data
        exe_vet_dis > 0 & exe_vet_dis_tot_athena == 0 ~ exe_vet_dis,
        TRUE ~ 0L
      )
    ),
    exe_vet_dis_100 = ifelse(
      exe_vet_dis == exe_vet_dis_tot_athena,
      exe_vet_dis_100_athena,
      ifelse(
        exe_vet_dis > 0 & exe_vet_dis_100_athena > 0,
        exe_vet_dis,
        0L
      )
    )
  ) %>%
  select(-exe_vet_dis, -ends_with("_athena"))

# Join pre- and post-2024 data
pin <- bind_rows(pin_legacy_fill, pin_tax_roll_fill)

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
  select(year, x = lon, y = lat, pin10, town_code, geometry) %>%
  geoarrow_collect_sf() %>%
  # Fill missing townships, prefering the newest available
  arrange(pin10, year) %>%
  fill(town_code, .direction = "updown", .by = pin10) %>%
  # For remaining missing town codes, replace with 99 to make looping through
  # them below easier.
  mutate(town_code = replace_na(town_code, 99))

# For each PIN10, keep only records where the shape/area of the PIN have changed
# and record the start and end year for each unique shape/area
pin_geometry_df_raw <- map(unique(pin_geometry_df_full$town_code), \(town) {
  print(glue("Processing {town}"))
  pin_geometry_df_full %>%
    filter(town_code == town) %>%
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
    select(
      pin10, start_year, end_year,
      longitude = x, latitude = y, geometry
    ) %>%
    arrange(pin10, start_year)
}, .progress = TRUE) %>%
  bind_rows()

# Write the raw PIN geometry to S3
geoarrow::write_geoparquet(
  pin_geometry_df_raw,
  sink = remote_path_pin_geometry_raw,
  compression = "zstd"
)
