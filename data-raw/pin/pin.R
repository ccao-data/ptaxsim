library(arrow)
library(DBI)
library(dplyr)
library(odbc)
library(tidyr)

# Paths for local raw data storage and remote storage on S3. Local storage is
# tracked via Git LFS
local_path <- "data-raw/pin/pin.parquet"
remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path <- file.path(remote_bucket, "pin")

# Get data frame of all AVs, tax codes, and exemptions per PIN since 2006. These
# values come from the legacy CCAO database, which mirrors the county mainframe
ccaodata <- dbConnect(
  odbc::odbc(),
  .connection_string = Sys.getenv("DB_CONFIG_CCAODATA")
)

# Pull AV and class from the Clerk and HEAD tables, giving preference to values
# from the Clerk table in case of mismatch (except for property class)
pin <- dbGetQuery(
  ccaodata, "
  SELECT
      C.TAX_YEAR AS year,
      C.PIN AS pin,
      CASE
          WHEN H.HD_CLASS IS NOT NULL THEN H.HD_CLASS
          ELSE C.CL_CLS
      END AS class,
      C.CL_TXCD AS tax_code_num,
      BILLS.TB_TOT_TAX_AMT AS tax_bill_total,
      C.CL_ASSD_VAL AS av,
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
  LEFT JOIN AS_HEADBR H
      ON C.PIN = H.PIN
      AND C.TAX_YEAR = H.TAX_YEAR
  LEFT JOIN TAXBILLAMOUNTS BILLS
      ON C.PIN = BILLS.PIN
      AND C.TAX_YEAR = BILLS.TAX_YEAR
  WHERE C.TAX_YEAR >= 2006
  ORDER BY C.TAX_YEAR, C.PIN, C.CL_TXCD
  "
) %>%
  mutate(
    across(c(year, pin, tax_code_num, class), as.character),
    across(c(av, starts_with("exe_")), as.integer),
    exe_homeowner = ifelse(exe_homeowner < 0L, 0L, exe_homeowner),
    tax_bill_total = tidyr::replace_na(tax_bill_total, 0)
  ) %>%
  # There are a few rows with AVs split for the same PIN. Sum to get 1 record
  # per PIN, then keep the record with the lowest tax code
  group_by(year, pin) %>%
  mutate(av = sum(av)) %>%
  ungroup() %>%
  distinct(year, pin, .keep_all = TRUE)

# Write to S3
arrow::write_dataset(
  dataset = pin,
  path = remote_path,
  format = "parquet",
  partitioning = "year",
  hive_style = TRUE,
  compression = "zstd"
)
