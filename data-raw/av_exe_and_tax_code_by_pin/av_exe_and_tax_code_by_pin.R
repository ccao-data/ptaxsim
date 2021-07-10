library(DBI)
library(odbc)
library(dplyr)
library(stringr)
library(readr)
library(data.table)

file_path <- paste0(
  "data-raw/av_exe_and_tax_code_by_pin",
  "/av_exe_and_tax_code_by_pin.csv"
)

# Get a table of all AVs, tax codes, and exemptions since 2014
if (!file.exists(str_c(file_path, ".bz2"))) {
  CCAODATA <- dbConnect(
    odbc::odbc(),
    .connection_string = Sys.getenv("DB_CONFIG_CCAODATA")
  )

  query_result <- dbGetQuery(
    CCAODATA,
    "SELECT * FROM CLERKVALUES WHERE TAX_YEAR >= 2014"
  )

  query_result %>%
    select(
      year = TAX_YEAR,
      pin = PIN,
      tax_code = CL_TXCD,
      class = CL_CLS,
      av = CL_ASSD_VAL,
      exe_homeowner = CL_HOMOWN_EXE_AMT,
      exe_senior = CL_HOMSTD_EXE_AMT,
      exe_freeze = CL_FREEZE_EXE_AMT,
      exe_longtime_homeowner = CL_EXOWNEXVAL,
      exe_disabled = CL_DIS_VET,
      exe_vet_returning = CL_RET_VET,
      exe_vet_dis_lt50 = CL_DIS_VET_LT75K,
      exe_vet_dis_50_69 = CL_DIS_VET_GE75K,
      exe_vet_dis_ge70 = CL_VET_EXE_AMT,
      exe_abate = CL_ABATE_AMT
    ) %>%
    readr::write_csv(file_path)
}

# Load data from CSV, then save to internal R data
av_exe_and_tax_code_by_pin <- readr::read_csv(str_c(file_path, ".bz2"))

# Ensure expected col types
av_exe_and_tax_code_by_pin <- av_exe_and_tax_code_by_pin %>%
  mutate(across(c(pin, tax_code, class), as.character))

# Convert the data to a data.table and use setkey to sort for faster joins
av_exe_and_tax_code_by_pin <- as.data.table(av_exe_and_tax_code_by_pin)
setkey(av_exe_and_tax_code_by_pin, year, pin)

# Save data to package
usethis::use_data(av_exe_and_tax_code_by_pin, overwrite = TRUE)
