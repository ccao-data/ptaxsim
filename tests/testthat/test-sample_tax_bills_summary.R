##### TEST sample_tax_bills_summary #####

library(dplyr)

ptaxsim_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  Sys.getenv("PTAXSIM_DB_PATH")
)
assign("ptaxsim_db_conn", ptaxsim_db_conn, envir = .GlobalEnv)

# Test to make sure we have added sample tax bills for the latest year
test_that("max data year matches max year for sample tax bills", {
  max_year <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    "SELECT data_year_max FROM metadata"
  ) %>%
    pull()

  expect_equal(max(sample_tax_bills_summary$year), max_year)
  expect_equal(max(sample_tax_bills_detail$year), max_year)
})

# Test to make sure we have updated both the summary and detail sample bills
test_that("sample_tax_bills_summary PIN-years match sample_tax_bills_detail", {
  summary_pin_years <- sample_tax_bills_summary %>%
    distinct(year, pin) %>%
    arrange(year, pin)

  detail_pin_years <- sample_tax_bills_detail %>%
    distinct(year, pin) %>%
    arrange(year, pin)

  expect_equal(summary_pin_years, detail_pin_years, ignore_attr = TRUE)
})
