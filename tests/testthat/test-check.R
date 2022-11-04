context("test check_dt()")

##### TEST check_dt() #####

library(data.table)
library(dplyr)

ptaxsim_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  Sys.getenv("PTAXSIM_DB_PATH")
)
assign("ptaxsim_db_conn", ptaxsim_db_conn, envir = .GlobalEnv)

ptaxsim_db_conn_wrong <- DBI::dbConnect(
  RSQLite::SQLite(),
  tempfile(fileext = ".db")
)
assign("ptaxsim_db_conn_wrong", ptaxsim_db_conn_wrong, envir = .GlobalEnv)

# Create a vectors with known, correct lookup values
pins <- c("14081020190000", "09274240240000", "07101010391078")
years <- c(2019, 2019, 2018)
tax_codes <- c("73105", "22031", "35011")

test_that("correct database path works as expected", {
  expect_true(check_db_conn(ptaxsim_db_conn))
})

test_that("lookups return correct checks", {
  expect_true(check_agency_dt_str(lookup_agency(years, tax_codes)))
  expect_true(check_agency_dt_str(lookup_agency(years[1], tax_codes)))
  expect_true(check_pin_dt_str(lookup_pin(years, pins)))
  expect_true(check_pin_dt_str(lookup_pin(years[1], pins)))
  expect_true(check_tif_dt_str(lookup_tif(years, tax_codes)))
  expect_true(check_tif_dt_str(lookup_tif(years[1], tax_codes)))
})

test_that("bad database path throws error", {
  expect_error(check_db_conn(ptaxsim_db_conn_wrong), regexp = "0 Kb")
})

test_that("missing tables throws error", {
  DBI::dbCreateTable(
    ptaxsim_db_conn_wrong,
    "pin",
    data.frame(
      "a" = c(1, 2, 3),
      "b" = c(4, 5, 6)
    )
  )

  expect_error(check_db_conn(ptaxsim_db_conn_wrong), regexp = "missing")
})

test_that("wrong column types throws error", {
  agency <- lookup_agency(years, tax_codes) %>%
    mutate(agency_num = as.integer(agency_num))
  expect_error(check_agency_dt_str(agency))

  pin <- lookup_pin(years, pins) %>%
    mutate(class = as.integer(class))
  expect_error(check_pin_dt_str(pin))

  tif <- lookup_tif(years, tax_codes) %>%
    mutate(agency_num = as.integer(agency_num))
  expect_error(check_tif_dt_str(tif))
})

test_that("outputs fail if not keyed data.table", {
  agency <- lookup_agency(years, tax_codes) %>%
    as_tibble()
  expect_error(check_agency_dt_str(agency))
  agency <- lookup_agency(years, tax_codes)
  expect_equal(key(agency), c("year", "tax_code", "agency_num"))
  setkey(agency, NULL)
  expect_error(check_agency_dt_str(agency))

  pin <- lookup_pin(years, pins) %>%
    as_tibble()
  expect_error(check_pin_dt_str(pin))
  pin <- lookup_pin(years, pins)
  expect_equal(key(pin), c("year", "pin"))
  setkey(pin, NULL)
  expect_error(check_pin_dt_str(pin))

  tif <- lookup_agency(years, tax_codes) %>%
    as_tibble()
  expect_error(check_tif_dt_str(tif))
  tif <- lookup_tif(years, tax_codes)
  expect_equal(key(tif), c("year", "tax_code", "agency_num"))
  setkey(tif, NULL)
  expect_error(check_tif_dt_str(tif))
})

test_that("invalid or closed connection object throws error", {
  DBI::dbDisconnect(ptaxsim_db_conn)
  DBI::dbDisconnect(ptaxsim_db_conn_wrong)
  expect_error(check_db_conn(ptaxsim_db_conn), regexp = "not valid")
  expect_error(check_db_conn(ptaxsim_db_conn_wrong), regexp = "not valid")
})
