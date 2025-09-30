context("test tax_bill()")

##### TEST tax_bill() #####

library(data.table)
library(dplyr)
ptaxsim_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  Sys.getenv("PTAXSIM_DB_PATH")
)
assign("ptaxsim_db_conn", ptaxsim_db_conn, envir = .GlobalEnv)

sum_dt <- sample_tax_bills_summary
det_dt <- sample_tax_bills_detail

# Create a vector of PINs with known, correct lookup values
pins <- c(
  "14081020190000", "09274240240000", "07101010391078"
)
years <- c(2019, 2019, 2018)

test_that("bad/incorrect vector inputs throw errors", {
  expect_error(tax_bill("2019", pins[1]))
  expect_error(tax_bill(numeric(0), pins[1]))
  expect_error(tax_bill(2019, 14081020190000))
  expect_error(tax_bill(c(2000, 2019), pins[1]))
  expect_error(tax_bill(2019, c("1408102019000", pins[2])))
  expect_error(tax_bill(2019, pins[1], tax_code_vec = 73105))
  expect_error(tax_bill(2019, pins[1], tax_code_vec = "5454"))
  expect_error(tax_bill(2019, pins[2], simplify = "yes"))
  expect_error(tax_bill(2018:2019, pins[2:3], simplify = c(TRUE, FALSE)))
})

test_that("bad/incorrect data frame inputs throw errors", {
  expect_error(tax_bill(2019, pins[1], pin_dt = c(23000, 959051)))
  expect_error(tax_bill(2019, pins[1], agency_dt = c("3232", "TIF")))
  expect_error(tax_bill(2019, pins[1], tif_dt = c(23000, 959051)))
})

test_that("non-unique values to main args throws error", {
  expect_error(tax_bill(2019, pins[c(1, 1)]))
  expect_error(tax_bill(c(2010, 2010), pins[1]))
  expect_error(
    tax_bill(c(2010, 2010), pins[c(1, 2)], tax_code_vec = c("73105"))
  )
})

test_that("incorrect size inputs throw errors", {
  expect_error(tax_bill(years, pins, tax_code_vec = c("73105", "73105")))
  expect_error(tax_bill(years, pins, tax_code_vec = c("73105", "73105")))
})

test_that("data frame inputs throw errors when required cols missing", {
  expect_error(tax_bill(2019, pins[1], pin_dt = data.table()))
  expect_error(tax_bill(2019, pins[1], agency_dt = data.table()))
  expect_error(tax_bill(2019, pins[1], tif_dt = data.table()))
})

test_that("function returns expected data type/structure", {
  expect_s3_class(tax_bill(2018, pins[1]), "data.frame")
  expect_s3_class(tax_bill(2018, pins[1]), "data.table")
  expect_equal(
    key(tax_bill(2018, pins[1])),
    c("year", "pin", "agency_num")
  )
  expect_named(
    tax_bill(2018:2019, pins[1:2]),
    c(
      "year", "pin", "class", "tax_code", "av", "eav", "agency_num",
      "agency_name", "agency_major_type", "agency_minor_type",
      "agency_tax_rate", "final_tax"
    )
  )
  expect_equal(
    sum(is.na(tax_bill(sum_dt$year, sum_dt$pin))),
    0
  )
  expect_named(
    tax_bill(2018:2019, pins[1:2], simplify = FALSE),
    c(
      "year", "pin", "class", "tax_code", "av", "eav", "exe_total",
      "agency_num", "agency_name", "agency_major_type", "agency_minor_type",
      "agency_total_ext", "agency_total_eav", "agency_tax_rate",
      "tax_amt_exe", "tax_amt_pre_exe", "tax_amt_post_exe", "tif_agency_num",
      "tif_agency_name", "tif_share", "transit_tif_to_cps", "transit_tif_to_tif",
      "transit_tif_to_dist", "final_tax_to_tif", "final_tax_to_dist"
    )
  )
  expect_equal(
    sum(is.na(tax_bill(years[1], pins[1], simplify = FALSE))),
    0
  )
  expect_equal(dim(tax_bill(years[1], pins[1], simplify = TRUE)), c(12, 12))
  expect_equal(dim(tax_bill(years[1], pins[1], simplify = FALSE)), c(10, 25))
})

test_that("returned amount/output correct for single PIN", {
  # District level tax amounts
  expect_equivalent(
    tax_bill(2018, pins[3], simplify = TRUE) %>%
      select(year, pin, agency_num, final_tax) %>%
      arrange(agency_num) %>%
      as_tibble(),
    det_dt %>%
      filter(pin == pins[3]) %>%
      select(year, pin, agency_num, final_tax) %>%
      arrange(agency_num) %>%
      as_tibble(),
    tolerance = 0.005
  )
})

test_that("grid expansion works correctly", {
  expect_equal(
    tax_bill(2006:2020, pins[1:3]) %>%
      group_by(pin) %>%
      summarize(n_year = length(unique(year))) %>%
      arrange(pin),
    tribble(
      ~"pin", ~"n_year",
      "07101010391078", 13,
      "09274240240000", 15,
      "14081020190000", 15
    )
  )
})

# Remove certain PINs from the test because they are anomalies/have VERY unique
# situations
exclude_pins <- c("20031180060000")
sum_dt <- sum_dt %>%
  filter(!pin %in% exclude_pins) %>%
  as_tibble()
det_dt <- det_dt %>%
  filter(!pin %in% exclude_pins) %>%
  as_tibble()

test_that("returned amount/output correct for all sample bills", {
  # Output is correct number of rows
  expect_equal(
    tax_bill(sum_dt$year, sum_dt$pin, simplify = FALSE) %>%
      nrow(),
    753
  )
  expect_equal(
    tax_bill(sum_dt$year, sum_dt$pin, simplify = TRUE) %>%
      nrow(),
    781
  )

  base_bills <- tax_bill(sum_dt$year, sum_dt$pin, simplify = TRUE) %>%
    select(year, pin, agency_num, final_tax)
  transit_bills <- base_bills %>%
    count(pin, agency_num) %>%
    filter(n > 1)

  # District level tax amounts
  expect_equivalent(
    tax_bill(sum_dt$year, sum_dt$pin, simplify = TRUE) %>%
      select(year, pin, agency_num, final_tax) %>%
      filter(!pin %in% transit_bills$pin) %>%
      group_by(year, pin, agency_num) %>%
      summarize(final_tax = sum(final_tax)) %>% # combine cps transit tif rows w/ tif row
      ungroup() %>%
      arrange(year, pin, agency_num),
    det_dt %>%
      select(year, pin, agency_num, final_tax) %>%
      filter(!pin %in% transit_bills$pin) %>%
      arrange(year, pin, agency_num) %>%
      as_tibble(),
    tolerance = 0.005
  )
})

# Exclude certain PINs in the RPM TIF or with extremely high bills
# Will run separate tests for these
sum_dt_no_rpm <- sum_dt %>%
  filter(!pin %in% c(
    "14174100180000",
    "01363010130000",
    "14333001380000",
    # TODO: The PIN below has an exemption on its 2022 bill but not in the
    # 2022 clerk data. Seems like a new parcel, need to investigate further
    "10252080490000",
    # TODO: This PIN is a huge and within the RPM TIF, so the values are off
    # slightly compared to the real bill, see issue #4
    "14211000010000",
    "14081020190000",
    "14081020210000",
    "14294210090000"
  ))

test_that("all differences are less than $25", {
  expect_true(
    left_join(
      tax_bill(sum_dt_no_rpm$year, sum_dt_no_rpm$pin) %>%
        select(year, pin, agency_num, tax_calc = final_tax),
      det_dt %>%
        select(year, pin, agency_num, tax_real = final_tax) %>%
        as_tibble(),
      by = c("year", "pin", "agency_num")
    ) %>%
      mutate(diff = abs(tax_calc - tax_real) < 25) %>%
      pull(diff) %>%
      all()
  )
})

test_that("no tax_bill inputs modified by reference", {
  test_tax_codes <- lookup_tax_code(years, pins)
  test_pin_dt <- lookup_pin(years, pins)
  test_agency_dt <- lookup_agency(years, test_tax_codes)
  test_tif_dt <- lookup_tif(years, test_tax_codes)

  test_tax_codes_og <- copy(test_tax_codes)
  test_pin_dt_og <- copy(test_pin_dt)
  test_agency_dt_og <- copy(test_agency_dt)
  test_tif_dt_og <- copy(test_tif_dt)

  tax_bill(
    year_vec = years,
    pin_vec = pins,
    tax_code_vec = test_tax_codes,
    agency_dt = test_agency_dt,
    pin_dt = test_pin_dt,
    tif_dt = test_tif_dt
  )

  expect_identical(test_tax_codes, test_tax_codes_og)
  expect_identical(test_pin_dt, test_pin_dt_og)
  expect_identical(test_agency_dt, test_agency_dt_og)
  expect_identical(test_tif_dt, test_tif_dt_og)
})

test_that("agnostic to input data.table row order", {
  tax_codes <- lookup_tax_code(2018:2021, sum_dt$pin)

  agency_dt_original <- lookup_agency(2018:2021, tax_codes)
  agency_dt_arranged <- lookup_agency(2018:2021, tax_codes) %>%
    arrange(agency_num, year)

  tif_dt_original <- lookup_tif(2018:2021, tax_codes)
  tif_dt_arranged <- lookup_tif(2018:2021, tax_codes) %>%
    arrange(agency_num, year)

  pin_dt_original <- lookup_pin(2018:2021, sum_dt$pin)
  pin_dt_arranged <- lookup_pin(2018:2021, sum_dt$pin) %>%
    arrange(pin, year)

  expect_identical(
    tax_bill(years, pins, agency_dt = agency_dt_original),
    tax_bill(years, pins, agency_dt = agency_dt_arranged)
  )
  expect_identical(
    tax_bill(years, pins, tif_dt = tif_dt_original),
    tax_bill(years, pins, tif_dt = tif_dt_arranged)
  )
  expect_identical(
    tax_bill(years, pins, pin_dt = pin_dt_original),
    tax_bill(years, pins, pin_dt = pin_dt_arranged)
  )
})

test_that("Returns 0 for agency with base/levy of 0", {
  expect_false(
    any(is.nan(tax_bill(2022, c("12283000140000", "12284120030000"))$final_tax))
  )
})

test_that("Simplify FALSE / TRUE identical", {
  # transit tif tax code
  transit_tif_pins <- tbl(ptaxsim_db_conn, "pin") %>%
    filter(tax_code_num == "73105", year == 2023) %>%
    slice_sample(n = 100) %>%
    select(pin) %>%
    collect() %>%
    pull(pin)

  simp_bills <- tax_bill(2023, transit_tif_pins, simplify = TRUE)
  not_simp_bills <- tax_bill(2023, transit_tif_pins, simplify = FALSE)

  expect_equivalent(
    simp_bills %>% summarize(total_tax = sum(final_tax)),
    not_simp_bills %>% summarize(total_tax = sum(final_tax_to_tif + final_tax_to_dist - transit_tif_to_dist)),
    tolerance = 0.005
  )
})

DBI::dbDisconnect(ptaxsim_db_conn)
