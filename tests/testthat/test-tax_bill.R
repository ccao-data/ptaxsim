context("test tax_bill()")

##### TEST tax_bill() #####

library(dplyr)
ptaxsim_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  Sys.getenv("PTAXSIM_DB_PATH")
)
assign("ptaxsim_db_conn", ptaxsim_db_conn, envir = .GlobalEnv)

sum_df <- sample_tax_bills_summary
det_df <- sample_tax_bills_detail

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
  expect_error(tax_bill(2019, pins[1], pin_df = c(23000, 959051)))
  expect_error(tax_bill(2019, pins[1], agency_df = c("3232", "TIF")))
  expect_error(tax_bill(2019, pins[1], tif_df = c(23000, 959051)))
})

test_that("incorrect size inputs throw errors", {
  expect_error(tax_bill(years, pins, tax_code_vec = c("73105", "73105")))
  expect_error(tax_bill(years, pins, tax_code_vec = c("73105", "73105")))
})

test_that("data frame inputs throw errors when required cols missing", {
  expect_error(tax_bill(2019, pins[1], pin_df = data.frame()))
  expect_error(tax_bill(2019, pins[1], agency_df = data.frame()))
  expect_error(tax_bill(2019, pins[1], tif_df = data.frame()))
})

test_that("function returns expect data type/structure", {
  expect_s3_class(tax_bill(2018, pins[1]), "data.frame")
  expect_named(
    tax_bill(2018:2019, pins[1:2]),
    c(
      "year", "pin", "class", "tax_code", "av", "eav",
      "agency_num", "agency_name", "agency_tax_rate", "tax_amt_post_exemptions",
      "tax_amt_total_to_tif", "tax_amt_final"
    )
  )
  expect_equal(
    sum(is.na(tax_bill(sum_df$year, sum_df$pin))),
    0
  )
  expect_named(
    tax_bill(2018:2019, pins[1:2], simplify = FALSE),
    c(
      "year", "pin", "class", "tax_code", "av", "eav", "exe_total",
      "agency_num", "agency_name", "agency_total_ext", "agency_total_eav",
      "agency_tax_rate", "tax_amt_exempt", "tax_amt_pre_exemptions",
      "tax_amt_post_exemptions", "tif_agency_num", "tif_share",
      "tax_rate_for_cps", "tax_prop_for_cps", "tax_amt_rpm_tif_to_cps",
      "tax_amt_rpm_tif_to_rpm", "tax_amt_rpm_tif_back_to_jur_total",
      "tax_amt_rpm_tif_back_to_jur_dist", "tax_amt_rpm_tif_back_to_jur",
      "tax_amt_total_to_tif", "tax_amt_final"
    )
  )
  expect_equal(
    sum(is.na(tax_bill(years[1], pins[1], simplify = FALSE))),
    0
  )
})

test_that("returned amount/output correct for single PIN", {
  # District level tax amounts
  expect_equivalent(
    tax_bill(2019, pins[1], simplify = FALSE) %>%
      select(year, pin, agency = agency_num, tax = tax_amt_final) %>%
      arrange(agency) %>%
      as_tibble(),
    det_df %>%
      filter(pin == pins[1]) %>%
      select(year, pin, agency, tax) %>%
      arrange(agency) %>%
      as_tibble(),
    tolerance = 0.005
  )
  # TIF total amounts
  expect_equal(
    tax_bill(2019, pins[1]) %>%
      pull(tax_amt_total_to_tif) %>%
      sum(),
    det_df %>%
      filter(pin == pins[1]) %>%
      slice(1) %>%
      pull(tif_total),
    tolerance = 0.001
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
sum_df <- sum_df %>%
  filter(!pin %in% exclude_pins) %>%
  as_tibble()
det_df <- det_df %>%
  filter(!pin %in% exclude_pins) %>%
  as_tibble()

test_that("returned amount/output correct for all sample bills", {
  # Output is correct number of rows
  expect_equal(
    tax_bill(sum_df$year, sum_df$pin, simplify = FALSE) %>%
      nrow(),
    371
  )

  # District level tax amounts
  expect_equivalent(
    tax_bill(sum_df$year, sum_df$pin, simplify = FALSE) %>%
      select(year, pin, agency = agency_num, tax_amt_final) %>%
      arrange(year, pin, agency),
    det_df %>%
      select(year, pin, agency, tax_amt_final = tax) %>%
      arrange(year, pin, agency) %>%
      as_tibble(),
    tolerance = 0.005
  )

  # TIF total amounts
  expect_equivalent(
    tax_bill(sum_df$year, sum_df$pin) %>%
      group_by(year, pin) %>%
      summarize(tif_total = sum(tax_amt_total_to_tif)) %>%
      arrange(pin),
    det_df %>%
      group_by(year, pin) %>%
      summarize(tif_total = first(tif_total)) %>%
      arrange(pin) %>%
      as_tibble(),
    tolerance = 0.005
  )
})

# Exclude certain PINs in the RPM TIF or with extremely high bills
# Will run separate tests for these
sum_df_no_rpm <- sum_df %>%
  filter(!pin %in% c("14174100180000", "01363010130000"))

test_that("all differences are less than $25", {
  expect_true(
    left_join(
      tax_bill(sum_df_no_rpm$year, sum_df_no_rpm$pin, simplify = FALSE) %>%
        select(year, pin, agency = agency_num, tax_calc = tax_amt_final),
      det_df %>%
        select(year, pin, agency, tax_real = tax) %>%
        as_tibble(),
      by = c("year", "pin", "agency")
    ) %>%
      mutate(diff = abs(tax_calc - tax_real) < 25) %>%
      pull(diff) %>%
      all()
  )
})

DBI::dbDisconnect(ptaxsim_db_conn)
