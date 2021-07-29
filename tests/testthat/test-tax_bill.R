context("test tax_bill()")

##### TEST tax_bill() #####

library(dplyr)
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
  expect_error(tax_bill(2019, pins[1], eav_vec = "20000"))
  expect_error(tax_bill(2019, pins[1], eav_vec = numeric(0)))
  expect_error(tax_bill(2019, pins[1], eq_fct_vec = "0.055"))
  expect_error(tax_bill(2019, pins[1], eq_fct_vec = numeric(0)))
  expect_error(tax_bill(2019, pins[2], simplify = "yes"))
  expect_error(tax_bill(2018:2019, pins[2:3], simplify = c(TRUE, FALSE)))
})

test_that("bad/incorrect data frame inputs throw errors", {
  expect_error(tax_bill(2019, pins[1], exemptions_df = c(23000, 959051)))
  expect_error(tax_bill(2019, pins[1], levies_df = c("3232", "TIF")))
  expect_error(tax_bill(2019, pins[1], tifs_df = c(23000, 959051)))
  expect_error(tax_bill(2019, pins[1], agency_eavs_df = c(23000, 959051)))
})

test_that("incorrect size inputs throw errors", {
  expect_error(tax_bill(c(2018, 2019), pins))
  expect_error(tax_bill(c(2018, 2018, 2019), c(pins[1], pins[2])))
  expect_error(tax_bill(years, pins, tax_code_vec = c("73105", "73105")))
  expect_error(tax_bill(years, pins, tax_code_vec = c("73105", "73105")))
  expect_error(tax_bill(years, pins, eav_vec = c(200000, 130000)))
  expect_error(tax_bill(years, pins, eq_fct_vec = c(0.3, 0.3, 0.3, 0.4)))
})

test_that("data frame inputs throw errors when required cols missing", {
  expect_error(tax_bill(2019, pins[1], exemptions_df = data.frame()))
  expect_error(tax_bill(2019, pins[1], agency_eavs_df = data.frame()))
  expect_error(tax_bill(2019, pins[1], tifs_df = data.frame()))
  expect_error(tax_bill(2019, pins[1], levies_df = data.frame()))
})

test_that("function returns expect data type/structure", {
  expect_s3_class(tax_bill(2018, pins[1]), "data.frame")
  expect_named(
    tax_bill(2018:2019, pins[1:2]),
    c(
      "year", "pin", "tax_code", "agency", "agency_name", "agency_tax_rate",
      "eav_before_exemptions", "tax_amt_post_exemptions",
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
      "year", "pin", "tax_code", "eav_before_exemptions", "eq_factor",
      "exe_homeowner", "exe_senior", "exe_freeze", "exe_longtime_homeowner",
      "exe_disabled", "exe_vet_returning", "exe_vet_dis_lt50",
      "exe_vet_dis_50_69", "exe_vet_dis_ge70", "exe_abate", "tif_agency",
      "tif_share", "in_rpm_tif", "agency", "agency_name", "total_levy",
      "total_eav", "agency_tax_rate", "tax_amt_exempt",
      "tax_amt_pre_exemptions", "tax_amt_post_exemptions",
      "tax_amt_total_to_tif", "is_cps_agency", "tax_rate_for_cps",
      "tax_prop_for_cps", "tax_amt_rpm_tif_to_cps", "tax_amt_rpm_tif_to_rpm",
      "tax_amt_rpm_tif_back_to_jur_total", "tax_amt_rpm_tif_back_to_jur_dist",
      "tax_amt_rpm_tif_back_to_jur", "tax_amt_final"
    )
  )
  expect_equal(
    sum(is.na(tax_bill(years[1], pins[1], simplify = FALSE))),
    0
  )
})

test_that("returned amount/output correct for single PIN", {
  # District level tax amounts
  expect_equal(
    tax_bill(2019, pins[1], simplify = FALSE) %>%
      select(year, pin, agency, tax = tax_amt_final) %>%
      arrange(agency),
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
    tax_bill(sum_df$year, sum_df$pin, simplify = F) %>%
      nrow(),
    262
  )

  # District level tax amounts
  expect_equivalent(
    tax_bill(sum_df$year, sum_df$pin, simplify = F) %>%
      select(year, pin, agency, tax_amt_final) %>%
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

test_that("all differences are less than $25", {
  expect_true(
    left_join(
      tax_bill(sum_df$year, sum_df$pin, simplify = F) %>%
        select(year, pin, agency, tax_calc = tax_amt_final),
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
