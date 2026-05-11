##### TEST tax_bill() #####

library(data.table)
library(dplyr)
library(stringr)

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
years <- 2024:2022

test_that("bad/incorrect vector inputs throw errors", {
  expect_error(tax_bill("2024", pins[1]))
  expect_error(tax_bill(numeric(0), pins[1]))
  expect_error(tax_bill(years[1], 14081020190000))
  expect_error(tax_bill(c(2000, years[1]), pins[1]))
  expect_error(tax_bill(years[1], c("1408102019000", pins[2])))
  expect_error(tax_bill(years[1], pins[1], tax_code_vec = 73105))
  expect_error(tax_bill(years[1], pins[1], tax_code_vec = "5454"))
  expect_error(tax_bill(years[1], pins[2], simplify = "yes"))
  expect_error(tax_bill(years[2:1], pins[2:3], simplify = c(TRUE, FALSE)))
})

test_that("bad/incorrect data frame inputs throw errors", {
  expect_error(tax_bill(years[1], pins[1], pin_dt = c(23000, 959051)))
  expect_error(tax_bill(years[1], pins[1], agency_dt = c("3232", "TIF")))
  expect_error(tax_bill(years[1], pins[1], tif_dt = c(23000, 959051)))
  expect_error(tax_bill(years[1], pins[1], pin_tif_dt = c(23000, 959051)))
})

test_that("non-unique values to main args throws error", {
  expect_error(tax_bill(years[1], pins[c(1, 1)]))
  expect_error(tax_bill(c(years[c(1, 1)]), pins[1]))
  expect_error(
    tax_bill(years[c(1, 1)], pins[c(1, 2)], tax_code_vec = c("73105"))
  )
})

test_that("incorrect size inputs throw errors", {
  expect_error(tax_bill(years, pins, tax_code_vec = c("73105", "73105")))
  expect_error(tax_bill(years, pins, tax_code_vec = c("73105", "73105")))
})

test_that("data frame inputs throw errors when required cols missing", {
  expect_error(tax_bill(years[1], pins[1], pin_dt = data.table()))
  expect_error(tax_bill(years[1], pins[1], agency_dt = data.table()))
  expect_error(tax_bill(years[1], pins[1], tif_dt = data.table()))
  expect_error(tax_bill(years[1], pins[1], pin_tif_dt = data.table()))
})

test_that("function returns expected data type/structure", {
  expect_s3_class(tax_bill(years[1], pins[1]), "data.frame")
  expect_s3_class(tax_bill(years[1], pins[1]), "data.table")
  expect_equal(
    key(tax_bill(years[1], pins[1])),
    c("year", "pin", "agency_num")
  )
  expect_named(
    tax_bill(years[1:2], pins[1:2]),
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
    tax_bill(years[1:2], pins[1:2], simplify = FALSE),
    c(
      "year", "pin", "class", "tax_code", "av", "eav", "exe_total",
      "agency_num", "agency_name", "agency_major_type", "agency_minor_type",
      "agency_total_ext", "agency_total_eav", "agency_tax_rate",
      "tax_amt_exe", "tax_amt_pre_exe", "tax_amt_post_exe", "tif_agency_num",
      "tif_agency_name", "tif_share",
      "transit_tif_to_cps", "transit_tif_to_tif",
      "transit_tif_to_dist", "final_tax_to_tif", "final_tax_to_dist"
    )
  )
  expect_equal(
    sum(is.na(tax_bill(years[1], pins[1], simplify = FALSE))),
    0
  )
  expect_equal(dim(tax_bill(years[1], pins[1], simplify = TRUE)), c(11, 12))
  expect_equal(dim(tax_bill(years[1], pins[1], simplify = FALSE)), c(9, 25))
})

test_that("returned amount/output correct for single PIN", {
  # District level tax amounts
  expect_equal(
    tax_bill(2018, pins[3], simplify = TRUE) %>%
      select(year, pin, agency_num, final_tax) %>%
      arrange(agency_num) %>%
      as_tibble(),
    det_dt %>%
      filter(pin == pins[3]) %>%
      select(year, pin, agency_num, final_tax) %>%
      arrange(agency_num) %>%
      as_tibble(),
    tolerance = 0.005,
    ignore_attr = TRUE
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
exclude_pins <- c(
  # Bill lists different pre- and post-exemption amounts, but does not show
  # any exemptions, and the Clerk data also has no exemptions
  "20031180060000",
  # Has a 2022 vetdis 100% exemption on the Treasurer bill that is not present
  # in the Clerk data because our Clerk data export did not include vetdis 100%
  # exemptions in 2022
  "10252080490000"
)
sum_dt_filtered <- sum_dt %>%
  filter(!pin %in% exclude_pins) %>%
  as_tibble()
det_dt_filtered <- det_dt %>%
  filter(!pin %in% exclude_pins) %>%
  as_tibble()

sum_tax_bill_simplified <- tax_bill(
  sum_dt_filtered$year, sum_dt_filtered$pin,
  simplify = TRUE
)
sum_tax_bill_unsimplified <- tax_bill(
  sum_dt_filtered$year, sum_dt_filtered$pin,
  simplify = FALSE
)

# Get a list of PINs that are in transit TIFs. We exclude these from our
# bill summary tests, because the Treasurer's method for calculating transit
# TIF distributions does not match our method
transit_tif_pins <- sum_tax_bill_simplified %>%
  select(year, pin, agency_num, final_tax) %>%
  count(year, pin, agency_num) %>%
  # PINs in transit TIFs will have two line items with the CPS agency number,
  # one being the TIF distribution
  filter(n > 1) %>%
  pull(pin)

# Helper function to roll up funds and subagencies into their parent
# agency for the purposes of reporting tax bill totals. This is useful
# because the Clerk and the Treasurer do not fully agree on which funds
# and subagencies should get their own line items starting in 2024, so we
# want to ignore those differences and make sure the overall totals match
# at the level of parent agencies
rollup_agencies <- function(df) {
  df %>%
    # Filter out PINs in transit TIFs because we already know that the
    # Treasurer calculates agency distributions differently than we do
    filter(!pin %in% transit_tif_pins) %>%
    # Filter out agencies with $0 bill amounts, since they seem to be
    # especially susceptible to being left off of one side of the comparison
    filter(final_tax > 0L) %>%
    # Order by agency num so that we can always be sure that the parent agency
    # number (which ends in 0) will get used as the final `agency_num`
    # for the group
    arrange(year, pin, agency_num) %>%
    # Group by parent agency number
    mutate(agency_num = substr(agency_num, 1, 8)) %>%
    group_by(year, pin, agency_num) %>%
    summarize(
      agency_name = first(agency_name),
      final_tax = sum(final_tax, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Convert output to dataframe because it is the simplest possible data
    # structure for the purposes of comparison
    as.data.frame()
}

test_that("returns correct row counts for all sample bills", {
  expect_equal(
    sum_tax_bill_simplified %>% rollup_agencies() %>% nrow(),
    det_dt_filtered %>% rollup_agencies() %>% nrow()
  )
  expect_equal(
    nrow(sum_tax_bill_unsimplified),
    sum_tax_bill_simplified %>%
      filter(!str_detect(agency_name, "TIF")) %>%
      nrow()
  )
})

test_that("amounts correct for non-transit-TIF sample bills", {
  # Make sure agency-level tax amounts match. Roll them up to the parent
  # agency to account for differences in fund reporting between the Clerk and
  # the Treasurer
  all_bills_actual <- rollup_agencies(sum_tax_bill_simplified)
  all_bills_expected <- rollup_agencies(det_dt_filtered)
  # Get a rich diff of mismatches to make it easier to debug differences in
  # case of test failure
  bill_mismatches <- all_bills_expected %>%
    full_join(
      all_bills_actual,
      by = c("year", "pin", "agency_num"),
      suffix = c("_expected", "_actual")
    ) %>%
    mutate(
      agency_name = if_else(
        !is.na(agency_name_expected),
        agency_name_expected,
        agency_name_actual
      ),
      chk_agency_not_in_expected = is.na(final_tax_expected),
      chk_agency_not_in_actual = is.na(final_tax_actual),
      chk_final_tax_mismatch = (
        !is.na(final_tax_expected) &
          !is.na(final_tax_actual) &
          # Check for absolute differences greater than one cent (we expect
          # occasional differences of one cent due to rounding)
          round(abs(final_tax_expected - final_tax_actual), 2) > 0.01 &
          # Check for relative differences greater than 0.1% of the expected
          # value
          round(
            abs(final_tax_expected - final_tax_actual) / final_tax_expected,
            3
          ) > 0.001
      )
    ) %>%
    select(-agency_name_expected, -agency_name_actual) %>%
    relocate(agency_name, .after = agency_num) %>%
    filter(if_any(starts_with("chk_"), ~ .x == TRUE))

  expect_equal(nrow(bill_mismatches), 0)
})

test_that(
  "all diffs are less than $25 for non-transit-TIF sample bills pre-2024",
  {
    # Exclude certain PINs in the RPM TIF or with extremely high bills.
    # Will run separate tests for these
    sum_dt_for_test <- sum_dt_filtered %>%
      filter(
        # Very large bill with slightly more than $25 difference, but
        # the percent difference is not worrisome
        pin != "01363010130000",
        !pin %in% transit_tif_pins,
        # Filter before 2024, since the Clerk and Treasurer agencies don't
        # match up exactly starting in 2024
        year < 2024
      )

    all_bills_actual <- tax_bill(sum_dt_for_test$year, sum_dt_for_test$pin) %>%
      select(year, pin, agency_num, agency_name, final_tax_actual = final_tax)
    bill_mismatches <- all_bills_actual %>%
      left_join(
        det_dt_filtered %>%
          select(year, pin, agency_num, final_tax_expected = final_tax) %>%
          as_tibble(),
        by = c("year", "pin", "agency_num")
      ) %>%
      mutate(diff = abs(final_tax_expected - final_tax_actual)) %>%
      filter(diff >= 25)

    expect_equal(nrow(bill_mismatches), 0)
  }
)

test_that(
  "all diffs are less than $25 for non-transit-TIF sample bills post-2024",
  {
    # Exclude certain PINs in the RPM TIF.
    # Will run separate tests for these
    sum_dt_for_test <- sum_dt_filtered %>%
      filter(
        !pin %in% transit_tif_pins,
        # Filter after 2024 so that we can test bills whose agencies don't
        # match up exactly with the Clerk data
        year >= 2024
      )

    all_bills_actual <- tax_bill(sum_dt_for_test$year, sum_dt_for_test$pin) %>%
      rollup_agencies() %>%
      select(year, pin, agency_num, agency_name, final_tax_actual = final_tax)
    all_bills_expected <- det_dt_filtered %>%
      rollup_agencies() %>%
      select(year, pin, agency_num, final_tax_expected = final_tax) %>%
      as_tibble()
    bill_mismatches <- all_bills_actual %>%
      left_join(
        all_bills_expected,
        by = c("year", "pin", "agency_num")
      ) %>%
      mutate(diff = abs(final_tax_expected - final_tax_actual)) %>%
      filter(diff >= 25)

    expect_equal(nrow(bill_mismatches), 0)
  }
)

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
  rpm_tif_pins <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    "
  SELECT pin
  FROM pin
  WHERE tax_code_num = '73105' AND year = 2023
  "
  ) %>%
    slice_sample(n = 100) %>%
    pull(pin)

  simp_bills <- tax_bill(2023, rpm_tif_pins, simplify = TRUE)
  not_simp_bills <- tax_bill(2023, rpm_tif_pins, simplify = FALSE)

expect_equal(
  simp_bills %>% summarize(total_tax = sum(final_tax)),
  not_simp_bills %>%
    summarize(total_tax = sum(final_tax_to_tif +
      final_tax_to_dist - transit_tif_to_dist)),
  tolerance = 0.005,
  ignore_attr = TRUE
)
})

test_that("PINs with EAV <= $150 have $0 tax bill", {
  eav_lt_150_pins <-
    DBI::dbGetQuery(
      ptaxsim_db_conn,
      "
  SELECT
  pin, av_board, eq_factor_final, a.year
  FROM pin a
  LEFT JOIN eq_factor b on a.year = b.year
  WHERE av_board < 1000
  AND av_board > 0
  "
    ) %>%
    mutate(
      eav = av_board * eq_factor_final,
      exe_total = rowSums(across(starts_with("exe_")))
    ) %>%
    filter(eav - exe_total < 150) %>%
    distinct(pin)

  eav_lt_150_bills <- tax_bill(2006:2024, eav_lt_150_pins$pin) %>%
    mutate(exe_total = rowSums(across(starts_with("exe_")))) %>%
    filter(eav - exe_total < 150)

  expect_equal(sum(eav_lt_150_bills$final_tax), 0)
})

DBI::dbDisconnect(ptaxsim_db_conn)
