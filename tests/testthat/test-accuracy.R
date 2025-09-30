context("test accuracy")

##### TEST accuracy #####

library(dplyr)

ptaxsim_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  Sys.getenv("PTAXSIM_DB_PATH")
)
assign("ptaxsim_db_conn", ptaxsim_db_conn, envir = .GlobalEnv)

# This test measures the accuracy of PTAXSIM be calculating the tax bill for a
# sample of PINs and comparing to the known tax bill amount. The goal is to be
# within $10 of each bill 97.5% of the time
pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "
  SELECT year, pin, tax_bill_total
  FROM pin
  ORDER BY random()
  LIMIT 1000000
  "
)

bills_raw <- tax_bill(pins$year, pins$pin)
bills_summ <- bills_raw %>%
  group_by(year, pin) %>%
  summarize(calced_bill = sum(final_tax)) %>%
  left_join(pins, by = c("year", "pin")) %>%
  rename(real_bill = tax_bill_total) %>%
  mutate(bill_diff = real_bill - calced_bill)

test_that("random sample of bills is >97.5% accurate", {
  expect_gte(
    sum(abs(bills_summ$bill_diff) < 10) / length(bills_summ$bill_diff),
    0.975
  )
})

test_that("no agency names are missing from the sample of bills", {
  expect_equal(
    sum(!is.na(bills_raw$agency_name)),
    nrow(bills_raw)
  )
})

test_that("total tax code revenue aligns with correct value", {
  transit_tif_pins <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    "
  SELECT pin
  FROM pin
  WHERE tax_code_num = '73103' AND year = 2023
  "
  ) %>% pull(pin)

  tax_code_rate <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    "
  SELECT DISTINCT tax_code_rate
  FROM tax_code
  WHERE tax_code_num = '73103' AND year = 2023
  "
  ) %>% pull(tax_code_rate)

  not_simp_bills <- tax_bill(2023, transit_tif_pins, simplify = FALSE)

  total_taxes <- not_simp_bills %>%
    distinct(pin, .keep_all = TRUE) %>%
    summarize(total_tax = sum(eav - exe_total) * tax_code_rate / 100)

  expect_equivalent(
    not_simp_bills %>%
      summarize(total_tax = sum(final_tax_to_tif +
        final_tax_to_dist - transit_tif_to_dist)),
    total_taxes,
    tolerance = 0.005
  )
})

DBI::dbDisconnect(ptaxsim_db_conn)
