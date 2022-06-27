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
# within $10 of each bill 99% of the time

pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "
  SELECT year, pin, tax_bill_total
  FROM (
    SELECT
      year,
      pin,
      tax_bill_total,
      row_number() OVER (PARTITION BY year ORDER BY random()) AS row_num
    FROM pins)
  WHERE row_num <= 10000
  "
)

bills <- tax_bill(pins$year, pins$pin) %>%
  group_by(year, pin) %>%
  summarize(calced_bill = sum(tax_amt_total_to_tif) + sum(tax_amt_final)) %>%
  left_join(pins, on = c("year", "pin")) %>%
  rename(real_bill = tax_bill_total) %>%
  mutate(bill_diff = real_bill - calced_bill)

test_that("random sample of bills is >95% accurate", {
  expect_gte(
    sum(abs(bills$bill_diff) < 10) / length(bills$bill_diff),
    0.95
  )
})

DBI::dbDisconnect(ptaxsim_db_conn)
