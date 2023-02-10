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
  LIMIT 2000000
  "
)

bills <- tax_bill(pins$year, pins$pin) %>%
  group_by(year, pin) %>%
  summarize(calced_bill = sum(final_tax)) %>%
  left_join(pins, by = c("year", "pin")) %>%
  rename(real_bill = tax_bill_total) %>%
  mutate(bill_diff = real_bill - calced_bill)

test_that("random sample of bills is >97.5% accurate", {
  expect_gte(
    sum(abs(bills$bill_diff) < 10) / length(bills$bill_diff),
    0.975
  )
})

DBI::dbDisconnect(ptaxsim_db_conn)
