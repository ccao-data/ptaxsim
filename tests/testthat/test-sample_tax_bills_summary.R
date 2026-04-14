context("Test sample_tax_bills_summary")

##### TEST sample_tax_bills_summary #####

library(data.table)
library(dplyr)
library(here)
library(readr)
library(stringr)

test_that("sample_tax_bills_summary PIN-years match sample_tax_bill PDFs", {
  pdf_dir <- here("data-raw", "sample_tax_bills")
  if (!dir.exists(pdf_dir)) stop("PDF directory not found: ", pdf_dir)

  pdf_pin_years <- list.files(
    pdf_dir,
    pattern = "\\.pdf$",
    full.names = FALSE
  ) %>%
    # Assume filenames are structured like "<year>_<class>_<PIN>.pdf"
    tibble(filename = .) %>%
    mutate(
      year = as.integer(str_extract(filename, "^[0-9]{4}")),
      pin = str_extract(filename, "[0-9]{14}")
    ) %>%
    select(year, pin) %>%
    arrange(year, pin)

  summary_pin_years <- sample_tax_bills_summary %>%
    select(year, pin) %>%
    arrange(year, pin) %>%
    as_tibble()

  expect_equal(pdf_pin_years, summary_pin_years, ignore_attrs = TRUE)
})
