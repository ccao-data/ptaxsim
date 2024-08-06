context("test lookup_vec()")

##### TEST lookup_vec() #####

library(dplyr)
ptaxsim_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  Sys.getenv("PTAXSIM_DB_PATH")
)
assign("ptaxsim_db_conn", ptaxsim_db_conn, envir = .GlobalEnv)

max_year <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT data_year_max FROM metadata"
) %>%
  pull()

# Create a vector of PINs with known, correct lookup values
pins <- c("14081020190000", "09274240240000", "07101010391078")
pin10s <- substr(pins, 1, 10)
years <- c(2019, 2019, 2018)

test_that("test that lookup values are correct", {
  expect_equal(
    lookup_tax_code(years, pins),
    c("73105", "22031", "35011")
  )
  expect_equal(
    lookup_tax_code(years, pins[1]),
    c("73105", "73105", "73105")
  )
  expect_equal(
    lookup_tax_code(c(2018, 2019), pins),
    c("73105", "22031", "35011", "73105", "22031", "35011")
  )
})

test_that("returns NA for inputs without lookups", {
  expect_equal(
    lookup_tax_code(c(2018, 2019, 2100), pins),
    c("73105", "22031", NA)
  )
  expect_equal(
    lookup_tax_code(2018:2019, c(pins[1], "34234342342333")),
    c("73105", NA)
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_tax_code("2019", pins[1]))
  expect_error(lookup_tax_code(2019, 14081020190000))
  expect_error(lookup_tax_code(c(2000, 2019), pins[1]))
  expect_error(lookup_tax_code(2019, c("1408102019000", pins[2])))
})

test_that("functions return expected type", {
  expect_type(lookup_tax_code(2019, pins), "character")
})

# Test that lookup values are correct against data from real tax bills
sum_df <- sample_tax_bills_summary
det_df <- sample_tax_bills_detail

test_that("test that lookup values are correct compared to real bill data", {
  expect_equal(
    lookup_tax_code(sum_df$year, sum_df$pin),
    sum_df$tax_code
  )
})


# Testing the lookups for functions that return data frames. These are harder to
# test since their output returns complex structured data/objects

context("test lookup_tif()")

##### TEST lookup_tif() #####

test_that("lookup values/data are correct", {
  expect_equal(
    lookup_tif(2018:2019, c("70069", "73105"))$tif_share,
    c(0.676, 0.226, 0.676, 0.231),
    tolerance = 0.001
  )
  expect_equal(
    lookup_tif(2018, c("70069", "73105"))$tif_share,
    c(0.676, 0.226),
    tolerance = 0.001
  )
  expect_equal(
    lookup_tif(2014:2019, "73105")$tif_share,
    c(0.0438, 0.0798, 0.226, 0.231),
    tolerance = 0.001
  )
})

test_that("function returns expect data type/structure", {
  expect_s3_class(
    lookup_tif(2018, "73105"),
    c("data.frame", "data.table")
  )
  expect_named(
    lookup_tif(2018, c("70069", "73105")),
    c(
      "year", "tax_code", "agency_num", "agency_name",
      "agency_major_type", "agency_minor_type", "tif_share"
    )
  )
  expect_equal(
    sum(is.na(lookup_tif(sum_df$year, sum_df$tax_code))),
    0
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_tif("2019", "73105"))
  expect_error(lookup_tif(2019, "14081020190000"))
  expect_error(lookup_tif(c(2000, 2019), "73105"))
  expect_error(lookup_tif(2019, 23015))
})


context("test lookup_pin()")

##### TEST lookup_pin() #####

test_that("lookup values/data are correct", {
  expect_equal(
    lookup_pin(2019, pins)$exe_homeowner,
    c(10000, 100000, 10000)
  )
  expect_equal(
    lookup_pin(2014:2021, pins[2])$exe_senior,
    c(15000, 15000, 10000, 24000, 32000, 32000, 32000, 32000)
  )
  expect_equal(
    lookup_pin(2016:2021, pins[2], stage = "mailed")$av,
    c(171414, 106400, 106400, 163489, 137998, 137998)
  )
  expect_equal(
    lookup_pin(2016:2021, pins[2], stage = "board")$av,
    c(106400, 106400, 106400, 137998, 137998, 137998)
  )
  expect_equal(
    lookup_pin(
      2016:2020, pins[2],
      stage = "board",
      eq_version = "tentative"
    )$av,
    c(106400, 106400, 106400, 137998, 137998)
  )
  expect_equal(
    lookup_pin(2016:2020, pins[2], eq_version = "tentative")$eav,
    c(292121, 309454, 301814, 379812, 425876)
  )

  # Match all values in real data to lookup values
  expect_equivalent(
    lookup_pin(2018:max_year, sum_df$pin) %>%
      mutate(
        exe_vet_dis = exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70,
        across(starts_with("exe_"), ~ .x != 0)
      ) %>%
      select(year, pin, exe_homeowner:exe_disabled, exe_vet_dis) %>%
      semi_join(sum_df, by = c("year", "pin")),
    sum_df %>%
      select(
        year, pin, exe_homeowner:exe_disabled,
        exe_vet_dis = exe_vet_disabled
      ) %>%
      as_tibble()
  )
})

test_that("function returns expect data type/structure", {
  expect_s3_class(
    lookup_pin(2018, pins),
    c("data.frame", "data.table")
  )
  expect_named(
    lookup_pin(2018:2019, pins[1]),
    c(
      "year", "pin", "class", "av", "eav", "exe_homeowner", "exe_senior",
      "exe_freeze", "exe_longtime_homeowner", "exe_disabled",
      "exe_vet_returning", "exe_vet_dis_lt50", "exe_vet_dis_50_69",
      "exe_vet_dis_ge70", "exe_abate"
    )
  )
  expect_equal(
    sum(is.na(lookup_pin(sum_df$year, sum_df$pin))),
    0
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_pin("2019", pins[1]))
  expect_error(lookup_pin(2019, pins[1], stage = 1))
  expect_error(lookup_pin(2019, pins[1], stage = "mail"))
  expect_error(lookup_pin(2019, "73105"))
  expect_error(lookup_pin(c(2000, 2019), pins[1]))
  expect_error(lookup_pin(2019, as.numeric(pins[1])))
  expect_error(lookup_pin(2019, pins[1], eq_version = "BOARD"))
  expect_error(lookup_pin(2019, pins[1], eq_version = 1))
})


context("test lookup_agency()")

##### TEST lookup_agency() #####

test_that("lookup values/data are correct", {
  expect_equal(
    lookup_agency(2019, "73105")$agency_total_eav,
    c(
      rep(166917611547, 2),
      rep(87816177317, 3),
      87766563300,
      rep(87816177317, 3),
      164054703895
    )
  )
  expect_equal(
    lookup_agency(2019, "73105")$agency_total_ext,
    c(
      757805956, 98481391, 1407693322, 106257575, 148409340, 130772179,
      3178945619, 286280738, 0, 638172798
    )
  )
  expect_known_hash(
    lookup_agency(2014:2019, "12064"),
    "cf6dcb93bf"
  )
  expect_known_hash(
    lookup_agency(sum_df$year, sum_df$tax_code),
    "30ede4ede0"
  )
})

test_that("function returns expect data type/structure", {
  expect_s3_class(
    lookup_agency(2018, "73105"),
    c("data.frame", "data.table")
  )
  expect_named(
    lookup_agency(2018:2019, "73105"),
    c(
      "year", "tax_code", "agency_num", "agency_name", "agency_major_type",
      "agency_minor_type", "agency_total_eav", "agency_total_ext"
    )
  )
  expect_equal(
    sum(is.na(lookup_agency(sum_df$year, sum_df$tax_code))),
    0
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_agency("2019", "73105"))
  expect_error(lookup_agency(2019, "06231050360000"))
  expect_error(lookup_agency(c(2000, 2019), "73105"))
  expect_error(lookup_agency(2019, 73105))
})


context("test lookup_pin10_geometry()")

##### TEST lookup_pin10_geometry() #####

test_that("lookup values/data are correct", {
  expect_equal(nrow(lookup_pin10_geometry(2019, pin10s)), 3)
  expect_equal(nrow(lookup_pin10_geometry(2019:2020, pin10s)), 6)
  expect_equal(nrow(lookup_pin10_geometry(c(2100, 2021), pin10s)), 3)
  # Returns parsed geoms
  expect_equal(
    lookup_pin10_geometry(2006:max_year, pin10s) %>%
      sf::st_as_sf(wkt = "geometry", crs = 4326) %>%
      sf::st_is_empty() %>%
      sum(),
    0
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_pin10_geometry("2019", "73105"))
  expect_error(
    lookup_pin10_geometry(2019, "06231050360000"),
    "^Must enter 10-digit PINs"
  )
  expect_error(lookup_pin10_geometry(2020:2019, "062310503600"))
  expect_error(lookup_pin10_geometry(2019, 06231050360000))
})

DBI::dbDisconnect(ptaxsim_db_conn)
