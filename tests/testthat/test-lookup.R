context("test lookup_vec()")

##### TEST lookup_vec() #####

library(dplyr)

# Create a vector of PINs with known, correct lookup values
pins <- c(
  "14081020190000", "09274240240000", "07101010391078"
)
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
    lookup_equalization_factor(years),
    c(2.9160, 2.9160, 2.9109)
  )
  expect_equal(
    lookup_eav(years, pins),
    c(173834, 402402, 32826)
  )
  expect_equal(
    lookup_eav(years, pins[1]),
    c(173834, 173834, 173530)
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
  expect_equal(
    lookup_equalization_factor(c(2010, 2100, 2019)),
    c(3.300, NA, 2.916)
  )
  expect_equal(
    lookup_eav(c(2018, 2019, 2100), pins),
    c(173530, 402402, NA)
  )
  expect_equal(
    lookup_eav(2018:2019, c(pins[1], "34234342342333")),
    c(173530, NA)
  )
})

test_that("incorrect size inputs throw errors", {
  expect_error(lookup_tax_code(c(2018, 2019), pins))
  expect_error(lookup_tax_code(c(2018, 2018, 2019), c(pins[1], pins[2])))
  expect_error(lookup_eav(c(2018, 2019), pins))
  expect_error(lookup_eav(c(2018, 2018, 2019), c(pins[1], pins[2])))
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_tax_code("2019", pins[1]))
  expect_error(lookup_tax_code(2019, 14081020190000))
  expect_error(lookup_tax_code(c(2000, 2019), pins[1]))
  expect_error(lookup_tax_code(2019, c("1408102019000", pins[2])))
  expect_error(lookup_equalization_factor("2019"))
  expect_error(lookup_eav("2019", pins[1]))
  expect_error(lookup_eav(2019, 14081020190000))
  expect_error(lookup_eav(c(2000, 2019), pins[1]))
  expect_error(lookup_eav(2019, c("1408102019000", pins[2])))
})

test_that("functions return expected type", {
  expect_type(lookup_tax_code(2019, pins), "character")
  expect_type(lookup_equalization_factor(2019), "double")
  expect_type(lookup_eav(2019, pins), "double")
})

# Test that lookup values are correct against data from real tax bills
sum_df <- sample_tax_bills_summary
det_df <- sample_tax_bills_detail

test_that("test that lookup values are correct compared to real bill data", {
  expect_equal(
    lookup_tax_code(sum_df$year, sum_df$pin),
    sum_df$tax_code
  )
  expect_equal(
    lookup_eav(sum_df$year, sum_df$pin),
    sum_df$eav_total
  )
})


# Testing the lookups for functions that return data frames. These are harder to
# test since their output returns complex structured data/objects

context("test lookup_tif()")

##### TEST lookup_tif() #####

test_that("lookup values/data are correct", {
  expect_equal(
    lookup_tifs(2018:2019, c("70069", "73105"))$tif_share,
    c(0.676, 0.231),
    tolerance = 0.001
  )
  expect_equal(
    lookup_tifs(2018, c("70069", "73105"))$tif_share,
    c(0.676, 0.226),
    tolerance = 0.001
  )
  expect_equal(
    lookup_tifs(2014:2019, "73105")$tif_share,
    c(0.0438, 0.0798, 0.226, 0.231),
    tolerance = 0.001
  )
})

test_that("function returns expect data type/structure", {
  expect_s3_class(
    lookup_tifs(2018, "73105"),
    "data.frame"
  )
  expect_named(
    lookup_tifs(2018, c("70069", "73105")),
    c("year", "tax_code", "agency", "agency_name", "tif_share")
  )
  expect_equal(
    sum(is.na(lookup_tifs(sum_df$year, sum_df$tax_code))),
    0
  )
})

test_that("lookup returns 1 row for each tax code in a TIF", {
  expect_equal(
    nrow(lookup_tifs(sum_df$year, sum_df$tax_code)),
    sum_df %>%
      group_by(tax_code) %>%
      summarise(in_tif = sum(in_tif) > 0) %>%
      filter(in_tif) %>%
      as_tibble() %>%
      nrow()
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_tifs("2019", "73105"))
  expect_error(lookup_tifs(2019, "14081020190000"))
  expect_error(lookup_tifs(c(2000, 2019), "73105"))
  expect_error(lookup_tifs(2019, 23015))
})


context("test lookup_exemptions()")

##### TEST lookup_exemptions() #####

test_that("lookup values/data are correct", {
  expect_equal(
    lookup_exemptions(2019, pins)$exe_homeowner,
    c(10000, 100000, 10000)
  )
  expect_equal(
    lookup_exemptions(2014:2019, pins[2])$exe_senior,
    c(15000, 15000, 10000, 24000, 32000, 32000)
  )

  # Match all values in real data to lookup values
  expect_equivalent(
    lookup_exemptions(sum_df$year, sum_df$pin) %>%
      mutate(
        exe_vet_dis = exe_vet_dis_lt50 + exe_vet_dis_50_69 + exe_vet_dis_ge70,
        across(starts_with("exe_"), ~ .x != 0)
      ) %>%
      select(year, pin, exe_homeowner:exe_disabled, exe_vet_dis),
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
    lookup_exemptions(2018, pins),
    "data.frame"
  )
  expect_named(
    lookup_exemptions(2018:2019, pins[1]),
    c(
      "year", "pin", "exe_homeowner", "exe_senior", "exe_freeze",
      "exe_longtime_homeowner", "exe_disabled", "exe_vet_returning",
      "exe_vet_dis_lt50", "exe_vet_dis_50_69", "exe_vet_dis_ge70",
      "exe_abate"
    )
  )
  expect_equal(
    sum(is.na(lookup_exemptions(sum_df$year, sum_df$pin))),
    0
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_exemptions("2019", pins[1]))
  expect_error(lookup_exemptions(2019, "73105"))
  expect_error(lookup_exemptions(c(2000, 2019), pins[1]))
  expect_error(lookup_exemptions(2019, as.numeric(pins[1])))
})


context("test lookup_agency_eavs()")

##### TEST lookup_agency_eavs() #####

test_that("lookup values/data are correct", {
  expect_equal(
    lookup_agency_eavs(2019, "73105")$total_eav,
    c(
      rep(166917611547, 2),
      rep(87816177317, 3),
      87766563300,
      rep(87816177317, 3),
      164054703895
    )
  )
  expect_known_hash(
    lookup_agency_eavs(2014:2019, "12064"),
    "2b7347916b"
  )
  expect_known_hash(
    lookup_agency_eavs(sum_df$year, sum_df$tax_code),
    "4abffbaf3c"
  )
})

test_that("function returns expect data type/structure", {
  expect_s3_class(
    lookup_agency_eavs(2018, "73105"),
    "data.frame"
  )
  expect_named(
    lookup_agency_eavs(2018:2019, "73105"),
    c("year", "tax_code", "agency", "agency_name", "total_eav")
  )
  expect_equal(
    sum(is.na(lookup_agency_eavs(sum_df$year, sum_df$tax_code))),
    0
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_agency_eavs("2019", "73105"))
  expect_error(lookup_agency_eavs(2019, "06231050360000"))
  expect_error(lookup_agency_eavs(c(2000, 2019), "73105"))
  expect_error(lookup_agency_eavs(2019, 73105))
})


context("test lookup_levies()")

##### TEST lookup_levies() #####

test_that("lookup values/data are correct", {
  expect_equal(
    lookup_levies(2019, "73105")$total_levy,
    c(
      757805956, 98481391, 1407693322, 106257575, 148409340, 130772179,
      3178945619, 286280738, 0, 638172798
    )
  )
  expect_known_hash(
    lookup_levies(2014:2019, "12064"),
    "d3ae515c92"
  )
  expect_known_hash(
    lookup_levies(sum_df$year, sum_df$tax_code),
    "6de973da82"
  )
  expect_equal(
    nrow(lookup_agency_eavs(sum_df$year, sum_df$tax_code)),
    nrow(lookup_levies(sum_df$year, sum_df$tax_code))
  )
})

test_that("function returns expect data type/structure", {
  expect_s3_class(
    lookup_levies(2018, "73105"),
    "data.frame"
  )
  expect_named(
    lookup_levies(2018:2019, "73105"),
    c("year", "tax_code", "agency", "agency_name", "total_levy")
  )
  expect_equal(
    sum(is.na(lookup_levies(sum_df$year, sum_df$tax_code))),
    0
  )
})

test_that("bad/incorrect inputs throw errors", {
  expect_error(lookup_levies("2019", "73105"))
  expect_error(lookup_levies(2019, "06231050360000"))
  expect_error(lookup_levies(c(2000, 2019), "73105"))
  expect_error(lookup_levies(2019, 73105))
})
