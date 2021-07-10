#' Lookup data frames required to calculate property tax bills
#'
#' @description The functions in this group take a tax year, PIN, and/or tax
#'   code as input and return a data frame as an output. The returned data
#'   frames are in a specific format (in terms of column name/order) and can be
#'   used directly in \code{\link{tax_bill}}.
#'
#'   When calculating tax bills for future years or providing custom/simulated
#'   data, you'll need to replicate the format returned by these functions.
#'
#'   Note: All vector inputs must be either the same length or length 1.
#'
#' @param year A numeric vector of tax years.
#' @param pin A character vector of 14-digit PINs.
#' @param tax_code A character vector of 5-digit Cook County tax codes.
#'
#' @return A data frame containing the looked-up values.
#'
#' @examples
#'
#' lookup_agency_eavs(2019, "73105")
#' lookup_exemptions(2018:2019, "20304190020000")
#' lookup_levies(2019, "73105")
#' lookup_tifs(2019, "73105")
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family lookups
#' @name lookup_df
NULL


#' Lookup values required to calculate property tax bills
#'
#' @description The functions in this group take a tax year and PIN as input and
#'   return a vector as an output. The output vector can be used directly in
#'   \code{\link{tax_bill}}.
#'
#'   Note: All vector inputs must be either the same length or length 1.
#'
#' @param year A numeric vector of tax years.
#' @param pin A character vector of 14-digit PINs.
#'
#' @return A vector containing the looked-up values. Will be the same length as
#'   the longest vector in the inputs.
#'
#' @examples
#'
#' lookup_eav(2017:2019, "20304190020000")
#' lookup_equalization_factor(2014:2019)
#' lookup_tax_code(2019, c("20304190020000", "16152090350000"))
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family lookups
#' @name lookup_vec
NULL


#' @describeIn lookup_df Lookup tax agency (CPS, Cook County, forest preserve)
#'   total equalized assessed values. Such values are known as the "base" and
#'   are used to calculate per agency tax rates. Returns a data frame with 1 row
#'   for each agency associated with the input tax code and year.
#'
#' @export
lookup_agency_eavs <- function(year, tax_code) {
  stopifnot(
    is.numeric(year),
    is.character(tax_code),
    all(year >= 2014),
    all(nchar(tax_code) == 5)
  )

  # Convert input vectors into data frame. Use tibble() first to enforce
  # comparable sizes
  df_idx <- data.table::setDT(dplyr::tibble(
    "year" = year,
    "tax_code" = tax_code
  ))

  dtplyr::lazy_dt(df_idx) %>%
    dplyr::left_join(
      ptaxsim::tax_codes_by_agency,
      by = c("year", "tax_code")
    ) %>%
    dplyr::left_join(
      ptaxsim::levy_and_total_eav_by_agency,
      by = c("year", "agency")
    ) %>%
    dplyr::anti_join(
      ptaxsim::tifs,
      by = c("year", "agency", "tax_code")
    ) %>%
    dplyr::distinct(
      .data$year, .data$tax_code, .data$agency,
      .data$agency_name, .data$total_eav
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(!is.na(.data$total_eav))
}


#' @describeIn lookup_vec Lookup the equalized assessed value for a specific PIN
#'   and year. Returns a numeric vector the same length as the longest input.
#'
#' @export
lookup_eav <- function(year, pin) {
  stopifnot(
    is.numeric(year),
    is.character(pin),
    all(year >= 2014),
    all(nchar(pin) == 14)
  )

  # Convert input vectors into data frame. Use tibble() first to enforce
  # comparable sizes
  df_idx <- data.table::setDT(dplyr::tibble(
    "year" = year,
    "pin" = pin
  ))

  dtplyr::lazy_dt(df_idx) %>%
    dplyr::left_join(
      ptaxsim::av_exe_and_tax_code_by_pin %>%
        dplyr::select(.data$year, .data$pin, .data$av),
      by = c("year", "pin")
    ) %>%
    dplyr::left_join(
      ptaxsim::equalization_factors,
      by = "year"
    ) %>%
    dplyr::mutate(
      eav_no_exemptions = round(.data$av * .data$equalization_factor)
    ) %>%
    dplyr::pull(.data$eav_no_exemptions)
}


#' @describeIn lookup_vec Lookup the state equalization rate for a given year.
#'   Returns a numeric vector the same length as the input.
#'
#' @export
lookup_equalization_factor <- function(year) {
  stopifnot(
    is.numeric(year),
    is.vector(year)
  )

  ptaxsim::equalization_factors$equalization_factor[
    match(year, ptaxsim::equalization_factors$year)
  ]
}


#' @describeIn lookup_df Lookup exemptions granted to a PIN for the specified
#'   year. Returns a data frame with 1 row per PIN per year.
#'
#' @export
lookup_exemptions <- function(year, pin) {
  stopifnot(
    is.numeric(year),
    is.character(pin),
    all(year >= 2014),
    all(nchar(pin) == 14)
  )

  # Convert input vectors into data frame. Use tibble() first to enforce
  # comparable sizes
  df_idx <- data.table::setDT(dplyr::tibble(
    "year" = year,
    "pin" = pin
  ))

  dtplyr::lazy_dt(df_idx) %>%
    dplyr::left_join(
      ptaxsim::av_exe_and_tax_code_by_pin,
      by = c("year", "pin")
    ) %>%
    dplyr::select(.data$year, .data$pin, dplyr::starts_with("exe_")) %>%
    dplyr::distinct() %>%
    dplyr::as_tibble()
}


#' @describeIn lookup_df Lookup tax agency (CPS, Cook County, forest preserve)
#'   total levies. Returns a data frame with 1 row for each agency associated
#'   with the input tax code and year.
#'
#' @export
lookup_levies <- function(year, tax_code) {
  stopifnot(
    is.numeric(year),
    is.character(tax_code),
    all(year >= 2014),
    all(nchar(tax_code) == 5)
  )

  # Convert input vectors into data frame. Use tibble() first to enforce
  # comparable sizes
  df_idx <- data.table::setDT(dplyr::tibble(
    "year" = year,
    "tax_code" = tax_code
  ))

  dtplyr::lazy_dt(df_idx) %>%
    dplyr::left_join(
      ptaxsim::tax_codes_by_agency,
      by = c("year", "tax_code")
    ) %>%
    dplyr::left_join(
      ptaxsim::levy_and_total_eav_by_agency,
      by = c("year", "agency")
    ) %>%
    dplyr::anti_join(
      ptaxsim::tifs,
      by = c("year", "agency", "tax_code")
    ) %>%
    dplyr::distinct(
      .data$year, .data$tax_code, .data$agency,
      .data$agency_name, .data$total_levy
    ) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(!is.na(.data$total_levy))
}


#' @describeIn lookup_vec Lookup 5-digit Cook County tax code by PIN and year.
#'   Returns a character vector the same length as the longest input.
#'
#' @export
lookup_tax_code <- function(year, pin) {
  stopifnot(
    is.numeric(year),
    is.character(pin),
    all(year >= 2014),
    all(nchar(pin) == 14)
  )

  # Convert input vectors into data frame. Use tibble() first to enforce
  # comparable sizes
  df_idx <- data.table::setDT(dplyr::tibble(
    "year" = year,
    "pin" = pin
  ))

  dtplyr::lazy_dt(df_idx) %>%
    dplyr::left_join(
      ptaxsim::av_exe_and_tax_code_by_pin %>%
        dplyr::select(.data$year, .data$pin, .data$tax_code),
      by = c("year", "pin")
    ) %>%
    dplyr::pull(.data$tax_code) %>%
    as.character()
}


#' @describeIn lookup_df Lookup any TIFs that apply to a given tax code and
#'   year. Returns a data frame with only 1 row per tax code and year, or 0 rows
#'   if the tax code is not within a TIF.
#'
#' @export
lookup_tifs <- function(year, tax_code) {
  stopifnot(
    is.numeric(year),
    is.character(tax_code),
    all(year >= 2014),
    all(nchar(tax_code) == 5)
  )

  # Convert input vectors into data frame. Use tibble() first to enforce
  # comparable sizes
  df_idx <- data.table::setDT(dplyr::tibble(
    "year" = year,
    "tax_code" = tax_code
  ))

  dtplyr::lazy_dt(df_idx) %>%
    dplyr::left_join(
      ptaxsim::tax_codes_by_agency,
      by = c("year", "tax_code")
    ) %>%
    dplyr::left_join(
      ptaxsim::tifs,
      by = c("year", "agency", "tax_code")
    ) %>%
    dplyr::anti_join(
      ptaxsim::levy_and_total_eav_by_agency,
      by = c("year", "agency")
    ) %>%
    dplyr::distinct(
      .data$year, .data$tax_code, .data$agency,
      agency_name = .data$tif_name,
      tif_share = .data$tax_code_distribution_percent
    ) %>%
    dplyr::mutate(tif_share = .data$tif_share / 100) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(!is.na(.data$tif_share))
}
