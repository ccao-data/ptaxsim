globalVariables(c("ptaxsim_db_conn", "."))

#' Lookup data frames required to calculate property tax bills
#'
#' @description The functions in this group take a tax year, PIN, and/or tax
#'   code as input and return a data frame as an output. The returned data
#'   frames are in a specific format (in terms of column name/type/order) and
#'   can be used directly in \code{\link{tax_bill}} or piped to helper
#'   functions.
#'
#'   When calculating tax bills for future years or providing custom/simulated
#'   data, you'll need to replicate the format returned by these functions.
#'
#' @param year A numeric vector of tax years.
#' @param pin A character vector of 14-digit PINs.
#' @param tax_code A character vector of 5-digit Cook County tax codes.
#' @param conn A connection object pointing to the local copy of the
#'   PTAXSIM database. Usually instantiated on package load.
#'
#' @return A data frame containing the specified tax data by year and
#'   PIN/tax code.
#'
#' @examples
#' \dontrun{
#' lookup_agency(2019, "73105")
#' lookup_pin(2018:2019, "20304190020000")
#' lookup_tif(2019, "73105")
#' }
#' @md
#' @family lookups
#' @name lookup_df
NULL


#' Lookup values required to calculate property tax bills
#'
#' @description The functions in this group take a tax year and PIN as input and
#'   return a vector as an output. The output vector can be used directly in
#'   \code{\link{tax_bill}}.
#'
#' @param year A numeric vector of tax years.
#' @param pin A character vector of 14-digit PINs.
#' @param conn A connection object pointing to the local copy of the
#'   PTAXSIM database. Usually instantiated on package load.
#'
#' @return A vector containing the specified tax data values.
#'
#' @examples
#' \dontrun{
#' lookup_tax_code(2019, c("20304190020000", "16152090350000"))
#' lookup_tax_code(2006:2020, c("20304190020000", "16152090350000"))
#' }
#' @md
#' @family lookups
#' @name lookup_vec
NULL


#' @describeIn lookup_df Lookup tax agency (CPS, Cook County, forest preserve)
#'   total equalized assessed values. Such values are known as the "base" and
#'   are used to calculate per agency tax rates. Also returns each agency's
#'   levy amount. Returns a data frame with 1 row for each agency associated
#'   with the input tax code and year.
#'
#' @export
lookup_agency <- function(year, tax_code, conn = ptaxsim_db_conn) {
  stopifnot(
    is.numeric(year),
    is.character(tax_code),
    all(year >= 2006),
    all(nchar(tax_code) == 5 | is.na(tax_code)),
    DBI::dbIsValid(conn)
  )

  df <- DBI::dbGetQuery(
    conn,
    statement = glue::glue_sql(
      "
      SELECT
        tc.year,
        tc.tax_code_num AS tax_code,
        tc.agency_num,
        a.agency_name,
        a.total_ext AS agency_total_ext,
        a.total_eav AS agency_total_eav
      FROM tax_codes tc
      LEFT JOIN agencies a
        ON tc.year = a.year
        AND tc.agency_num = a.agency_num
      WHERE tc.year IN ({years*})
      AND tc.tax_code_num IN ({tax_codes*})
      AND a.total_eav IS NOT NULL
      ORDER BY tc.year, tc.tax_code_num, tc.agency_num
    ",
      years = unique(year),
      tax_codes = unique(tax_code),
      .con = conn
    )
  )

  df <- data.table::setDT(df, key = c("year", "tax_code", "agency_num"))

  return(df)
}


#' @describeIn lookup_df Lookup the equalized assessed value and exemptions
#'   for a specific PIN and year. Returns a numeric vector the same length
#'   as the longest input.
#'
#' @export
lookup_pin <- function(year, pin, conn = ptaxsim_db_conn) {
  stopifnot(
    is.numeric(year),
    is.character(pin),
    all(year >= 2006),
    all(nchar(pin) == 14),
    DBI::dbIsValid(conn)
  )

  # This lookup uses a temp table since it's faster than putting lots of
  # values into the WHERE clause for large lookups
  df_idx <- expand.grid("year" = unique(year), "pin" = unique(pin))
  DBI::dbWriteTable(
    conn = conn,
    name = "lookup_pin",
    value = df_idx,
    overwrite = TRUE,
    temporary = TRUE
  )

  df <- DBI::dbGetQuery(
    conn,
    statement = glue::glue_sql(
      "
      SELECT
        lp.year,
        lp.pin,
        p.class,
        p.av,
        CAST(ROUND(p.av * ef.equalization_factor, 0) AS int) AS eav,
        p.exe_homeowner,
        p.exe_senior,
        p.exe_freeze,
        p.exe_longtime_homeowner,
        p.exe_disabled,
        p.exe_vet_returning,
        p.exe_vet_dis_lt50,
        p.exe_vet_dis_50_69,
        p.exe_vet_dis_ge70,
        p.exe_abate
      FROM lookup_pin lp
      INNER JOIN pins p
        ON lp.year = p.year
        AND lp.pin = p.pin
      LEFT JOIN eq_factors ef
        ON p.year = ef.year
      ORDER BY lp.year, lp.pin
    ",
      pins = pin,
      years = year,
      .con = conn
    )
  )

  df <- data.table::setDT(df, key = c("year", "pin"))

  return(df)
}


#' @describeIn lookup_vec Lookup Cook County tax code for a specific PIN and
#'   year. The tax code represents the unique geographic overlap of different
#'   taxing districts. If the input vectors are both length N, then the output
#'   will also be length N. If the input vectors are different lengths, the
#'   Cartesian product is returned.
#'
#' @export
lookup_tax_code <- function(year, pin, conn = ptaxsim_db_conn) {
  stopifnot(
    is.numeric(year),
    is.character(pin),
    all(year >= 2006),
    all(nchar(pin) == 14),
    DBI::dbIsValid(conn)
  )

  if (length(year) != length(pin)) {
    df_idx <- expand.grid("year" = year, "pin" = pin)
  } else {
    df_idx <- data.frame("year" = year, "pin" = pin)
  }

  DBI::dbWriteTable(
    conn = conn,
    name = "lookup_tax_code",
    value = df_idx,
    overwrite = TRUE,
    temporary = TRUE
  )

  df <- DBI::dbGetQuery(
    conn,
    statement = glue::glue_sql(
      "
      SELECT
        p.tax_code_num
      FROM lookup_tax_code ltc
      LEFT JOIN pins p
        ON p.year = ltc.year
        AND p.pin = ltc.pin
    ",
      pins = pin,
      years = year,
      .con = conn
    )
  )

  return(df$tax_code_num)
}


#' @describeIn lookup_df Lookup any TIFs that apply to a given tax code and
#'   year. Returns a data frame with only 1 row per tax code and year, or 0 rows
#'   if the tax code is not within a TIF.
#'
#' @export
lookup_tif <- function(year, tax_code, conn = ptaxsim_db_conn) {
  stopifnot(
    is.numeric(year),
    is.character(tax_code),
    all(year >= 2006),
    all(nchar(tax_code) == 5 | is.na(tax_code)),
    DBI::dbIsValid(conn)
  )

  tif_share <- NULL
  df <- DBI::dbGetQuery(
    conn,
    statement = glue::glue_sql("
      SELECT
        td.year,
        td.tax_code_num AS tax_code,
        td.agency_num,
        ts.tif_name AS agency_name,
        td.tax_code_distribution_percent / 100 AS tif_share
      FROM tif_distributions td
      LEFT JOIN tif_summaries ts
        ON td.year = ts.year
        AND td.agency_num = ts.agency_num
      WHERE td.year IN ({years*})
      AND td.tax_code_num IN ({tax_codes*})
      ORDER BY td.year, td.tax_code_num, td.agency_num
    ",
      years = unique(year),
      tax_codes = unique(tax_code),
      .con = conn
    )
  )

  df <- data.table::setDT(df, key = c("year", "tax_code", "agency_num"))
  df[, tif_share := as.numeric(tif_share)]

  return(df)
}
