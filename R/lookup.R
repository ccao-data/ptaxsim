globalVariables(c("ptaxsim_db_conn", "."))

#' Lookup data sets required to calculate property tax bills
#'
#' @description The functions in this group take a tax year, PIN, and/or tax
#'   code as input and return a \code{data.table} as an output. The returned
#'   data are in a specific format (in terms of column name/type/order)
#'   and can be used directly in \code{\link{tax_bill}} or piped to helper
#'   functions.
#'
#'   When calculating tax bills for future years or providing custom/simulated
#'   data, you'll need to replicate the format returned by these functions.
#'
#' @param year A numeric vector of tax years.
#' @param pin A character vector of 14-digit PINs.
#' @param tax_code A character vector of 5-digit Cook County tax codes.
#' @param conn A connection object pointing to a local copy of the
#'   PTAXSIM database.
#'
#' @return A \code{data.table} containing the specified tax data.
#'
#' @examples
#' \dontrun{
#' lookup_agency(2019, "73105")
#' lookup_pin(2018:2019, "20304190020000")
#' lookup_tif(2019, "73105")
#' }
#' @md
#' @family lookups
#' @name lookup_dt
NULL


#' Lookup values required to calculate property tax bills
#'
#' @description The functions in this group take a tax year and PIN as input and
#'   return a vector as an output. The output vector can be used directly in
#'   \code{\link{tax_bill}}.
#'
#' @param year A numeric vector of tax years.
#' @param pin A character vector of 14-digit PINs.
#' @param conn A connection object pointing to a local copy of the
#'   PTAXSIM database.
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


#' @describeIn lookup_dt Lookup tax agency (CPS, Cook County, forest preserve)
#'   total equalized assessed values. Such values are known as the "base" and
#'   are used to calculate per agency tax rates. Also returns each agency's
#'   levy amount. Returns a \code{data.table} with 1 row for each agency
#'   associated with the input tax code and year.
#'
#' @export
lookup_agency <- function(year, tax_code, conn = ptaxsim_db_conn) {
  stopifnot(
    is.numeric(year),
    is.character(tax_code),
    all(year >= 2006),
    all(nchar(tax_code) == 5 | is.na(tax_code)),
    check_db_conn(conn)
  )

  dt <- DBI::dbGetQuery(
    conn,
    statement = glue::glue_sql(
      "
      SELECT
          tc.year,
          tc.tax_code_num AS tax_code,
          tc.agency_num,
          ai.agency_name,
          ai.major_type AS agency_major_type,
          ai.minor_type AS agency_minor_type,
          a.cty_cook_eav AS agency_total_eav,
          a.total_ext AS agency_total_ext
      FROM tax_code tc
      LEFT JOIN agency a
          ON tc.year = a.year
          AND tc.agency_num = a.agency_num
      LEFT JOIN agency_info ai
          ON tc.agency_num = ai.agency_num
      WHERE tc.year IN ({years*})
      AND tc.tax_code_num IN ({tax_codes*})
      AND a.cty_cook_eav IS NOT NULL
      ORDER BY tc.year, tc.tax_code_num, tc.agency_num
    ",
      years = unique(year),
      tax_codes = unique(tax_code),
      .con = conn
    )
  )

  dt <- data.table::setDT(dt, key = c("year", "tax_code", "agency_num"))

  return(dt)
}


#' @describeIn lookup_dt Lookup the equalized assessed value and exemptions
#'   for a specific PIN and year. Returns a \code{data.table} with AV, EAV, and
#'   a column specifying the EAV value of each received exemption.
#'
#' @param stage A length 1 character vector indicating the assessment stage
#'   from which to pull assessed value (column \code{av}) and equalized assessed
#'   value (column \code{eav}). Options include \code{"mailed"},
#'   \code{"certified"}, \code{"board"}, and \code{"clerk"}.
#'
#' @export
lookup_pin <- function(year, pin, stage = "clerk", conn = ptaxsim_db_conn) {
  stopifnot(
    is.numeric(year),
    is.character(pin),
    is.character(stage),
    length(stage) == 1,
    all(year >= 2006),
    all(nchar(pin) == 14),
    check_db_conn(conn)
  )

  if (!stage %in% c("mailed", "certified", "board", "clerk")) {
    stop("stage must be one of: mailed, certified, board, clerk")
  }

  # This lookup uses a temp table since it's faster than putting lots of
  # values into the WHERE clause for large lookups
  dt_idx <- data.table::CJ("year" = year, "pin" = pin, unique = TRUE)
  DBI::dbWriteTable(
    conn = conn,
    name = "lookup_pin",
    value = dt_idx,
    overwrite = TRUE,
    temporary = TRUE
  )

  stage <- glue::glue("av_", stage)
  dt <- DBI::dbGetQuery(
    conn,
    statement = glue::glue_sql(
      "
      SELECT
          lp.year,
          lp.pin,
          p.class,
          p.{`stage`} AS av,
          CAST(ROUND(p.{`stage`} * ef.eq_factor, 0) AS int) AS eav,
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
      INNER JOIN pin p
          ON lp.year = p.year
          AND lp.pin = p.pin
      LEFT JOIN eq_factor ef
          ON lp.year = ef.year
      ORDER BY lp.year, lp.pin
    ",
      .con = conn
    )
  )

  dt <- data.table::setDT(dt, key = c("year", "pin"))

  return(dt)
}


#' @describeIn lookup_vec Lookup the Cook County tax code for a specific PIN and
#'   year. Tax codes represent the unique geographic overlap of different
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
    check_db_conn(conn)
  )

  if (length(year) != length(pin)) {
    dt_idx <- data.table::CJ("year" = year, "pin" = pin, sorted = FALSE)
  } else {
    dt_idx <- data.table::data.table("year" = year, "pin" = pin)
  }

  DBI::dbWriteTable(
    conn = conn,
    name = "lookup_tax_code",
    value = dt_idx,
    overwrite = TRUE,
    temporary = TRUE
  )

  dt <- DBI::dbGetQuery(
    conn,
    statement =
      "
      SELECT p.tax_code_num
      FROM lookup_tax_code ltc
      LEFT JOIN pin p
          ON p.year = ltc.year
          AND p.pin = ltc.pin
    "
  )

  return(dt$tax_code_num)
}


#' @describeIn lookup_dt Lookup any TIFs that apply to a given tax code and
#'   year. Returns a \code{data.table} with only 1 row per tax code and year,
#'   or 0 rows if the tax code is not part of a TIF.
#'
#' @export
lookup_tif <- function(year, tax_code, conn = ptaxsim_db_conn) {
  stopifnot(
    is.numeric(year),
    is.character(tax_code),
    all(year >= 2006),
    all(nchar(tax_code) == 5 | is.na(tax_code)),
    check_db_conn(conn)
  )

  tif_share <- NULL
  dt <- DBI::dbGetQuery(
    conn,
    statement = glue::glue_sql("
      SELECT
          td.year,
          td.tax_code_num AS tax_code,
          td.agency_num,
          ai.agency_name,
          ai.major_type AS agency_major_type,
          ai.minor_type AS agency_minor_type,
          td.tax_code_distribution_pct / 100 AS tif_share
      FROM tif_distribution td
      LEFT JOIN agency_info ai
          ON td.agency_num = ai.agency_num
      WHERE td.year IN ({years*})
      AND td.tax_code_num IN ({tax_codes*})
      ORDER BY td.year, td.tax_code_num, td.agency_num
    ",
      years = unique(year),
      tax_codes = unique(tax_code),
      .con = conn
    )
  )

  dt <- data.table::setDT(dt, key = c("year", "tax_code", "agency_num"))
  dt[, tif_share := as.numeric(tif_share)]

  return(dt)
}
