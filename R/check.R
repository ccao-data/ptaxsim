#' Check if a connection object is a valid PTAXSIM database
#'
#' @description Checks the size, table names, and validity of a DBI connection
#'   object for the PTAXSIM database. Throws an error if the size or table names
#'   are not as expected.
#'
#' @param conn A valid DBI connection object pointing to a local PTAXSIM
#'   SQLite database file.
#'
#' @return Returns \code{TRUE} if all requirements are met, otherwise returns
#'   an error.
#'
#' @examples
#' \dontrun{
#' ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db")
#' check_db_conn(ptaxsim_db_conn)
#' }
#' @md
#' @family check
#' @export
check_db_conn <- function(conn) {
  if (!DBI::dbIsValid(conn)) {
    stop(
      "Database connection object is not valid! ",
      "Check the path to the PTAXSIM database file, make sure the database ",
      "file is not compressed, and make sure the connection object is active"
    )
  }

  db_size <- DBI::dbGetQuery(
    conn,
    "
    SELECT page_count * page_size AS size
    FROM pragma_page_count(), pragma_page_size();
    "
  )
  db_size <- db_size$size
  if (db_size == 0) {
    stop(
      "Connected to a database of size 0 Kb. Check the path to the PTAXSIM ",
      "database file"
    )
  }

  expected_tables <- c(
    "agency", "agency_fund", "agency_fund_info", "agency_info", "cpi",
    "eq_factor", "pin", "tax_code", "tif", "tif_distribution"
  )
  actual_tables <- DBI::dbListTables(conn)

  diff <- setdiff(expected_tables, actual_tables)
  if (length(diff) > 0) {
    stop(
      "Database is missing expected table(s): ",
      paste(diff, collapse = ", "),
      ". Check the path to the PTAXSIM database file"
    )
  }

  return(TRUE)
}


#' Check if the PTAXSIM package and database file are synchronized
#'
#' @description Performs a two-way check to synchronize the package and
#'   database.
#'
#'   1. Package version must be \code{>=} the minimum package version recorded
#'   in the database \code{metadata} table
#'   2. Database version must be \code{>=} the minimum database version recorded
#'   in the package \code{DESCRIPTION} file
#'
#' @param conn A valid DBI connection object pointing to a local PTAXSIM
#'   database file.
#'
#' @return Returns \code{TRUE} if all requirements are met, otherwise returns
#'   an error.
#'
#' @examples
#' \dontrun{
#' ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim.db")
#' check_db_sync(ptaxsim_db_conn)
#' }
#' @md
#' @family check
#' @export
check_db_sync <- function(conn) {
  if (!DBI::dbExistsTable(conn, "metadata")) {
    stop(
      "The PTAXSIM database file is missing the required metadata table. ",
      "It is likely that your database file is out-of-date. Please download ",
      "an updated version of the database file from GitLab"
    )
  }

  metadata <- DBI::dbGetQuery(conn, "SELECT * FROM metadata")
  loaded_db_version <- metadata$db_version
  req_db_version <-
    utils::packageDescription("ptaxsim")[["Config/Requires_DB_Version"]]

  loaded_pkg_version <- getNamespaceVersion("ptaxsim")
  req_pkg_version <- metadata$requires_pkg_version

  db_diff <- utils::compareVersion(loaded_db_version, req_db_version)
  if (db_diff == -1) {
    stop(
      "The PTAXSIM database file does not meet the minimum version ",
      "requirements of the PTAXSIM package (package requires DB version >= ",
      req_db_version, ", you have: ", loaded_db_version, "). Please download ",
      "an updated version of the PTAXSIM database file from GitLab"
    )
  }

  pkg_diff <- utils::compareVersion(loaded_pkg_version, req_pkg_version)
  if (pkg_diff == -1) {
    stop(
      "The PTAXSIM package does not meet the minimum version requirements ",
      "necessary to use the current database file (DB requires package ",
      "version >= ", req_pkg_version, ", you have ", loaded_pkg_version, "). ",
      "Please install an updated version of the PTAXSIM package from GitLab"
    )
  }

  return(TRUE)
}


# Checks to ensure data.tables input to tax_bill() are the same as those
# returned by the lookup_ functions
check_agency_dt_str <- function(agency_dt) {
  stopifnot(
    is.data.frame(agency_dt),
    data.table::is.data.table(agency_dt)
  )

  agency_dt_str <- c(
    "year" = "numeric", "tax_code" = "character", "agency_num" = "character",
    "agency_name" = "character", "agency_major_type" = "character",
    "agency_minor_type" = "character", "agency_total_eav" = "numeric",
    "agency_total_ext" = "numeric"
  )

  if (!identical(agency_dt_str, sapply(agency_dt, mode))) {
    stop(
      "agency_dt must be in the same format as the agency data ",
      "returned by lookup_agency(). Ensure all column names and types ",
      "are the same"
    )
  }

  keys <- c("year", "tax_code", "agency_num")
  if (!all(keys %in% data.table::key(agency_dt))) {
    stop(
      "agency_dt must have the same data.table keys as the agency data ",
      "returned by lookup_agency(). Please ensure that the year, tax_code, ",
      "and agency_num columns are set as data.table keys"
    )
  }

  return(TRUE)
}


check_pin_dt_str <- function(pin_dt) {
  stopifnot(
    is.data.frame(pin_dt),
    data.table::is.data.table(pin_dt)
  )

  pin_dt_str <- c(
    "year" = "numeric", "pin" = "character", "class" = "character",
    "av" = "numeric", "eav" = "numeric", "exe_homeowner" = "numeric",
    "exe_senior" = "numeric", "exe_freeze" = "numeric",
    "exe_longtime_homeowner" = "numeric", "exe_disabled" = "numeric",
    "exe_vet_returning" = "numeric", "exe_vet_dis_lt50" = "numeric",
    "exe_vet_dis_50_69" = "numeric", "exe_vet_dis_ge70" = "numeric",
    "exe_abate" = "numeric"
  )

  if (!identical(pin_dt_str, sapply(pin_dt, mode))) {
    stop(
      "pin_dt must be in the same format as the PIN data ",
      "returned by lookup_pin(). Ensure there is 1 row per PIN per ",
      "year and all column names and types are the same"
    )
  }

  keys <- c("year", "pin")
  if (!all(keys %in% data.table::key(pin_dt))) {
    stop(
      "pin_dt must have the same data.table keys as the PIN data ",
      "returned by lookup_pin(). Please ensure that the year ",
      "and pin columns are set as data.table keys"
    )
  }

  return(TRUE)
}


check_tif_dt_str <- function(tif_dt) {
  stopifnot(
    is.data.frame(tif_dt),
    data.table::is.data.table(tif_dt)
  )

  tif_dt_str <- c(
    "year" = "numeric", "tax_code" = "character",
    "agency_num" = "character", "agency_name" = "character",
    "agency_major_type" = "character", "agency_minor_type" = "character",
    "tif_share" = "numeric"
  )

  if (!identical(tif_dt_str, sapply(tif_dt, mode))) {
    stop(
      "tif_dt must be in the same format as the agency data ",
      "returned by lookup_tif(). Ensure all column names and types ",
      "are the same"
    )
  }

  keys <- c("year", "tax_code", "agency_num")
  if (!all(keys %in% data.table::key(tif_dt))) {
    stop(
      "tif_dt must have the same data.table keys as the agency data ",
      "returned by lookup_tif(). Please ensure that the year, tax_code, ",
      "and agency_num columns are set as data.table keys"
    )
  }

  return(TRUE)
}
