# Checks to ensure data frames input to tax_bill() are the same as those
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
