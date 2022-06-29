# Checks to ensure data frames input to tax_bill() are the same as those
# returned by the lookup_ functions
check_agency_df_str <- function(agency_df) {
  stopifnot(is.data.frame(agency_df))

  agency_df_str <- c(
    "year" = "numeric", "tax_code" = "character", "agency_num" = "character",
    "agency_name" = "character", "agency_total_ext" = "numeric",
    "agency_total_eav" = "numeric"
  )

  if (!identical(agency_df_str, sapply(agency_df, mode))) {
    stop(
      "agency_df must be in the same format as the agency data ",
      "returned by lookup_agency(). Ensure all column names and types ",
      "are the same"
    )
  }

  return(TRUE)
}


check_pin_df_str <- function(pin_df) {
  stopifnot(is.data.frame(pin_df))

  pin_df_str <- c(
    "year" = "numeric", "pin" = "character", "class" = "character",
    "av" = "numeric", "eav" = "numeric", "exe_homeowner" = "numeric",
    "exe_senior" = "numeric", "exe_freeze" = "numeric",
    "exe_longtime_homeowner" = "numeric", "exe_disabled" = "numeric",
    "exe_vet_returning" = "numeric", "exe_vet_dis_lt50" = "numeric",
    "exe_vet_dis_50_69" = "numeric", "exe_vet_dis_ge70" = "numeric",
    "exe_abate" = "numeric"
  )

  if (!identical(pin_df_str, sapply(pin_df, mode))) {
    stop(
      "pin_df must be in the same format as the PIN data ",
      "returned by lookup_pin(). Ensure there is 1 row per PIN per ",
      "year and all column names and types are the same"
    )
  }

  return(TRUE)
}

check_tif_df_str <- function(tif_df) {
  stopifnot(is.data.frame(tif_df))

  tif_df_str <- c(
    "year" = "numeric", "tax_code" = "character",
    "agency_num" = "character", "agency_name" = "character",
    "tif_share" = "numeric"
  )

  if (!identical(tif_df_str, sapply(tif_df, mode))) {
    stop(
      "tif_df must be in the same format as the agency data ",
      "returned by lookup_tifs(). Ensure all column names and types ",
      "are the same"
    )
  }

  return(TRUE)
}
