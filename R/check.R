#' Check that levy changes conform to PTELL
#'
#' @description The Property Tax Extension Limitation Law (PTELL) limits the
#'   growth of taxing agency levies. It caps the maximum levy increase for a
#'   given year at the rate of inflation or 5%, whichever is less. PTELL is
#'   colloquially known as "tax caps."
#'
#'   All non-home rule taxing districts in Cook County are subject to PTELL.
#'   This typically includes smaller government agencies such as libraries,
#'   forest preserves, and townships.
#'
#'   PTELL uses the prior year's urban consumer price index (CPI-U) to calculate
#'   the inflation rate. For example, the tax year 2020 CPI-U was 2.3%, based on
#'   the change from December 2018 to December 2019.
#'
#'   The goal of this function is to check that hypothetical levy changes do not
#'   violate PTELL. However, the true application of PTELL is extremely complex.
#'   As such, this function aims to be a heuristic, *not a true check*. It comes
#'   with the following caveats, use at your own discretion:
#'
#'   - Ignores home rule districts via home rule indicators provided by the
#'     Cook County Clerk's Office
#'   - Does not perform rate limits checks for specific funds (this
#'     functionality is entirely excluded from PTAXSIM)
#'   - Ignores potential Clerk adjustments to final levies to account for loss
#'   - Does not differentiate between cap and non-cap funds
#'   - Certain property types are supposed to be excluded when calculating PTELL
#'     limiting rates, including:
#'     - New property
#'     - Property annexed from other districts
#'     - Recovered TIF increment (after expiration)
#'     - Expired incentives that reduce AV or EAV
#'     - Disconnected property
#'
#'     This function ignores those exclusions and simply uses the aggregate EAV
#'     for each taxing district provided by the Clerk
#'   - In practice, the Clerk uses the highest aggregate levy of the past
#'     3 years to calculate the limiting rate. However, this function simply
#'     uses the prior year's levy
#'
#' @param levies_df_old A data frame in the format returned by
#'   \code{\link{lookup_levies}}. Contains the original (unchanged) levies.
#' @param levies_df_new A data frame in the format returned by
#'   \code{\link{lookup_levies}}. Contains the new, altered levies.
#' @param quiet Default FALSE. A boolean value for whether or not to warn about
#'   violations of PTELL. See details for more information.
#' @param agency_eavs_df A data frame containing all aggregate agency EAVs to be
#'   used to calculate PTELL limiting rates. Defaults to the actual EAVs
#'   supplied by the Clerk in \code{\link{levy_and_total_eav_by_agency}}. Can be
#'   altered in the case of changed or future (estimated EAVs).
#'
#' @return Returns a warning when any change to levies between
#'   \code{levies_df_old} and \code{levies_df_new} could violate PTELL.
#'
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family check
#' @export
check_levies_ptell <- function(levies_df_old,
                               levies_df_new,
                               quiet = FALSE,
                               agency_eavs_df = ptaxsim::levy_and_total_eav_by_agency) { # nolint
  check_levies_df_str(levies_df_old)
  check_levies_df_str(levies_df_new)

  levies_df_old %>%
    dplyr::left_join(
      agency_eavs_df %>%
        dplyr::select(.data$year, .data$agency, .data$home_rule_ind),
      by = c("year", "agency"),
      copy = TRUE
    )
}


# Checks to ensure data frames are in the outputs returned by the lookup_
# series of functions
check_agency_eavs_df_str <- function(agency_eavs_df) {
  stopifnot(is.data.frame(agency_eavs_df))

  agency_eavs_df_str <- c(
    "year" = "numeric", "tax_code" = "character", "agency" = "character",
    "agency_name" = "character", "total_eav" = "numeric"
  )

  if (!identical(agency_eavs_df_str, sapply(agency_eavs_df, mode))) {
    stop(
      "agency_eavs_df must be in the same format as the agency data ",
      "returned by lookup_agency_eavs(). Ensure all column names and types ",
      "are the same"
    )
  }

  return(TRUE)
}


check_exemptions_df_str <- function(exemptions_df) {
  stopifnot(is.data.frame(exemptions_df))

  exemptions_df_str <- c(
    "year" = "numeric", "pin" = "character", "exe_homeowner" = "numeric",
    "exe_senior" = "numeric", "exe_freeze" = "numeric",
    "exe_longtime_homeowner" = "numeric", "exe_disabled" = "numeric",
    "exe_vet_returning" = "numeric", "exe_vet_dis_lt50" = "numeric",
    "exe_vet_dis_50_69" = "numeric", "exe_vet_dis_ge70" = "numeric",
    "exe_abate" = "numeric"
  )

  if (!identical(exemptions_df_str, sapply(exemptions_df, mode))) {
    stop(
      "exemptions_df must be in the same format as the exemptions data ",
      "returned by lookup_exemptions(). Ensure there is 1 row per PIN per ",
      "year and all column names and types are the same"
    )
  }

  return(TRUE)
}


check_levies_df_str <- function(levies_df) {
  stopifnot(is.data.frame(levies_df))

  levies_df_str <- c(
    "year" = "numeric", "tax_code" = "character", "agency" = "character",
    "agency_name" = "character", "total_levy" = "numeric"
  )
  if (!identical(levies_df_str, sapply(levies_df, mode))) {
    stop(
      "levies_df must be in the same format as the levy data ",
      "returned by lookup_levies(). Ensure all column names and types ",
      "are the same"
    )
  }

  return(TRUE)
}


check_tifs_df_str <- function(tifs_df) {
  stopifnot(is.data.frame(tifs_df))

  tifs_df_str <- c(
    "year" = "numeric", "tax_code" = "character", "agency" = "character",
    "agency_name" = "character", "tif_share" = "numeric"
  )

  if (!identical(tifs_df_str, sapply(tifs_df, mode))) {
    stop(
      "tifs_df must be in the same format as the agency data ",
      "returned by lookup_tifs(). Ensure all column names and types ",
      "are the same"
    )
  }

  return(TRUE)
}
