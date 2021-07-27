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
#' @param fudge_factor Default 0.05. A percentage multiplier by which to
#'   increase the PTELL limiting rate. This helps adjust for things like
#'   recovered TIF increment and non-capped funds, which aren't included in this
#'   check. Formula used is \code{lim_rate + (lim_rate * fudge_factor)}.
#' @param quiet Default FALSE. A boolean value for whether or not to warn about
#'   violations of PTELL. See details for more information.
#' @param keep_ptell_cols Default FALSE. A boolean value indicating whether to
#'   keep PTELL columns joined to \code{levies_df_new}. When FALSE, the function
#'   just returns the unaltered \code{levies_df_new} plus any warnings.
#' @param eavs_df A data frame containing all aggregate agency EAVs used to
#'   calculate PTELL limiting rates. Defaults to the built-in data supplied by
#'   the Clerk (\code{\link{levy_and_total_eav_by_agency}}). Can be altered in
#'   the case of changed or future (estimated EAVs).
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
                               fudge_factor = 0.05,
                               quiet = FALSE,
                               keep_ptell_cols = FALSE,
                               eavs_df = ptaxsim::levy_and_total_eav_by_agency) { # nolint
  stopifnot(
    check_levies_df_str(levies_df_old),
    check_levies_df_str(levies_df_new),
    is.numeric(fudge_factor),
    length(fudge_factor) == 1,
    fudge_factor >= 0,
    fudge_factor <= 1,
    is.logical(quiet),
    length(quiet) == 1,
    is.logical(keep_ptell_cols),
    length(keep_ptell_cols) == 1
  )

  eavs_df_str <- c(
    "year" = "numeric", "agency" = "character", "agency_name" = "character",
    "home_rule_ind" = "logical",
    "total_eav" = "numeric", "total_levy" = "numeric"
  )

  if (!identical(eavs_df_str, sapply(eavs_df, mode))) {
    stop(
      "eavs_df must be in the same format as the built-in data ",
      "frame levy_and_total_eav_by_agency. Ensure columns have the same ",
      "names and types"
    )
  }

  # Get the lagged (previous) levy for each year, to be used when calculating
  # the PTELL limiting rate
  eavs_df <- eavs_df %>%
    dplyr::group_by(.data$agency) %>%
    dplyr::arrange(.data$agency, .data$year) %>%
    dplyr::mutate(prev_levy = dplyr::lag(.data$total_levy)) %>%
    dplyr::select(
      .data$year, .data$agency, .data$home_rule_ind,
      .data$prev_levy, .data$total_eav
    ) %>%
    dplyr::ungroup()

  cpis <- ptaxsim::cpis %>%
    dplyr::select(.data$levy_year, .data$ptell_cook)

  # Join data onto old levies data frame and calculate the naive limiting rate,
  # fudged limiting rate, and max levy
  levies_df_old <- levies_df_old %>%
    dplyr::left_join(eavs_df, by = c("year", "agency"), copy = TRUE) %>%
    dplyr::left_join(cpis, by = c("year" = "levy_year"), copy = TRUE) %>%
    dplyr::mutate(
      ptell_lim_rate = (.data$prev_levy * (1 +
        .data$ptell_cook)) / .data$total_eav,
      ptell_fudge_rate = (.data$ptell_lim_rate +
        (.data$ptell_lim_rate * fudge_factor)),
      ptell_max_levy = .data$total_eav * .data$ptell_fudge_rate
    ) %>%
    dplyr::select(
      .data$year, .data$agency, .data$home_rule_ind, .data$total_eav,
      .data$prev_levy, .data$ptell_cook,
      .data$ptell_lim_rate, .data$ptell_fudge_rate, .data$ptell_max_levy
    )

  # Join max levies onto new levies df and create and indicate col for exceeding
  levies_df_new <- levies_df_new %>%
    dplyr::left_join(levies_df_old, by = c("year", "agency")) %>%
    dplyr::mutate(
      ptell_viol = .data$total_levy > .data$ptell_max_levy &
        !.data$home_rule_ind
    )

  # Get the number of districts that exceed PTELL. Throw warning if any exceed
  num_ptell_violations <- sum(levies_df_new$ptell_viol)
  if (num_ptell_violations > 0) {
    warning(
      "At least ", num_ptell_violations, " taxing district have levy increases",
      " that may violate PTELL. Consider inspecting the new levies by setting ",
      "keep_ptell_cols to TRUE"
    )
  }

  # Return calculated PTELL columns attached to levies_df_new. Useful for
  # diagnostics and debugging
  if (keep_ptell_cols) {
    return(levies_df_new)
  } else {
    levies_df_new <- levies_df_new %>%
      dplyr::select(
        .data$year, .data$tax_code, .data$agency,
        .data$agency_name, .data$total_levy
      )
    return(levies_df_new)
  }
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
