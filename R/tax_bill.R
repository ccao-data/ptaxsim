#' Calculate an approximate Cook County property tax bill
#'
#' @description This function calculates a Cook County property tax bill using
#'   levy, base, and TIF information from the Cook County Clerk. It allows users
#'   to simulate different tax scenarios by changing the input data. For
#'   example, this function can answer the following questions:
#'
#'   - What would my tax bill be if my assessed value was $10K lower?
#'   - What would my tax bill be if Chicago increased its levy by 1%?
#'   - What would my tax bill be if my property was in a different neighborhood?
#'   - What would my tax bill be if a large new development was added to
#'     my neighborhood?
#'
#'   Most of the data necessary to answer these questions is built into the
#'   package. Data for the most current tax year is usually available the
#'   following year (2020 data is available in 2021). If needed, users can
#'   supply current tax year data manually. See vignettes for more information.
#'
#' @details Note that all vector inputs (suffixed with \code{_vec}) must be
#'   either length 1 or the same length as the longest vector (standard
#'   recycling rules apply).
#'
#'   The district-level tax amounts returned by this function will *not*
#'   perfectly match the amounts on real tax bills. This is due to rounding
#'   and truncating that occurs in the real system. Most estimated amounts will
#'   still be within a few dollars of the amounts on real bills.
#'
#' @note This package is for educational purposes only.
#'   The Assessor's office releases the package without any representations or
#'   warranties of any kind, whether express or implied. Any data, figures, or
#'   amounts contained within the package, used by the package, or produced by
#'   the package are solely for illustrative purposes. Any results produced by
#'   this package as distributed are official and should not be relied upon for
#'   any business or commercial purpose. They merely demonstrate the package's
#'   features. The Assessor's office expressly disclaims any liability for
#'   any entity's reliance on this package.
#'
#' @param year_vec Numeric vector of tax years for which to return bills.
#' @param pin_vec Character vector of 14-digit Property Index Numbers (PINs)
#'   with no dashes or spaces.
#' @param tax_code_vec Character vector of 5-digit Cook County tax codes. These
#'   codes are included on individual property tax bills. If missing, the
#'   \code{\link{lookup_tax_code}} function will be used to retrieve tax codes
#'   based on \code{year_vec} and \code{pin_vec}.
#' @param eav_vec Numeric vector of Equalized Assessed Values (EAVs) for each
#'   combination of year and PIN. If missing, the \code{\link{lookup_eav}}
#'   function will be used to retrieve each PIN's EAV based on \code{year_vec}
#'   and \code{pin_vec}.
#' @param eq_fct_vec Numeric vector of Cook County equalization factors for each
#'   year. If missing \code{\link{lookup_equalization_factor}} will be used to
#'   retrieve the equalization factor specific to \code{year_vec}.
#' @param exemptions_df Data frame containing the exemptions specific to each
#'   PIN and year. Data *must* be identical to the format returned by
#'   \code{\link{lookup_exemptions}}. If missing,
#'   \code{\link{lookup_exemptions}} will be used to retrieve each PIN's
#'   exemptions based on \code{year_vec} and \code{pin_vec}.
#' @param levies_df Data frame containing levies for each taxing district in
#'   the specified tax code. Data *must* be identical to the format returned by
#'   \code{\link{lookup_levies}}. If missing,
#'   \code{\link{lookup_levies}} will be used to retrieve each taxing
#'   jurisdiction's levies based on \code{year_vec} and \code{tax_code_vec}.
#' @param agency_eavs_df Data frame containing total EAVs (the base) for each
#'   taxing district in the specified tax code. Data *must* be identical to the
#'   format returned by \code{\link{lookup_agency_eavs}}. If missing,
#'   \code{\link{lookup_agency_eavs}} will be used to retrieve each taxing
#'   jurisdiction's base based on \code{year_vec} and \code{tax_code_vec}.
#' @param tifs_df Data frame containing any TIF applicable to the specified
#'   tax code. Will be an empty data frame if no TIF exists for the property.
#'   Data *must* be identical to the format returned by
#'   \code{\link{lookup_tifs}}. If missing, \code{\link{lookup_tifs}} will be
#'   used to retrieve the tax code's TIF share based on \code{year_vec}
#'   and \code{tax_code_vec}.
#' @param simplify Default TRUE. Boolean for whether or not to keep only the
#'   the most vital columns in the final output, including tax amounts, rates,
#'   and amounts to TIFs.
#'
#' @return A data frame which contains a tax bill for each specified PIN and
#'   year. Each tax bill is broken out by taxing district, meaning there is a
#'   row for each district relevant to each PIN. Most PINs have 10-15 districts
#'   (rows) associated with each year.
#'
#'   Note that unlike official tax bills, TIFs are not broken out into a
#'   separate line item and instead are contained in their own column
#'   (named \code{tax_amt_total_to_tif}).
#'
#' @examples
#' \donttest{
#' # Get a single tax bill
#' tax_bill(2019, "14081020210000")
#'
#' # Get tax bills for multiple years
#' tax_bill(2017:2019, "14081020210000")
#'
#' # Get multiple tax bills for different PINs and years
#' tax_bill(c(2014, 2019), c("14081020210000", "07133020190000"))
#'
#' # Get a tax bill with a new EAV
#' tax_bill(2019, "14081020210000", eav_vec = 200000)
#'
#' # Get a tax bill for a new location
#' tax_bill(2019, "14081020210000", tax_code_vec = "35011")
#' }
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family tax_bill
#' @export
tax_bill <- function(year_vec,
                     pin_vec,
                     tax_code_vec = lookup_tax_code(year_vec, pin_vec),
                     eav_vec = lookup_eav(year_vec, pin_vec),
                     eq_fct_vec = lookup_equalization_factor(year_vec),
                     exemptions_df = lookup_exemptions(year_vec, pin_vec),
                     levies_df = lookup_levies(year_vec, tax_code_vec),
                     agency_eavs_df = lookup_agency_eavs(year_vec, tax_code_vec), # nolint
                     tifs_df = lookup_tifs(year_vec, tax_code_vec),
                     simplify = TRUE) {

  # Basic type/input checking for vector inputs
  stopifnot(
    is.numeric(year_vec),
    length(year_vec) > 0,
    all(nchar(pin_vec) == 14),
    is.character(pin_vec),
    all(nchar(tax_code_vec) == 5 | is.na(tax_code_vec)),
    is.character(tax_code_vec),
    is.numeric(eav_vec),
    length(eav_vec) > 0,
    is.numeric(eq_fct_vec),
    length(eq_fct_vec) > 0,
    is.logical(simplify),
    length(simplify) == 1
  )

  # Input checking to ensure data frames have expected structure (col name and
  # data type)
  stopifnot(
    check_agency_eavs_df_str(agency_eavs_df),
    check_exemptions_df_str(exemptions_df),
    check_levies_df_str(levies_df),
    check_tifs_df_str(tifs_df)
  )

  # Combine the input vectors in a single tibble. Let tibble() handle errors
  # for mismatched vector sizes/types
  df <- dplyr::tibble(
    "year" = year_vec,
    "pin" = pin_vec,
    "tax_code" = tax_code_vec,
    "eav_before_exemptions" = eav_vec,
    "eq_factor" = eq_fct_vec
  )

  # Join exemptions data to the PIN and year
  df <- dplyr::left_join(df, exemptions_df, by = c("year", "pin"))

  # Fetch TIF for a given tax code. There should only be one TIF per tax code (
  # with a few exceptions)
  df <- df %>%
    dplyr::left_join(
      dplyr::select(tifs_df, -dplyr::any_of("agency_name")) %>%
        dplyr::rename(tif_agency = .data$agency) %>%
        dplyr::filter(!is.na(.data$tif_share)),
      by = c("year", "tax_code")
    )

  # Add an indicator for when the PIN is in the special Red-Purple Modernization
  # TIF, which has different rules than other TIFs
  df <- df %>%
    dplyr::group_by(.data$year, .data$pin) %>%
    dplyr::mutate(in_rpm_tif = any(.data$tif_agency == "030210900")) %>%
    dplyr::ungroup()

  # Fill any missing values in TIF share or indicator
  df <- df %>%
    dplyr::mutate(
      tif_share = replace(.data$tif_share, is.na(.data$tif_share), 0),
      in_rpm_tif = replace(.data$in_rpm_tif, is.na(.data$in_rpm_tif), FALSE)
    )

  # Fetch levy for all agencies of a tax code, this will expand the number of
  # rows from 1 per PIN per year, to N per PIN per year, where N is the number
  # of agencies associated with a PIN's tax code
  df <- df %>%
    dplyr::left_join(levies_df, by = c("year", "tax_code")) %>%
    dplyr::filter(!is.na(.data$total_levy))

  # Fetch total EAV for all agencies of a tax code. This should be the same
  # number of agencies as for the levies df
  df <- df %>%
    dplyr::left_join(
      dplyr::select(agency_eavs_df, -dplyr::any_of("agency_name")),
      by = c("year", "tax_code", "agency")
    )

  # Calculate the tax amount deducted for each exemption, then aggregate and
  # calculate the final taxable amount
  df <- df %>%
    dplyr::mutate(
      agency_tax_rate = .data$total_levy / .data$total_eav,
      dplyr::across(
        dplyr::starts_with("exe_"),
        ~ .x * .data$agency_tax_rate
      ),
      tax_amt_exempt = rowSums(
        dplyr::across(dplyr::starts_with("exe_"))
      ),
      tax_amt_pre_exemptions =
        .data$eav_before_exemptions * .data$agency_tax_rate,
      tax_amt_post_exemptions =
        .data$tax_amt_pre_exemptions - .data$tax_amt_exempt,
      tax_amt_post_exemptions = ifelse(
        .data$tax_amt_post_exemptions < 0, 0, .data$tax_amt_post_exemptions
      ),
      tax_amt_total_to_tif =
        .data$tax_amt_post_exemptions * .data$tif_share
    )

  # Special handling for the Red-Purple Modernization TIF (RPM1). For this TIF
  # specifically:
  # 1. CPS receives their proportionate share of revenue (they ignore the TIF)
  # 2. 80% of the remaining revenue goes to the RPM TIF
  # 3. 20% of the remaining revenue goes to all taxing districts besides CPS
  df <- df %>%
    dplyr::group_by(.data$year, .data$pin) %>%
    dplyr::mutate(
      is_cps_agency = .data$agency == "044060000",
      tax_rate_for_cps = sum(.data$is_cps_agency * .data$agency_tax_rate),
      tax_prop_for_cps = .data$tax_rate_for_cps / sum(.data$agency_tax_rate),

      # Get total amount from TIF held for CPS. Will be 0 if not in RPM TIF
      tax_amt_rpm_tif_to_cps =
        .data$tax_amt_total_to_tif * .data$tax_prop_for_cps * .data$in_rpm_tif,

      # Get total amount from TIF held for the RPM project, which is 80% of the
      # funds remaining after the CPS cut
      tax_amt_rpm_tif_to_rpm =
        (.data$tax_amt_total_to_tif - .data$tax_amt_rpm_tif_to_cps) * 0.8 *
          .data$in_rpm_tif,

      # Get total amount from TIF sent BACK to jurisdictions. Need to divvy up
      # the amount that would be sent to CPS proportionally among other bodies
      # using each agency's tax rate
      tax_amt_rpm_tif_back_to_jur_total = sum(
        (.data$tax_amt_total_to_tif -
          .data$tax_amt_rpm_tif_to_cps - .data$tax_amt_rpm_tif_to_rpm)
      ),
      tax_amt_rpm_tif_back_to_jur_dist = ifelse(
        !.data$is_cps_agency,
        .data$agency_tax_rate /
          (sum(.data$agency_tax_rate * !.data$is_cps_agency)) *
          .data$in_rpm_tif,
        0
      ),
      tax_amt_rpm_tif_back_to_jur =
        (.data$tax_amt_rpm_tif_back_to_jur_total *
          .data$tax_amt_rpm_tif_back_to_jur_dist),
      tax_amt_final =
        .data$tax_amt_post_exemptions - .data$tax_amt_total_to_tif +
          .data$tax_amt_rpm_tif_back_to_jur,
      tax_amt_total_to_tif =
        .data$tax_amt_total_to_tif - .data$tax_amt_rpm_tif_back_to_jur
    ) %>%
    dplyr::ungroup()


  # Remove extraneous columns if simplify is TRUE
  if (simplify) {
    df <- df %>%
      dplyr::select(
        .data$year,
        .data$pin,
        .data$tax_code,
        .data$agency,
        .data$agency_name,
        .data$agency_tax_rate,
        .data$eav_before_exemptions,
        .data$tax_amt_post_exemptions,
        .data$tax_amt_total_to_tif,
        .data$tax_amt_final
      )
  }

  return(df)
}
