#' Calculate an approximate Cook County property tax bill
#'
#' @description This function calculates a Cook County property tax bill using
#'   levy, base, and TIF information from the Cook County Clerk and Assessor.
#'   It allows users to simulate different tax scenarios by changing the input
#'   data. For example, this function can answer the following questions:
#'
#'   - What would my tax bill be if my assessed value was $10K lower?
#'   - What would my tax bill be if Chicago increased its levy by 5%?
#'   - What would my tax bill be if my property was in a different neighborhood?
#'   - What would my tax bill be if a large new development was added to
#'     my neighborhood?
#'
#'   Most of the data necessary to answer these questions is built into the
#'   package. Data for the most current tax year is usually available the
#'   following year (2020 data is available in 2021). If needed, users can
#'   supply current tax year data manually. See vignettes for more information.
#'
#' @details Note that all vector inputs (suffixed with \code{_vec}) have two
#'   input modes:
#'
#'   1. If the input vectors are the same length, pairwise tax bills are
#'   returned (each PIN, year, and tax code combination returns 1 bill).
#'   2. If the input vectors are different lengths, the Cartesian product of
#'   the vectors is returned (5 years and 10 PINs will return all 5 years' of
#'   bills for each PIN).
#'
#'   The district-level tax amounts returned by this function will *not*
#'   perfectly match the amounts on real tax bills. This is due to rounding
#'   and truncating that occurs in the real system. Most estimated amounts will
#'   still be within a few dollars of the amounts on real bills.
#'
#' @param year_vec Numeric vector of tax years for which to return bills.
#' @param pin_vec Character vector of 14-digit Property Index Numbers (PINs)
#'   with no dashes or spaces.
#' @param tax_code_vec Character vector of 5-digit Cook County tax codes. These
#'   codes are included on individual property tax bills. If missing, the
#'   \code{\link{lookup_tax_code}} function will be used to retrieve tax codes
#'   based on \code{year_vec} and \code{pin_vec}.
#' @param pin_df Data frame containing the exemptions and assessed value
#'   specific to each PIN and year. Data *must* be identical to the format
#'   returned by \code{\link{lookup_pin}}. If missing, \code{\link{lookup_pin}}
#'   will be used to retrieve each PIN's information based on \code{year_vec}
#'   and \code{pin_vec}.
#' @param agency_df Data frame containing the levy and base amount for each
#'   taxing district in the specified tax code. Data *must* be identical to the
#'   format returned by \code{\link{lookup_agency}}. If missing,
#'   \code{\link{lookup_agency}} will be used to retrieve each taxing
#'   jurisdiction's levies based on \code{year_vec} and \code{tax_code_vec}.
#' @param tif_df Data frame containing any TIF applicable to the specified
#'   tax code. Will be an empty data frame if no TIF exists for the property.
#'   Data *must* be identical to the format returned by
#'   \code{\link{lookup_tif}}. If missing, \code{\link{lookup_tif}} will be
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
#' @importFrom data.table :=
#' @examples
#' \dontrun{
#' # Get a single tax bill
#' tax_bill(2019, "14081020210000")
#'
#' # Get tax bills for multiple years
#' tax_bill(2017:2019, "14081020210000")
#'
#' # Get multiple tax bills for different PINs and years
#' tax_bill(c(2014, 2019), c("14081020210000", "07133020190000"))
#'
#' # Get a tax bill for a new location
#' tax_bill(2019, "14081020210000", tax_code_vec = "35011")
#' }
#' @md
#' @family tax_bill
#' @export
tax_bill <- function(year_vec,
                     pin_vec,
                     tax_code_vec = lookup_tax_code(year_vec, pin_vec),
                     pin_df = lookup_pin(year_vec, pin_vec),
                     agency_df = lookup_agency(year_vec, tax_code_vec),
                     tif_df = lookup_tif(year_vec, tax_code_vec),
                     simplify = TRUE) {

  # Basic type/input checking for vector inputs
  stopifnot(
    is.numeric(year_vec),
    length(year_vec) > 0,
    all(nchar(pin_vec) == 14),
    is.character(pin_vec),
    all(nchar(tax_code_vec) == 5 | is.na(tax_code_vec)),
    is.character(tax_code_vec),
    is.logical(simplify),
    length(simplify) == 1
  )

  # Input checking to ensure data frames have expected structure (col name and
  # data type)
  stopifnot(
    check_agency_df_str(agency_df),
    check_pin_df_str(pin_df),
    check_tif_df_str(tif_df)
  )

  # Create data.table from inputs. Use the cartesian product if the inputs are
  # different lengths
  tax_code <- NULL
  if (length(year_vec) != length(pin_vec)) {
    dt <- data.table::as.data.table(
      expand.grid("year" = year_vec, "pin" = pin_vec)
    )
  } else {
    dt <- data.table::data.table("year" = year_vec, "pin" = pin_vec)
  }
  dt[, tax_code := tax_code_vec]
  dt <- data.table::setDT(dt, key = c("year", "pin"))

  # Merge PIN-level data (exemptions, eav) to the input data
  dt <- merge(dt, pin_df, all.x = TRUE)

  # Fetch TIF for a given tax code. There should only be one TIF per tax code (
  # with a few exceptions)
  dt <- merge(
    dt,
    data.table::setnames(tif_df[, !"agency_name"], "agency_num", "tif_agency"),
    by = c("year", "tax_code"),
    all.x = TRUE
  )

  # Add an indicator for when the PIN is in the special Red-Purple Modernization
  # TIF, which has different rules than other TIFs
  in_rpm_tif <- tif_agency <- tif_share <- NULL
  dt[, in_rpm_tif := any(tif_agency == "030210900"), by = c("year", "pin")]
  dt[is.na(in_rpm_tif), in_rpm_tif := FALSE]
  dt[is.na(tif_share), tif_share := 0]

  # Fetch levy for all agencies of a tax code, this will expand the number of
  # rows from 1 per PIN per year, to N per PIN per year, where N is the number
  # of agencies associated with a PIN's tax code
  dt <- merge(dt, agency_df, by = c("year", "tax_code"), allow.cartesian = TRUE)

  # Calculate the tax amount deducted for each exemption, then aggregate and
  # calculate the final taxable amount
  agency_tax_rate <- tax_amt_exempt <- tax_amt_pre_exemptions <- total_ext <-
    total_eav <- .SD <- eav <- is_cps_agency <- tax_amt_post_exemptions <-
    tax_amt_total_to_tif <- agency_num <- tax_amt_rpm_tif_back_to_jur_dist <-
    tax_rate_for_cps <- tax_prop_for_cps <- tax_amt_rpm_tif_to_cps <-
    tax_amt_rpm_tif_to_cps <- tax_amt_final <- tax_amt_rpm_tif_back_to_jur <-
    tax_amt_rpm_tif_back_to_jur_total <- tax_amt_rpm_tif_to_rpm <- av <-
    class <- NULL

  exe_cols <- names(dt)[startsWith(names(dt), "exe_")]
  dt[, agency_tax_rate := total_ext / total_eav]
  dt[, (exe_cols) := lapply(.SD, "*", agency_tax_rate), .SDcols = exe_cols]
  dt[, tax_amt_exempt := rowSums(.SD), .SDcols = exe_cols]
  dt[, eav := ifelse(class == "239", av, eav)] # farmland taxed with AV, not EAV
  dt[, tax_amt_pre_exemptions := eav * agency_tax_rate]
  dt[, tax_amt_post_exemptions := tax_amt_pre_exemptions - tax_amt_exempt]
  dt[tax_amt_post_exemptions < 0, tax_amt_post_exemptions := 0]
  dt[, tax_amt_total_to_tif := tax_amt_post_exemptions * tif_share]

  # Special handling for the Red-Purple Modernization TIF (RPM1). For this TIF
  # specifically:
  # 1. CPS receives their proportionate share of revenue (they ignore the TIF)
  # 2. 80% of the remaining revenue goes to the RPM TIF
  # 3. 20% of the remaining revenue goes to all taxing districts besides CPS
  dt[, is_cps_agency := agency_num == "044060000"]
  dt[, tax_rate_for_cps :=
    sum(is_cps_agency * agency_tax_rate), by = c("year", "pin")]
  dt[, tax_prop_for_cps :=
    tax_rate_for_cps / sum(agency_tax_rate), by = c("year", "pin")]

  # Get total amount from TIF held for CPS. Will be 0 if not in RPM TIF
  dt[, tax_amt_rpm_tif_to_cps :=
    tax_amt_total_to_tif * tax_prop_for_cps * in_rpm_tif,
  by = c("year", "pin")
  ]

  # Get total amount from TIF held for the RPM project, which is 80% of the
  # funds remaining after the CPS cut
  dt[, tax_amt_rpm_tif_to_rpm :=
    (tax_amt_total_to_tif - tax_amt_rpm_tif_to_cps) * 0.8 * in_rpm_tif,
  by = c("year", "pin")
  ]


  # Get total amount from TIF sent BACK to jurisdictions. Need to divvy up
  # the amount that would be sent to CPS proportionally among other bodies
  # using each agency's tax rate
  dt[, tax_amt_rpm_tif_back_to_jur_total :=
    sum((tax_amt_total_to_tif - tax_amt_rpm_tif_to_cps
      - tax_amt_rpm_tif_to_rpm)),
  by = c("year", "pin")
  ]
  dt[!(is_cps_agency), tax_amt_rpm_tif_back_to_jur_dist :=
    agency_tax_rate / (sum(agency_tax_rate * !is_cps_agency)) * in_rpm_tif,
  by = c("year", "pin")
  ]
  dt[(is_cps_agency), tax_amt_rpm_tif_back_to_jur_dist := 0]
  dt[, tax_amt_rpm_tif_back_to_jur :=
    tax_amt_rpm_tif_back_to_jur_total * tax_amt_rpm_tif_back_to_jur_dist,
  by = c("year", "pin")
  ]
  dt[, tax_amt_final :=
    round(
      tax_amt_post_exemptions - tax_amt_total_to_tif +
        tax_amt_rpm_tif_back_to_jur,
      2
    ),
  by = c("year", "pin")
  ]
  dt[, tax_amt_total_to_tif :=
    round(tax_amt_total_to_tif - tax_amt_rpm_tif_back_to_jur, 2),
  by = c("year", "pin")
  ]

  # Set key columns for final table
  data.table::setkeyv(dt, cols = c("year", "pin", "agency_num"))

  # Remove extraneous columns if simplify is TRUE
  if (simplify) {
    return(dt[, c(
      "year", "pin", "tax_code", "av", "eav",
      "agency_num", "agency_name", "agency_tax_rate",
      "tax_amt_post_exemptions", "tax_amt_total_to_tif",
      "tax_amt_final"
    )])
  } else {
    data.table::setcolorder(
      dt,
      neworder = c(
        "year", "pin", "tax_code", "class", "av", "eav",
        "exe_homeowner", "exe_senior", "exe_freeze", "exe_longtime_homeowner",
        "exe_disabled", "exe_vet_returning", "exe_vet_dis_lt50",
        "exe_vet_dis_50_69", "exe_vet_dis_ge70", "exe_abate", "tif_agency",
        "tif_share", "in_rpm_tif", "agency_num", "agency_name", "total_ext",
        "total_eav", "agency_tax_rate", "tax_amt_exempt",
        "tax_amt_pre_exemptions", "tax_amt_post_exemptions",
        "tax_amt_total_to_tif", "is_cps_agency", "tax_rate_for_cps",
        "tax_prop_for_cps", "tax_amt_rpm_tif_to_cps", "tax_amt_rpm_tif_to_rpm",
        "tax_amt_rpm_tif_back_to_jur_total", "tax_amt_rpm_tif_back_to_jur_dist",
        "tax_amt_rpm_tif_back_to_jur", "tax_amt_final"
      )
    )
    return(dt)
  }
}
