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
#'   (named \code{final_tax_to_tif}).
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
      expand.grid("year" = year_vec, "pin" = pin_vec, stringsAsFactors = FALSE)
    )
  } else {
    dt <- data.table::data.table("year" = year_vec, "pin" = pin_vec)
  }
  dt[, tax_code := tax_code_vec]
  data.table::setDT(dt, key = c("year", "pin"))

  # Shink the PIN data prior to join by collapsing exemptions into a total
  # exempt amount
  exe_total <- .SD <- class <- av <- eav <- NULL
  exe_cols <- names(pin_df)[startsWith(names(pin_df), "exe_")]
  pin_df[, exe_total := rowSums(.SD), .SDcols = exe_cols]
  pin_df[class == "239", eav := av] # farmland is taxed on AV, not EAV
  pin_df[, (exe_cols) := NULL]

  # Join PIN-level data (exemptions, eav) to the input data
  year <- pin <- i.av <- i.eav <- i.class <- i.exe_total <- NULL
  dt[pin_df, on = .(year, pin), c("av", "eav", "class", "exe_total") :=
    .(i.av, i.eav, i.class, i.exe_total)]

  # Fetch TIF for a given tax code. There should only be one TIF per tax code (
  # with a few exceptions)
  i.agency_num <- i.tif_share <- NULL
  dt[tif_df, on = .(year, tax_code), c("tif_agency_num", "tif_share") :=
    .(i.agency_num, i.tif_share)]

  # Add an indicator for when the PIN is in the special Red-Purple Modernization
  # TIF, which has different rules than other TIFs
  in_rpm_tif <- tif_agency_num <- tif_share <- NULL
  dt[, in_rpm_tif := any(tif_agency_num == "030210900"), by = .(year, pin)]
  dt[is.na(in_rpm_tif), in_rpm_tif := FALSE]
  data.table::setnafill(dt, "const", 0, cols = "tif_share")

  # Fetch levy for all agencies of a tax code, this will expand the number of
  # rows from 1 per PIN per year, to N per PIN per year, where N is the number
  # of agencies associated with a PIN's tax code
  dt <- merge(dt, agency_df, by = c("year", "tax_code"), allow.cartesian = TRUE)

  # Calculate the exemption effect by subtracting the exempt amount from
  # the total taxable EAV
  agency_num <- agency_tax_rate <- agency_total_ext <- agency_total_eav <-
    tax_amt_exe <- tax_amt_pre_exe <- tax_amt_post_exe <-
    final_tax_to_tif <- NULL
  dt[, agency_tax_rate := agency_total_ext / agency_total_eav]
  dt[, tax_amt_exe := exe_total * agency_tax_rate]
  dt[, tax_amt_pre_exe := round(eav * agency_tax_rate, 2)]
  dt[, tax_amt_post_exe := round(tax_amt_pre_exe - tax_amt_exe, 2)]
  dt[tax_amt_post_exe < 0, tax_amt_post_exe := 0]
  dt[, final_tax_to_tif := tax_amt_post_exe * tif_share]

  # Special handling for the Red-Purple Modernization TIF (RPM1). For this TIF
  # specifically:
  # 1. CPS receives their proportionate share of revenue (they ignore the TIF)
  # 2. 80% of the remaining revenue goes to the RPM TIF
  # 3. 20% of the remaining revenue goes to all taxing districts besides CPS

  # Start by calculating the portion that ignores the TIF and goes to CPS
  # Using chaining here to discard unneeded intermediate variables
  crate <- cprop <- rpm_tif_to_cps <- rpm_tif_to_rpm <- rpm_tif_to_dist <- NULL
  dt[
    (in_rpm_tif),
    crate := agency_tax_rate[agency_num == "044060000"],
    by = .(year, pin)
  ][
    (in_rpm_tif),
    cprop := crate / sum(agency_tax_rate),
    by = .(year, pin)
  ][
    (in_rpm_tif),
    rpm_tif_to_cps := final_tax_to_tif * cprop
  ][, c("crate", "cprop") := NULL]

  # Get total amount from TIF held for the RPM project, which is 80% of the
  # funds remaining after the CPS cut
  dt[(in_rpm_tif), rpm_tif_to_rpm := (final_tax_to_tif - rpm_tif_to_cps) * 0.8]

  # Assign the remaining 20% to each district, then divvy up CPS's 20%
  # proportionate to each district's tax rate
  dt[
    (in_rpm_tif),
    rpm_tif_to_dist := (final_tax_to_tif - rpm_tif_to_cps) * 0.2
  ][
    (in_rpm_tif),
    rpm_tif_to_dist :=
      rpm_tif_to_dist + rpm_tif_to_dist[agency_num == "044060000"] *
      (agency_tax_rate / sum(agency_tax_rate[agency_num != "044060000"])),
    by = .(year, pin)
  ]

  # Set CPS share that goes back to districts to 0, since they have their own
  # special distribution
  dt[in_rpm_tif & agency_num == "044060000", rpm_tif_to_dist := 0]

  # Fill RPM distributions with 0 for any non-RPM property
  data.table::setnafill(
    x = dt,
    type = "const",
    fill = 0,
    cols = c("rpm_tif_to_cps", "rpm_tif_to_rpm", "rpm_tif_to_dist")
  )

  # Calculate the final tax amount for districts by subtracting the TIF amount
  final_tax_to_dist <- NULL
  dt[, final_tax_to_dist :=
    round(tax_amt_post_exe - final_tax_to_tif + rpm_tif_to_dist, 2)]
  dt[, final_tax_to_tif := round(final_tax_to_tif - rpm_tif_to_dist, 2)]
  dt[, in_rpm_tif := NULL]
  data.table::setkey(dt, year, pin, agency_num)

  # Remove extraneous columns if simplify is TRUE
  if (simplify) {
    return(dt[, c(
      "year", "pin", "class", "tax_code", "av", "eav",
      "agency_num", "agency_name", "agency_tax_rate",
      "tax_amt_post_exe", "final_tax_to_tif", "final_tax_to_dist"
    )])
  } else {
    data.table::setcolorder(
      dt,
      neworder = c(
        "year", "pin", "class", "tax_code", "av", "eav", "exe_total",
        "agency_num", "agency_name", "agency_total_ext", "agency_total_eav",
        "agency_tax_rate", "tax_amt_exe", "tax_amt_pre_exe", "tax_amt_post_exe",
        "tif_agency_num", "tif_share", "rpm_tif_to_cps", "rpm_tif_to_rpm",
        "rpm_tif_to_dist", "final_tax_to_tif", "final_tax_to_dist"
      )
    )
    return(dt)
  }
}
