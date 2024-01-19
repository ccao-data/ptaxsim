#' Calculate an approximate Cook County property tax bill
#'
#' @description This function calculates a Cook County property tax bill using
#'   levy, base, and TIF information from the Cook County Clerk and Assessor.
#'   It allows users to simulate different tax scenarios by changing the input
#'   data. For example, this function can be used to answer the following
#'   questions:
#'
#'   - What would my tax bill be if my assessed value was $10K lower?
#'   - What would my tax bill be if Chicago increased its levy by 5%?
#'   - What would my tax bill be if my property was in a different neighborhood?
#'   - What would my tax bill be if a large new development was added to
#'     my neighborhood?
#'
#'   Most of the data necessary to answer these questions is built into the
#'   included database. Data for the most current tax year is usually available
#'   the following year (i.e. 2021 data is available in 2022). If needed, users
#'   can supply current tax year data manually. See vignettes for more
#'   information.
#'
#' @details Note that all vector inputs (suffixed with \code{_vec}) have two
#'   input modes:
#'
#'   1. If the input vectors are the same length, pairwise tax bills are
#'   returned (each PIN, year, and tax code combination returns 1 bill).
#'   2. If the input vectors are different lengths, the Cartesian product of
#'   the vectors is returned (5 years and 10 PINs will return all 5 years of
#'   bills for each PIN).
#'
#'   The district-level tax amounts returned by this function will *not*
#'   perfectly match the amounts on real tax bills. This is due to rounding
#'   and truncating that occurs in the real system. Most estimated amounts will
#'   still be within a few dollars of the amounts on real bills.
#'
#'   PIN and year combinations not found in the database will be silently
#'   dropped from the output.
#'
#' @param year_vec Numeric vector of tax years for which to return bills.
#' @param pin_vec Character vector of 14-digit Property Index Numbers (PINs)
#'   with no dashes or spaces.
#' @param tax_code_vec Character vector of 5-digit Cook County tax codes. These
#'   codes are included on individual property tax bills. If missing, the
#'   \code{\link{lookup_tax_code}} function is used to retrieve tax codes
#'   based on \code{year_vec} and \code{pin_vec}.
#' @param agency_dt \code{data.table} containing the levy and base amount for
#'   each taxing district in the specified tax code. Data *must* be identical
#'   to the format returned by \code{\link{lookup_agency}}. If missing,
#'   \code{\link{lookup_agency}} is used to retrieve each tax district's
#'   levy and base based on \code{year_vec} and \code{tax_code_vec}.
#' @param pin_dt \code{data.table} containing the exemptions and assessed value
#'   specific to each PIN and year. Data *must* be identical to the format
#'   returned by \code{\link{lookup_pin}}. If missing, \code{\link{lookup_pin}}
#'   is used to retrieve each PIN's information based on \code{year_vec}
#'   and \code{pin_vec}.
#' @param tif_dt \code{data.table} containing any TIF applicable to the
#'   specified tax code. Is an empty \code{data.table} if no TIF exists for
#'   the property. Data *must* be identical to the format returned by
#'   \code{\link{lookup_tif}}. If missing, \code{\link{lookup_tif}} is
#'   used to retrieve the tax code's TIF share based on \code{year_vec}
#'   and \code{tax_code_vec}.
#' @param simplify Default \code{TRUE}. Boolean to keep only the columns that
#'   appear on a real tax bill. Additionally, collapses the TIF output column
#'   \code{final_tax_to_tif} to a line-item, similar to the format on a real
#'   tax bill.
#'
#' @return A \code{data.table} which contains a tax bill for each specified PIN
#'   and year. Each tax bill is broken out by taxing district, meaning there is
#'   a row for each district relevant to each PIN. Most PINs have 10-15
#'   districts (rows) associated with each year.
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
                     agency_dt = lookup_agency(year_vec, tax_code_vec),
                     pin_dt = lookup_pin(year_vec, pin_vec),
                     tif_dt = lookup_tif(year_vec, tax_code_vec),
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

  # Check input lengths are either Cartesian product OR all equal to each other
  vecs_len_diff <- length(year_vec) != length(pin_vec)
  vecs_len_exp <- length(unique(year_vec)) *
    length(unique(pin_vec)) == length(tax_code_vec)
  vecs_len_eq <- (
    length(year_vec) == length(pin_vec) &
      length(pin_vec) == length(tax_code_vec)
  )
  if (vecs_len_diff) {
    if (anyDuplicated(year_vec) || anyDuplicated(pin_vec)) {
      stop(
        "Input vectors year_vec and pin_vec must contain only unique values ",
        "if they are not equal length"
      )
    }
  } else if (!((vecs_len_diff && vecs_len_exp) || vecs_len_eq)) {
    stop(
      "Input vectors must be equal length OR tax_code_vec must be the ",
      "Cartesian product of year_vec and pin_vec"
    )
  }

  # Input checking to ensure data.tables have expected structure (col name and
  # data type)
  stopifnot(
    check_agency_dt_str(agency_dt),
    check_pin_dt_str(pin_dt),
    check_tif_dt_str(tif_dt)
  )

  # Create data.table from inputs. Use the Cartesian product if the inputs are
  # different lengths
  tax_code <- NULL
  if (length(year_vec) != length(pin_vec)) {
    dt <- data.table::CJ("year" = year_vec, "pin" = pin_vec, sorted = FALSE)
  } else {
    dt <- data.table::data.table("year" = year_vec, "pin" = pin_vec)
  }
  dt[, tax_code := tax_code_vec]
  data.table::setDT(dt, key = c("year", "pin", "tax_code"))

  # Join PIN-level data (exemptions, eav) to the input data. eav_total is equal
  # to the sum of all exemptions
  exe_total <- .SD <- class <- av <- eav <- NULL
  year <- pin <- i.av <- i.eav <- i.class <- NULL
  exe_cols <- paste0("i.", names(pin_dt)[startsWith(names(pin_dt), "exe_")])
  dt[
    pin_dt,
    on = .(year, pin),
    c("av", "eav", "class", "exe_total") := .(
      i.av, i.eav, i.class,
      rowSums(data.table::as.data.table(mget(exe_cols)))
    )
  ]
  dt[class == "239", eav := av] # farmland is taxed on AV, not EAV

  # Fetch TIF for a given tax code. There should only be one TIF per tax code (
  # with a few exceptions)
  i.agency_num <- i.agency_name <- i.tif_share <- NULL
  dt[
    tif_dt[order(year, tax_code, agency_num), ],
    on = .(year, tax_code),
    c("tif_agency_num", "tif_agency_name", "tif_share") :=
      .(i.agency_num, i.agency_name, i.tif_share)
  ]

  # Add an indicator for when the PIN is in the special Red-Purple Modernization
  # TIF, which has different rules than other TIFs
  in_rpm_tif <- tif_agency_num <- tif_agency_name <- tif_share <- NULL
  dt[, in_rpm_tif := any(tif_agency_num == "030210900"), by = .(year, pin)]
  dt[is.na(in_rpm_tif), in_rpm_tif := FALSE]
  data.table::setnafill(dt, "const", 0, cols = "tif_share")

  # Fetch levy for all agencies of a tax code, this will expand the number of
  # rows from 1 per PIN per year, to N per PIN per year, where N is the number
  # of agencies associated with a PIN's tax code
  agency_num <- agency_tax_rate <- agency_total_ext <- agency_total_eav <-
    tax_amt_exe <- tax_amt_pre_exe <- tax_amt_post_exe <-
    final_tax_to_tif <- NULL
  dt <- merge(
    x = dt,
    y = agency_dt[order(year, tax_code, agency_num), ],
    by = c("year", "tax_code"),
    allow.cartesian = TRUE
  )

  # Calculate the exemption effect by subtracting the exempt amount from
  # the total taxable EAV
  dt[, agency_tax_rate := agency_total_ext / as.numeric(agency_total_eav)]
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

  if (simplify) {
    # Collapse per-district TIF amounts into a single row, just like on a
    # real tax bill
    tif_row <- dt[
      !is.na(tif_agency_num),
      .(final_tax = sum(final_tax_to_tif)),
      by = .(
        year, pin, class, tax_code, av, eav,
        tif_agency_num, tif_agency_name
      )
    ]
    data.table::setnames(
      tif_row,
      c("tif_agency_num", "tif_agency_name"),
      c("agency_num", "agency_name")
    )
    tif_row[
      ,
      c("agency_major_type", "agency_minor_type", "agency_tax_rate") :=
        list("MUNICIPALITY/TOWNSHIP", "TIF", 0.0)
    ]

    # Keep only necessary columns, then merge with the TIF row(s)
    drop_cols <- c(
      "exe_total", "agency_total_ext", "agency_total_eav", "tax_amt_exe",
      "tax_amt_pre_exe", "tax_amt_post_exe", "tif_agency_num",
      "tif_agency_name", "tif_share", "rpm_tif_to_cps", "rpm_tif_to_rpm",
      "rpm_tif_to_dist", "final_tax_to_tif"
    )
    dt[, (drop_cols) := NULL]
    data.table::setnames(dt, "final_tax_to_dist", "final_tax")
    dt <- rbind(dt, tif_row)
    data.table::setcolorder(
      dt,
      neworder = c(
        "year", "pin", "class", "tax_code", "av", "eav", "agency_num",
        "agency_name", "agency_major_type", "agency_minor_type",
        "agency_tax_rate", "final_tax"
      )
    )
  } else {
    data.table::setcolorder(
      dt,
      neworder = c(
        "year", "pin", "class", "tax_code", "av", "eav", "exe_total",
        "agency_num", "agency_name", "agency_major_type", "agency_minor_type",
        "agency_total_ext", "agency_total_eav", "agency_tax_rate",
        "tax_amt_exe", "tax_amt_pre_exe", "tax_amt_post_exe", "tif_agency_num",
        "tif_agency_name", "tif_share", "rpm_tif_to_cps", "rpm_tif_to_rpm",
        "rpm_tif_to_dist", "final_tax_to_tif", "final_tax_to_dist"
      )
    )
  }

  data.table::setkey(dt, year, pin, agency_num)
  return(dt)
}
