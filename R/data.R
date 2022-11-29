#' Detailed data from real sample tax bills
#'
#' @description Data frame of the summary table from each tax bill in
#'   \code{data-raw/}. Each bill is scanned with Tabula and then formatted to
#'   match the output of \code{\link{tax_bill}}. The line item values are used
#'   for unit/integration testing.
#'
#' @format A data frame containing 396 rows and 10 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{pin}{Property index number identifying a specific parcel or unit}
#'   \item{class}{Property class}
#'   \item{agency_num}{Tax district/agency ID number}
#'   \item{agency_name}{Tax district/agency name. NOTE: This may be different
#'   from the name produced by \code{\link{tax_bill}}, since it is copied
#'   directly from the PDF tax bill}
#'   \item{final_tax}{Total tax amount after exemptions for the specified
#'   agency}
#'   \item{rate}{Total tax rate for the specified agency in this tax code}
#'   \item{percent}{Percent of the total tax bill for this line item}
#'   \item{pension}{Dollar amount from this line item dedicated to pensions}
#'   \item{prev_tax}{Total tax amount after exemptions for the specified agency
#'   from the previous year}
#' }
#'
#' @source \url{https://www.cookcountytreasurer.com/setsearchparameters.aspx}
"sample_tax_bills_detail"


#' Summary data from real sample tax bills
#'
#' @description The Cook County Treasurer is responsible for producing the
#'   actual tax bills sent to taxpayers. This package contains a set of real
#'   sample tax bills (stored in \code{/data-raw} as PDFs). This data set
#'   contains the summary statistics and metadata from those tax bills.
#'
#'   Note: All exemption columns (prefixed with \code{exe_}) are booleans
#'   for whether or not that exemption was received.
#'
#' @format A data frame containing 42 rows and 17 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{pin}{Property index number identifying a specific parcel or unit}
#'   \item{tax_code}{5-digit tax code}
#'   \item{class}{Property class}
#'   \item{township_name}{Township name}
#'   \item{in_tif}{Boolean for whether the PIN/tax code is in a Tax Increment
#'   Finance (TIF) district}
#'   \item{in_ssa}{Boolean for whether the PIN/tax code is in a Special Service
#'   Area (SSA)}
#'   \item{exe_homeowner}{Homeowner exemption}
#'   \item{exe_senior}{Senior exemption}
#'   \item{exe_freeze}{Senior freeze exemption}
#'   \item{exe_longtime_homeowner}{Longtime homeowner exemption}
#'   \item{exe_disabled}{Persons with disabilities exemption}
#'   \item{exe_vet_returning}{Veterans exemption}
#'   \item{exe_vet_disabled}{Veterans disability exemption}
#'   \item{eav}{Total EAV before exemptions}
#'   \item{tax_amt_pre_exe}{Total tax amount before exemptions}
#'   \item{tax_amt_post_exe}{Total tax amount after exemptions}
#' }
#'
#' @source \url{https://www.cookcountytreasurer.com/setsearchparameters.aspx}
"sample_tax_bills_summary"
