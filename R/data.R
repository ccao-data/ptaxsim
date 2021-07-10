#' Assessed values (AVs), exemptions, and tax codes by PIN and year
#'
#' @description PIN-level assessments and exemptions pulled from internal CCAO
#'   databases. These values are used in lookup functions and when calculating
#'   tax bills.
#'
#'   Note that exemption columns store the *amount* of the exemption, rather
#'   than a binary did/did not receive indicator. Amounts are recorded in *EAV*,
#'   rather than the tax-value of the exemption.
#'
#'   Also contains each PIN's tax code, which defines the unique combination of
#'   taxing districts for a given property/location. All properties in a tax
#'   code will share the same tax districts and rates.
#'
#'   New tax codes are created when new taxing districts (including TIFS and
#'   SSAs) are established.
#'
#' @format A data frame containing 11233251 rows and 15 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{pin}{Property index number identifying a specific parcel or unit}
#'   \item{tax_code}{5-digit tax code}
#'   \item{class}{Property class}
#'   \item{av}{Assessed value}
#'   \item{exe_homeowner}{Homeowner exemption}
#'   \item{exe_senior}{Senior exemption}
#'   \item{exe_freeze}{Senior freeze exemption}
#'   \item{exe_longtime_homeowner}{Longtime homeowner exemption}
#'   \item{exe_disabled}{Persons with disabilities exemption}
#'   \item{exe_vet_returning}{Returning veterans exemption}
#'   \item{exe_vet_dis_lt50}{Veteran less than 50\% disability}
#'   \item{exe_vet_dis_50_69}{Veteran greater than 50\% and
#'   less than 69\% disability}
#'   \item{exe_vet_dis_ge70}{Veteran greater than 70\% disability}
#'   \item{exe_abate}{Abatement exemption}
#' }
#'
#' @source CLERKVALUES AS/400 table (or equivalent SQL table)
"av_exe_and_tax_code_by_pin"


# nolint start
#' State equalization factor by year
#'
#' @description The state multiplier/equalization factor is calculated by the
#'   Illinois Department of Revenue. It is a multiplier specifically for Cook
#'   County assessments. Cook County assesses property between 10 and 25% of
#'   fair market value, while most other counties assess at the state-mandated
#'   33.33%.
#'
#'   The equalizer multiplies Cook County aggregate assessed values to bring
#'   them up to the state-mandated 33.33% level of assessment.
#'
#' @format A data frame containing 41 rows and 2 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{equalization_factor}{State multiplier/equalization factor}
#' }
#'
#' @source \url{https://www.cookcountyclerkil.gov/publications/equalization-factor-overview-1973-current-0}
"equalization_factors"
# nolint end


# nolint start
#' Levy and aggregate (base) equalized assessed value by tax agency by year
#'
#' @description Cook County is made up of numerous independent taxing bodies
#'   (AKA agencies or districts) that all receive funding from property taxes.
#'   The amount each district receives is determined by its levy. The pool of
#'   available value each district can tax is known as its base/total EAV.
#'
#'   Each district's individual tax rate is determined by dividing its levy by
#'   its base EAV.
#'
#' @format A data frame containing 5921 rows and 6 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{agency}{Tax district/agency ID number}
#'   \item{agency_name}{Tax district/agency name}
#'   \item{agency_rate}{Agency tax rate for this year. Equal to levy/base}
#'   \item{total_eav}{Total equalized assessed value (EAV) in the taxing
#'   district. Also known as the base}
#'   \item{total_levy}{Total levy amount the taxing district requested}
#' }
#'
#' @source \url{https://www.cookcountyclerkil.gov/service/tax-extension-and-rates}
"levy_and_total_eav_by_agency"
# nolint end


#' Detailed data from real sample tax bills
#'
#' @description Data frame of the summary table from each tax bill in
#'   \code{data-raw/}. Each bill is scanned with Tabula and then formatted to
#'   match the output of \code{\link{tax_bill}}. The line item values are used
#'   for unit/integration testing.
#'
#' @format A data frame containing 272 rows and 11 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{pin}{Property index number identifying a specific parcel or unit}
#'   \item{class}{Property class}
#'   \item{agency}{Tax district/agency ID number}
#'   \item{agency_name}{Tax district/agency name. NOTE: This may be different
#'   from the name produced by \code{\link{tax_bill}}, since it is copied
#'   directly from the PDF tax bill}
#'   \item{tax}{Total tax amount after exemptions for the specified agency}
#'   \item{rate}{Total tax rate for the specified agency in this tax code}
#'   \item{percent}{Percent of the total tax bill for this line item}
#'   \item{pension}{Dollar amount from this line item dedicated to pensions}
#'   \item{prev_tax}{Total tax amount after exemptions for the specified agency
#'   from the previous year}
#'   \item{tif_total}{Total amount sent to TIFs for this tax bill}
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
#' @format A data frame containing 22 rows and 17 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{pin}{Property index number identifying a specific parcel or unit}
#'   \item{tax_code}{5-digit tax code}
#'   \item{class}{Property class}
#'   \item{township}{Township name}
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
#'   \item{eav_total}{Total EAV before exemptions}
#'   \item{tax_before_exemptions}{Total tax amount before exemptions}
#'   \item{tax_after_exemptions}{Total tax amount after exemptions}
#' }
#'
#' @source \url{https://www.cookcountytreasurer.com/setsearchparameters.aspx}
"sample_tax_bills_summary"


# nolint start
#' Cook County taxing agency/district by tax code and year
#'
#' @description Each Cook County taxing district (usually) spans multiple tax
#'   codes. Similarly, each tax code usually contains multiple overlapping
#'   taxing districts. This table is a many-to-many lookup of tax codes to
#'   taxing districts.
#'
#'   Tax codes and taxing districts can change year-to-year as new agencies are
#'   created or deprecated.
#'
#' @format A data frame containing 303980 rows and 6 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{tax_code}{5-digit tax code}
#'   \item{agency}{Tax district/agency ID number}
#' }
#'
#' @source \url{https://www.cookcountyclerkil.gov/service/tax-extension-and-rates}
"tax_codes_by_agency"
# nolint end


#' Tax Increment Financing (TIF) district information by tax code and year
#'
#' @description Tax Increment Financing districts (commonly referred to as TIFS)
#'   are a taxing tool used to generate revenue for the economic development of
#'   a specific area.
#'
#'   They work by capping the EAV of an area, then capturing any taxes on value
#'   above the "frozen" EAV (the increment).
#'
#' @format A data frame containing 7792 rows and 19 columns:
#' \describe{
#'   \item{year}{Tax year}
#'   \item{tax_code}{5-digit tax code}
#'   \item{agency}{Tax district/agency ID number of the TIF. TIFs are recorded
#'   as separate agencies within the Clerk's system}
#'   \item{tif_name}{TIF district common name}
#'   \item{tax_code_rate}{Aggregate tax code tax rate, sum of all agency rates
#'   for agencies in the tax code}
#'   \item{tax_code_eav}{Total tax code EAV for the year}
#'   \item{tax_code_frozen_eav}{Total tax code EAV for the year the TIF was
#'   created}
#'   \item{tax_code_increment}{Current tax code EAV above the frozen amount}
#'   \item{tax_code_revenue}{Tax revenue generated for the TIF from the
#'   increment in this tax code for the year}
#'   \item{tax_code_distribution_percent}{The percent of total EAV in the
#'   increment/above the frozen amount. Also known as the TIF share}
#'   \item{tif_total_eav}{Total EAV for the entire TIF for the year}
#'   \item{tif_total_frozen_eav}{Total EAV for the entire TIF for the year
#'   the TIF was created}
#'   \item{tif_total_increment}{Current total TIF EAV above the frozen amount}
#'   \item{tif_total_revenue}{Tax revenue generated for the entire TIF for
#'   the year}
#'   \item{first_year}{Year the TIF was created}
#'   \item{this_year_revenue}{Tax revenue generated for the entire TIF for
#'   the year}
#'   \item{prev_year_revenue}{Tax revenue generated for the entire TIF for
#'   the previous year}
#'   \item{tif_new_this_year}{Boolean for first year of the TIF}
#'   \item{tif_cancelled_this_year}{Boolean for final year of the TIF. Some TIFS
#'   are cancelled early}
#' }
#'
#' @source \url{https://www.cookcountyclerkil.gov/service/tif-reports}
"tifs"
