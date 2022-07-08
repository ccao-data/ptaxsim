library(arrow)
library(bit64)
library(dplyr)
library(purrr)
library(readr)
library(readxl)
library(snakecase)
library(stringr)
library(tidyr)
options(scipen = 99)

calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# The levy of each jurisdiction is reported by the Cook County Clerk's Office.
# URL here: https://www.cookcountyclerkil.gov/service/tax-extension-and-rates

# Using the provided agency rate reports (from 2006 onward) for the sake of
# simplicity. These files also contain the total Cook County Equalized
# Assessed Value by agency

remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path_agency <- file.path(
  remote_bucket, "agency", "part-0.parquet"
)
remote_path_agency_info <- file.path(
  remote_bucket, "agency_info", "part-0.parquet"
)
remote_path_agency_fund <- file.path(
  remote_bucket, "agency_fund", "part-0.parquet"
)
remote_path_agency_fund_info <- file.path(
  remote_bucket, "agency_fund_info", "part-0.parquet"
)

# Get a list of all levy report spreadsheets
file_names <- list.files(
  path = "data-raw/agency",
  pattern = "*.xls*",
  full.names = TRUE
)





# agency_fund ------------------------------------------------------------------

# Load the detail sheet from each agency file. This includes the levy and rate
# for each fund
agency_fund <- map_dfr(file_names, function(file) {
  message("Reading: ", file)
  readxl::read_xlsx(file, sheet = 2) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    mutate(across(c(contains("agency")), as.character)) %>%
    # Renaming columns since they change names across years
    rename_with(~ str_remove(.x, "tax_"), any_of("tax_year")) %>%
    rename_with(~ str_remove(.x, "_18"), ends_with("_18")) %>%
    rename_with(~ str_remove(.x, "_num"), starts_with("agency")) %>%
    rename_with(~"levy", any_of(c("levy", "fund_levy"))) %>%
    rename_with(~"loss_pct", any_of(c(
      "loss", "loss_percent", "fund_loss"
    ))) %>%
    rename_with(~"levy_plus_loss", any_of(c(
      "levy_and_loss", "fund_levy_plus_loss"
    ))) %>%
    rename_with(~"rate_ceiling", any_of(c(
      "ceiling", "rate_ceiling", "fund_rate_ceiling"
    ))) %>%
    rename_with(~"max_levy", any_of(c(
      "max_levy", "fund_max_levy"
    ))) %>%
    rename_with(~"prelim_rate", any_of(c(
      "prelim_rate", "fund_prelim_rate"
    ))) %>%
    rename_with(~"ptell_reduced_levy", any_of(c(
      "ptell_levy", "fund_ptell_levy", "ptell_red_levy",
      "fund_ptell_reduced_levy"
    ))) %>%
    rename_with(~"ptell_reduced_ind", any_of(c(
      "ptell_ind", "reduction_ind", "rate_reduction_indicator",
      "reduction_indicator"
    ))) %>%
    rename_with(~"final_levy", any_of(c(
      "final_levy", "fund_final_levy"
    ))) %>%
    rename_with(~"final_rate", any_of(c(
      "fund_rate", "final_rate", "fund_final_rate", "final_fund_rate"
    ))) %>%
    select(
      year,
      agency_num = agency, fund_num = fund, fund_name, levy, loss_pct,
      levy_plus_loss, rate_ceiling, max_levy, prelim_rate, ptell_reduced_levy,
      ptell_reduced_ind, final_levy, final_rate
    ) %>%
    mutate(across(year, as.character))
}) %>%
  mutate(
    agency_num = str_pad(agency_num, 9, "left", "0"),
    fund_num = str_pad(fund_num, 3, "left", "0"),
    loss_pct = ifelse(
      year == 2011 & agency_num == "030380104" & fund_num == "001",
      0,
      loss_pct
    ),
    loss_pct = ifelse(levy == levy_plus_loss & is.na(loss_pct), 0, loss_pct),
    loss_pct = ifelse(
      levy != levy_plus_loss & is.na(loss_pct),
      (levy_plus_loss - levy) / levy,
      loss_pct
    ),
    loss_pct = replace_na(loss_pct, 0),
    loss_pct = loss_pct / 100,
    
    # Backout original levy if missing based on levy + loss
    levy = ifelse(
      is.na(levy),
      round(levy_plus_loss / (1 + loss_pct), 0),
      levy
    ),
    rate_ceiling = replace_na(rate_ceiling, 0),
    rate_ceiling = ifelse(final_rate == 0 & final_levy == 0, 0, rate_ceiling),
    ptell_reduced_levy = na_if(ptell_reduced_levy, 0),
    ptell_reduced_ind = ptell_reduced_ind == "*",
    ptell_reduced_ind = replace_na(ptell_reduced_ind, FALSE),
    final_rate = ifelse(
      agency_num == "050200000" & fund_num == "202" & year == 2006,
      0,
      final_rate
    )
  ) %>%
  arrange(year, agency_num, fund_num) %>%
  mutate(
    across(
      c(levy, levy_plus_loss, max_levy, ptell_reduced_levy, final_levy),
      as.integer64
    ),
    across(
      c(loss_pct, rate_ceiling, prelim_rate, final_rate),
      as.double
    )
  ) %>%
  arrange(year, agency_num, fund_num)




# agency_fund_info -------------------------------------------------------------

# Breakout the fund names into their own table
agency_fund_info <- agency_fund %>%
  group_by(fund_num) %>%
  summarise(fund_name = calc_mode(fund_name)) %>%
  ungroup() %>%
  arrange(fund_num) %>%
  mutate(
    fund_name = str_trim(str_squish(fund_name)),
    capped_ind = !fund_num %in% c(
      "003", "027", "054", "182", "202", "259", "261", "284", "286", "287",
      "293", "294", "315", "320", "321", "322", "351", "400", "401", "402",
      "404", "405", "406", "407"
    )
  )

# Drop names from the fund table since they're now stored separately
agency_fund <- agency_fund %>%
  select(-fund_name)

# Write the resulting datasets to S3
arrow::write_parquet(
  x = agency_fund,
  sink = remote_path_agency_fund,
  compression = "zstd"
)
arrow::write_parquet(
  x = agency_fund_info,
  sink = remote_path_agency_fund_info,
  compression = "zstd"
)




# agency -----------------------------------------------------------------------

# Load the overview of each agency file. This includes the agency name, total
# EAV, final extension, and much more
agency <- map_dfr(file_names, function(file) {
  message("Reading: ", file)
  readxl::read_xlsx(file) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    mutate(
      across(
        c("cpi", contains("reduction_percent")),
        as.numeric
      ),
      across(
        c(
          contains("year"), contains("agency"),
          contains("reduction_type"), contains("agg_ext_base")
        ),
        as.character
      )
    ) %>%
    # Renaming columns since they change names across years
    rename_with(~ str_remove(.x, "tax_"), any_of("tax_year")) %>%
    rename_with(~ str_remove(.x, "_18"), ends_with("_18")) %>%
    rename_with(~ str_remove(.x, "_num"), starts_with("agency")) %>%
    rename_with(~ str_replace(.x, "county", "cook"), any_of("county_eav")) %>%
    rename_with(~"agg_ext_base_year", any_of(c(
      "agg_ext_base_year", "agg_ext_base_yr", "agg_ext_base",
      "prior_year", "agg_yr"
    ))) %>%
    rename_with(~"lim_numerator", any_of(c(
      "lim_numerator", "prior_agg_ext"
    ))) %>%
    rename_with(~"lim_denominator", any_of(c(
      "lim_denominator", "agg_ext_x_cpi"
    ))) %>%
    rename_with(~"prior_eav", any_of(c(
      "prior_eav", "prior_eav_np_total"
    ))) %>%
    rename_with(~"curr_new_prop", any_of(c(
      "current_new_prop", "new_prop", "curr_new_prop", "current_new_property"
    ))) %>%
    rename_with(~"lasalle_eav", any_of(c("lasalle_eav", "la_salle_eav"))) %>%
    rename_with(~"mchenry_eav", any_of(c("mc_henry_eav", "mchency_eav"))) %>%
    rename_with(
      ~"reduction_type",
      any_of(c("reduction_type", "reduction"))
    ) %>%
    rename_with(~"reduction_pct", any_of(c(
      "reduction_percent", "reduction_factor", "clerk_reduction_factor"
    ))) %>%
    rename_with(~"total_non_cap_ext", any_of(c(
      "total_non_cap_ext", "final_non_cap_ext"
    ))) %>%
    rename_with(~"total_ext", any_of(c("total_ext", "final_ext"))) %>%
    
    # Select, order, and rename columns
    select(
      year, agency_num = agency, agency_name, home_rule_ind, agg_ext_base_year,
      lim_numerator, lim_denominator, lim_rate, prior_eav, curr_new_prop,
      ends_with("_eav"), percent_burden,
      starts_with("grand_total_"),
      reduction_type, reduction_pct, total_non_cap_ext,
      any_of("total_ext")
    ) %>%
    rename_with(~ paste0("cty_", .x), ends_with("_eav")) %>%
    select(-any_of("cty_total_eav")) %>%
    rename(
      prior_eav = cty_prior_eav,
      cty_total_eav = cty_overall_eav,
      pct_burden = percent_burden
    ) %>%
    rename_with(
      ~ gsub("grand_total_", "total_", .x),
      starts_with("grand_total_")
    ) %>%
    relocate(total_ext, .after = everything())
}) %>%
  mutate(
    agency_num = str_pad(agency_num, 9, "left", "0"),
    agency_name = str_trim(str_squish(agency_name)),
    agg_ext_base_year = as.integer(agg_ext_base_year),
    agg_ext_base_year = na_if(agg_ext_base_year, 0),
    home_rule_ind = ifelse(home_rule_ind %in% c("Y", "No PTELL"), TRUE, FALSE),
    home_rule_ind = replace_na(home_rule_ind, FALSE),
    across(
      c(
        starts_with("lim_"), "agg_ext_base_year", "total_reduced_levy",
        starts_with("reduction_")
      ),
      ~ ifelse(home_rule_ind, NA, .x)
    ),
    # One row is missing a Cook EAV value. Fill manually from prior year
    cty_cook_eav = ifelse(
      agency_num == "030580002" & year == "2006",
      0,
      cty_cook_eav
    ),
    across(starts_with("cty_"), replace_na, 0),
    # Make all percentages decimals
    across(
      c(pct_burden, reduction_pct),
      ~ ifelse(year != 2017, .x / 100, .x)
    ),
    reduction_type = ifelse(
      !toupper(reduction_type) %in% c("NO REDUCTION", "NONE"),
      toupper(reduction_type),
      NA_character_
    )
  ) %>%
  arrange(year, agency_num) %>%
  # Coerce columns to expected types
  mutate(
    across(c(year), as.character),
    across(
      c(lim_numerator, lim_denominator, prior_eav:cty_total_eav,
        total_levy, total_max_levy, total_reduced_levy, total_final_levy),
      as.integer64
    ),
    across(
      c(lim_rate, pct_burden, total_prelim_rate, total_final_rate,
        reduction_pct, total_non_cap_ext, total_ext),
      as.double
    )
  )

# Tax year 2013 is missing the total levy columns from its overview sheet, but
# we can fill it in by joining the totals from each fund sheet
agency_fund_2013 <- agency_fund %>%
  filter(year == 2013) %>%
  group_by(agency_num) %>%
  summarize(
    total_levy = sum(levy),
    total_max_levy = sum(max_levy),
    total_prelim_rate = ceiling(sum(prelim_rate) * 1000) / 1000,
    total_reduced_levy = sum(ptell_reduced_levy),
    total_final_levy = sum(final_levy),
    total_final_rate = sum(final_rate)
  )

agency_2013 <- agency %>%
  filter(year == 2013) %>%
  select(-c(
    total_levy, total_max_levy, total_prelim_rate,
    total_reduced_levy, total_final_levy, total_final_rate
  )) %>%
  left_join(agency_fund_2013, by = "agency_num")

agency <- agency %>%
  filter(year != 2013) %>%
  bind_rows(agency_2013) %>%
  arrange(year, agency_num)




# agency_info ------------------------------------------------------------------

# Create a separate table containing only agency names and types
agency_info <- agency %>%
  group_by(agency_num) %>%
  summarise(agency_name_original = calc_mode(agency_name)) %>%
  ungroup() %>%
  mutate(
    # Clean up, standardize, and length district names
    an = str_remove_all(agency_name_original, "#|NO\\.|\\."),
    an = str_replace_all(an, "SPEC\\s", "SPECIAL "),
    an = str_replace_all(an, "DIST\\s", "DISTRICT "),
    an = str_replace_all(an, "DIST$|DST$", "DISTRICT"),
    an = str_replace_all(an, "SERV\\s", "SERVICE "),
    an = str_replace_all(an, "SSA\\s", "SPECIAL SERVICE AREA "),
    an = str_replace_all(an, "COMM\\s", "COMMUNITY "),
    an = str_replace_all(an, "FD$", "FUND"),
    an = str_replace_all(an, "(?<=[A-Z0-9])(&)(?=[A-Z0-9])", " & "),
    an = str_replace_all(an, "(?<=[A-Z0-9])(\\/)(?=[A-Z0-9])", " / "),
    an = str_replace_all(an, "(?<=[A-Z0-9])(-)(?=[A-Z0-9])", " - "),
    an = str_replace_all(an, "HLTH\\s", "HEALTH "),
    an = str_replace_all(an, "FAC\\s", "FACILITIES "),
    an = str_replace_all(an, "TWP\\s|TWNSHP\\s", "TOWNSHIP "),
    an = str_replace_all(an, "MOSQ\\s", "MOSQUITO "),
    an = str_replace_all(an, "GR CHGO", "GREATER CHICAGO"),
    an = str_replace_all(an, "VIL\\s", "VILLAGE "),
    an = str_replace_all(an, "MENT\\s", "MENTAL "),
    an = str_replace_all(an, "HTS\\s", "HEIGHTS "),
    an = str_replace_all(an, "SCH\\s", "SCHOOL "),
    an = str_replace_all(an, "H\\sS\\s|HS\\s", "HIGH SCHOOL "),
    an = str_replace_all(an, "C\\C\\s", "CC "),
    an = str_replace_all(
      an, "(SPECIAL SERVICE)(?=\\s[0-9])", "SPECIAL SERVICE AREA"
    ),
    an = str_trim(str_squish(toupper(an)))
  )


# Write both data sets to S3
arrow::write_parquet(
  x = agency,
  sink = remote_path_agency,
  compression = "zstd"
)
