# =============================================================================
# Vacant land LOA counterfactual (PTAXSIM)
# Run top to bottom
# =============================================================================
library(ptaxsim)
library(DBI)
library(data.table)
library(bit64)

# --- 0. Parameters (the only things you should need to change) ---------------
tax_year                  <- 2024
counterfactual_class_list <- c("100", "190")
counterfactual_LOA_change <- 0.25 / 0.10          # = 2.5, multiplier to convert 10% LOA --> 25% LOA

# Quick test mode: set to a few tax codes (e.g. c("72082", "73105")) to only
# calculate bills for those. Set to NULL to run every PIN in Cook County.
# (Agency totals below always use ALL vacant PINs, so results stay correct.)
# run_tax_codes <- c("72082", "73105")
run_tax_codes <- NULL 

chunk_size <- 25000   # PINs per tax_bill() call

ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "./ptaxsim-2024.0.0.db")

# --- 1. Vacant PINs: actual EAV, TIF frozen EAV, counterfactual EAV ----------
pins_all <- setDT(DBI::dbGetQuery(
  ptaxsim_db_conn,
  paste0("SELECT pin, class, tax_code_num, tax_bill_total FROM pin WHERE year = ", tax_year)
))
pins_all[, tax_code := as.character(tax_code_num)]

vac <- pins_all[class %in% counterfactual_class_list, .(pin, class, tax_code)]
cat("Vacant PINs:", nrow(vac), "\n")

vac_dt <- lookup_pin(tax_year, vac$pin)
vac <- merge(vac, vac_dt[, .(pin, av, eav)], by = "pin")

# Frozen (base) EAV for PINs inside a TIF
tif_pin <- setDT(DBI::dbGetQuery(
  ptaxsim_db_conn,
  paste0("SELECT pin, pin_frozen_eav FROM pin_tif_distribution WHERE year = ", tax_year)
))
tif_pin <- tif_pin[pin %in% vac$pin]
if (anyDuplicated(tif_pin$pin)) {
  warning("Some vacant PINs have more than one TIF row; check how to handle these.")
  tif_pin <- tif_pin[, .(pin_frozen_eav = max(pin_frozen_eav)), by = pin]
}
vac <- merge(vac, tif_pin, by = "pin", all.x = TRUE)
vac[, in_tif := !is.na(pin_frozen_eav)]

k <- counterfactual_LOA_change
vac[, av_cf  := as.integer(round(av  * k))]
vac[, eav_cf := as.integer(round(eav * k))]

# Gained EAV that goes into the agency tax BASE:
#   non-TIF PIN: all of it
#   TIF PIN:     only the part that stays below the frozen EAV (the rest is increment)
vac[, base_old := fifelse(in_tif, pmin(eav,    pin_frozen_eav), as.numeric(eav))]
vac[, base_new := fifelse(in_tif, pmin(eav_cf, pin_frozen_eav), as.numeric(eav_cf))]
vac[, base_gain := base_new - base_old]

# Share of each PIN's EAV above the frozen base (what goes to the TIF)
vac[, tif_share_old := fifelse(in_tif & eav    > 0, (eav    - base_old) / eav,    0)]
vac[, tif_share_cf  := fifelse(in_tif & eav_cf > 0, (eav_cf - base_new) / eav_cf, 0)]

cat("Vacant PINs in a TIF:", sum(vac$in_tif), "\n")
cat("Total gained EAV:", format(sum(vac$eav_cf - vac$eav), big.mark = ","), "\n")
cat("  of which added to agency bases:", format(sum(vac$base_gain), big.mark = ","), "\n")

# Check that my TIF share formula matches PTAXSIM's tif_share for a sample
chk_tif <- lookup_pin_tif(tax_year, head(vac[in_tif == TRUE, pin], 500))
chk_tif <- merge(chk_tif[, .(tif_share = sum(tif_share)), by = pin],
                 vac[, .(pin, tif_share_old)], by = "pin")
cat("Max |tif_share - my formula|:", max(abs(chk_tif$tif_share - chk_tif$tif_share_old)), "\n")

# --- 2. Counterfactual agency data (county-wide) -----------------------------
all_tc <- as.character(DBI::dbGetQuery(
  ptaxsim_db_conn,
  paste0("SELECT DISTINCT tax_code_num FROM pin WHERE year = ", tax_year)
)$tax_code_num)

agency_actual <- lookup_agency(tax_year, all_tc)

gain_tc <- vac[, .(tc_gain = sum(base_gain)), by = tax_code]
agency_cf <- merge(agency_actual, gain_tc, by = "tax_code", all.x = TRUE)
agency_cf[is.na(tc_gain), tc_gain := 0]
agency_cf[, agency_gain := sum(tc_gain), by = agency_num]
agency_cf[, agency_total_eav :=
            bit64::as.integer64(round(as.numeric(agency_total_eav) + agency_gain))]
agency_cf[, c("tc_gain", "agency_gain") := NULL]
setkey(agency_cf, year, tax_code, agency_num)
setcolorder(agency_cf, names(agency_actual))

# Tax code rates: sum of agency levy / agency EAV. Levies stay the same.
rate_fun <- function(a) a[, .(tax_rate = sum(as.numeric(agency_total_ext) /
                                             as.numeric(agency_total_eav))), by = tax_code]
rates <- merge(rate_fun(agency_actual), rate_fun(agency_cf),
               by = "tax_code", suffixes = c("_actual", "_cf"))
rates[, pct_change := 100 * (tax_rate_cf / tax_rate_actual - 1)]
cat("\nTax code rate % change (summary):\n"); print(summary(rates$pct_change))

# --- 3. Bills: actual vs. counterfactual -------------------------------------
pins_to_run <- if (is.null(run_tax_codes)) pins_all$pin else pins_all[tax_code %in% run_tax_codes, pin]
chunks <- split(pins_to_run, ceiling(seq_along(pins_to_run) / chunk_size))
cat("\nCalculating bills for", length(pins_to_run), "PINs in", length(chunks), "chunk(s)\n")

results <- vector("list", length(chunks))
for (i in seq_along(chunks)) {
  p <- chunks[[i]]

  pin_a     <- lookup_pin(tax_year, p)
  pin_tif_a <- lookup_pin_tif(tax_year, p)

  # Counterfactual inputs: only vacant PINs change
  pin_c     <- copy(pin_a)
  pin_c[vac, on = "pin", `:=`(av = i.av_cf, eav = i.eav_cf)]
  pin_tif_c <- copy(pin_tif_a)
  pin_tif_c[vac, on = "pin", tif_share := i.tif_share_cf]

  bill_a <- tax_bill(tax_year, p, agency_dt = agency_actual, pin_dt = pin_a, pin_tif_dt = pin_tif_a)
  bill_c <- tax_bill(tax_year, p, agency_dt = agency_cf,     pin_dt = pin_c, pin_tif_dt = pin_tif_c)

  # Collapse to one row per PIN
  a <- bill_a[, .(bill_actual = sum(final_tax), eav_actual = max(eav)), by = .(pin, class, tax_code)]
  c <- bill_c[, .(bill_cf = sum(final_tax), eav_cf = max(eav)), by = pin]
  results[[i]] <- merge(a, c, by = "pin")
  cat("  chunk", i, "of", length(chunks), "done\n")
}
res <- rbindlist(results)
res[, pct_change := fifelse(bill_actual > 0, 100 * (bill_cf / bill_actual - 1), NA_real_)]
res[, is_vacant := class %in% counterfactual_class_list]

# --- 4. Sanity checks --------------------------------------------------------
# (a) Baseline: actual bills should match the stored totals
chk <- merge(res, pins_all[, .(pin, tax_bill_total)], by = "pin")
cat("\nBaseline check (bill_actual - tax_bill_total):\n"); print(summary(chk$bill_actual - chk$tax_bill_total))

# (b) Vacant PINs should rise; everyone else should fall slightly
cat("\nMedian % change in bill, vacant vs. non-vacant:\n")
print(res[, .(n = .N, median_pct = median(pct_change, na.rm = TRUE)), by = is_vacant])

# --- 5. Save -----------------------------------------------------------------
saveRDS(res,   "counterfactual_pin_results.rds")
saveRDS(rates, "counterfactual_taxcode_rates.rds")
saveRDS(vac,   "counterfactual_vacant_pins.rds")
cat("\nSaved results.\n")