# =============================================================================
# Summary tables: actual vs. counterfactual (vacant land at 25% LOA)
# Needs the .rds files saved by counterfactual_pipeline.R, plus the two agency
# files saved with saveRDS(agency_actual, ...) / saveRDS(agency_cf, ...).
# For county-wide results, run the pipeline with run_tax_codes <- NULL first.
# =============================================================================
library(data.table)
library(data.table)
library(bit64)

res           <- readRDS("counterfactual_pin_results.rds")
rates         <- readRDS("counterfactual_taxcode_rates.rds")
vac           <- readRDS("counterfactual_vacant_pins.rds")
agency_actual <- readRDS("counterfactual_agency_actual.rds")
agency_cf     <- readRDS("counterfactual_agency_cf.rds")

library(bit64)

to_num <- function(v) if (inherits(v, "integer64")) bit64::as.double.integer64(v) else as.numeric(v)

agency_actual[, `:=`(agency_total_eav = to_num(agency_total_eav),
                     agency_total_ext = to_num(agency_total_ext))]
agency_cf[, agency_total_eav := to_num(agency_total_eav)]
res[, `:=`(eav_actual = to_num(eav_actual), eav_cf = to_num(eav_cf))]

print(summary(agency_actual$agency_total_eav))   # expect values in the billions
print(summary(res$eav_actual))                   # expect plain dollar values

dir.create("summary_tables", showWarnings = FALSE)

# --- Helpers: municipality and major class -----------------------------------
# Each tax code's municipality = the agency with minor type "MUNI"
muni <- agency_actual[agency_minor_type == "MUNI",
                      .(muni = agency_name[1]), by = tax_code]

res <- merge(res, muni, by = "tax_code", all.x = TRUE)
res[is.na(muni), muni := "NO MUNICIPALITY (UNINCORPORATED)"]

res[, major_class := fcase(
  substr(class, 1, 1) == "1", "1 Vacant land",
  substr(class, 1, 1) == "2", "2 Residential",
  substr(class, 1, 1) == "3", "3 Apartments",
  substr(class, 1, 1) == "5", "5 Commercial/Industrial",
  default = "Other"
)]
res_major <- res[major_class != "Other"]

summ <- function(dt, by) {
  dt[, .(
    n             = .N,
    median_actual = median(bill_actual),
    median_cf     = median(bill_cf),
    median_change = median(bill_cf - bill_actual),
    median_pct    = median(pct_change, na.rm = TRUE),
    total_actual  = sum(bill_actual),
    total_cf      = sum(bill_cf)
  ), by = by]
}

# --- 1. Vacant land: how much do AVs / EAVs change? --------------------------
vac_summary <- vac[, .(
  n_pins           = .N,
  n_in_tif         = sum(in_tif),
  total_av         = sum(as.numeric(av)),
  total_av_cf      = sum(as.numeric(av_cf)),
  total_eav        = sum(as.numeric(eav)),
  total_eav_cf     = sum(as.numeric(eav_cf)),
  gain_to_base     = sum(base_gain),
  gain_to_tif      = sum(eav_cf - eav) - sum(base_gain)
)]
cat("\n1. Vacant land AV/EAV change\n"); print(vac_summary)
fwrite(vac_summary, "summary_tables/1_vacant_av_change.csv")

# --- 2. Countywide: median bills by major class ------------------------------
county_tbl <- summ(res_major, "major_class")[order(major_class)]
cat("\n2. Countywide median bills by major class\n"); print(county_tbl)
fwrite(county_tbl, "summary_tables/2_county_by_class.csv")

# --- 3. Municipality x major class -------------------------------------------
muni_tbl <- summ(res_major, c("muni", "major_class"))
fwrite(muni_tbl, "summary_tables/3_muni_by_class_long.csv")

# Wide version (drops very small cells so medians are meaningful)
muni_wide <- dcast(
  muni_tbl[n >= 10], muni ~ major_class,
  value.var = c("median_actual", "median_cf", "median_pct")
)
fwrite(muni_wide, "summary_tables/3_muni_by_class_wide.csv")

# Which municipalities see the biggest impact? (vacant land bills, % change)
cat("\n3a. Municipalities: biggest median % increase for vacant land\n")
print(muni_tbl[major_class == "1 Vacant land" & n >= 10][order(-median_pct)][1:10])
cat("\n3b. Municipalities: biggest median % decrease for residential\n")
print(muni_tbl[major_class == "2 Residential" & n >= 10][order(median_pct)][1:10])

# --- 4. Tax code rate changes (proxy for % change in non-vacant bills) -------
tc_eav <- res[, .(eav_total = sum(as.numeric(eav_actual)),
                  eav_vacant = sum(as.numeric(eav_actual[is_vacant]))), by = tax_code]
tc_eav[, vacant_share := eav_vacant / eav_total]

rates2 <- merge(rates, muni, by = "tax_code", all.x = TRUE)
rates2 <- merge(rates2, tc_eav, by = "tax_code", all.x = TRUE)
rates2 <- rates2[order(pct_change)]
cat("\n4. Tax codes with the largest rate decreases\n"); print(rates2[1:10])
fwrite(rates2, "summary_tables/4_taxcode_rate_change.csv")

# --- 5. Top 5 agencies by rate % change --------------------------------------
ag_a <- unique(agency_actual[, .(agency_num, agency_name, agency_minor_type,
                                 eav = as.numeric(agency_total_eav),
                                 ext = as.numeric(agency_total_ext))], by = "agency_num")
ag_c <- unique(agency_cf[, .(agency_num, eav_cf = as.numeric(agency_total_eav))], by = "agency_num")
ag <- merge(ag_a, ag_c, by = "agency_num")
ag[, rate_actual := ext / eav]
ag[, rate_cf     := ext / eav_cf]
ag[, pct_change  := 100 * (rate_cf / rate_actual - 1)]
ag <- ag[order(pct_change)]
cat("\n5. Top 5 agencies by rate % change\n"); print(ag[1:5])
fwrite(ag, "summary_tables/5_agency_rate_change.csv")

cat("\nTables written to ./summary_tables/\n")