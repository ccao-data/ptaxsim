library(arrow)
library(bit64)
library(dplyr)
library(miniUI)
library(openxlsx)
library(purrr)
library(readxl)
library(snakecase)
library(stringr)
library(tabulizer)
library(tidyr)

calc_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Clean scanned PDF text and collapse into data frame
clean_matrix <- function(mat) {
  nc <- ncol(mat)
  nr <- nrow(mat)
  chr <- stringr::str_replace_all(mat, "[^0-9\\.]", "")
  out <- as.data.frame(matrix(chr, nrow = nr, ncol = nc)) %>%
    mutate(V1 = str_remove_all(V1, "[^0-9]")) %>%
    na_if("") %>%
    # filter(!is.na(V1)) %>%
    # select(where(function(x) all(!is.na(x)))) %>%
    mutate(
      across(everything(), ~ str_replace_all(.x, "^\\.$", "0"))
    )

  if (ncol(out) == 8) out <- out[, -2]
  as.matrix(out)
}


# TIF information is maintained by Cook County Clerk's Office.
# URL here: https://www.cookcountyclerkil.gov/service/tif-reports

# This script processes both TIF distribution reports and summaries into
# separate tables

remote_bucket <- Sys.getenv("S3_REMOTE_BUCKET")
remote_path_summ <- file.path(remote_bucket, "tif_summaries", "part-0.parquet")
remote_path_dist <- file.path(
  remote_bucket, "tif_distributions", "part-0.parquet"
)




# Summaries --------------------------------------------------------------------

## Excel Files -----

# Get summary report spreadsheets from after 2013
summ_file_names_xls <- list.files(
  path = "data-raw/tif/summaries/",
  pattern = "*Cook.*\\.xls*",
  full.names = TRUE
)

# Load each file and cleanup columns, then combine into single df
tif_summaries_xls <- map_dfr(summ_file_names_xls, function(file) {

  # Extract year from file name
  year_ext <- as.integer(str_extract(file, "\\d{4}"))

  # Load file based on extension
  if (tools::file_ext(file) == "xls") {
    df <- readxl::read_xls(file)
  } else if (tools::file_ext(file) == "xlsx") {
    df <- readxl::read_xlsx(file)
  }

  df %>%
    mutate(year = year_ext) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    rename_with(~ str_replace(.x, str_c(year_ext, "_"), "curr_year_")) %>%
    rename_with(~ str_replace(.x, str_c(year_ext - 1, "_"), "prev_year_")) %>%
    mutate(
      tif_cancelled_this_year =
        year == str_extract(new_cancelled, "\\d{4}") &
          str_detect(tolower(new_cancelled), "cancel"),
      across(c(tif_cancelled_this_year), ~ replace_na(.x, FALSE)),
      across(c(curr_year_revenue, prev_year_revenue), ~ replace_na(.x, 0)),
      agency = str_pad(agency, 9, "left", "0")
    ) %>%
    select(
      year,
      agency_num = agency, tif_name, prev_year_revenue,
      curr_year_revenue, first_year, tif_cancelled_this_year,
      any_of("municipality")
    )
})


## PDF Files -----

# Get summary report PDFs
summ_file_names_pdf <- list.files(
  path = "data-raw/tif/summaries/",
  pattern = "*Summary\\.pdf",
  full.names = TRUE
)

tif_summaries_pdf <- map_dfr(summ_file_names_pdf, function(file) {
  message("Reading: ", file)
  year_ext <- as.integer(str_extract(file, "\\d{4}"))

  # Extract tables from PDFs. Some tables get an extra 3rd column which we can
  # drop
  tables <- extract_tables(file) %>%
    map(function(x) if (ncol(x) > 6) x[, c(1:2, 4:7)] else x) %>%
    .[lapply(., nrow) > 1]

  do.call(rbind, tables) %>%
    as_tibble() %>%
    set_names(c(
      "agency_num", "tif_name", "first_year",
      "curr_year_revenue", "prev_year_revenue", "pct_diff"
    )) %>%
    filter(agency_num != "AGENCY") %>%
    na_if("-") %>%
    na_if("") %>%
    mutate(
      year = year_ext,
      agency_num = str_pad(
        str_squish(str_trim(str_remove_all(agency_num, "-"))),
        9,
        "left",
        "0"
      ),
      tif_cancelled_this_year =
        year == str_extract(tif_name, "\\d{4}"),
      tif_name =
        str_trim(str_squish(str_remove(tif_name, "City of|Village of")))
    ) %>%
    filter(
      tif_cancelled_this_year | is.na(tif_cancelled_this_year),
      !is.na(agency_num)
    ) %>%
    mutate(
      tif_name = str_remove_all(tif_name, "\\ *Cancel.*"),
      tif_name = str_remove_all(tif_name, "\\ *CANCEL.*"),
      tif_name = str_remove_all(tif_name, "\\ *New.*"),
      tif_name = str_squish(str_trim(tif_name)),
      tif_cancelled_this_year = replace_na(tif_cancelled_this_year, FALSE),
      across(
        c("first_year", "curr_year_revenue", "prev_year_revenue"),
        ~ replace_na(readr::parse_number(.x), 0)
      )
    ) %>%
    select(
      year, agency_num, tif_name, prev_year_revenue,
      curr_year_revenue, first_year, tif_cancelled_this_year
    )
})

# Combine Excel and PDF outputs into since data frame
tif_summaries <- bind_rows(
  tif_summaries_pdf,
  tif_summaries_xls
) %>%
  filter(!is.na(tif_name)) %>%
  # Manual fixes for misread values
  mutate(
    agency_num = ifelse(
      tif_name == "Melrose Park - Mid Metro Industrial Area",
      "030770500",
      agency_num
    ),
    first_year = ifelse(
      tif_name == "Homewood - East CBD2011", 2011, first_year
    ),
    tif_name = ifelse(
      tif_name == "Homewood - East CBD2011",
      "Homewood - East CBD",
      tif_name
    ),
    tif_cancelled_this_year = ifelse(
      tif_name == "Homewood - East CBD" & year == 2011,
      FALSE,
      tif_cancelled_this_year
    ),
    tif_new_this_year = year == first_year
  ) %>%
  group_by(agency_num) %>%
  mutate(tif_name = calc_mode(tif_name)) %>%
  arrange(year) %>%
  fill(municipality, .direction = "updown") %>%
  ungroup() %>%
  mutate(
    municipality = ifelse(
      is.na(municipality),
      str_extract(tif_name, ".*(?= - )"),
      municipality
    ),
    municipality = ifelse(
      is.na(municipality),
      tif_name,
      municipality
    ),
    tif_name = str_remove_all(tif_name, "[\\^]"),
    # More manual fixes
    agency_num = ifelse(
      tif_name == "Melrose Park - North Avenue / 25th Avenue",
      "030770502",
      agency_num
    ),
    curr_year_revenue = ifelse(
      agency_num == "030130504" & year == 2011,
      0, curr_year_revenue
    ),
    prev_year_revenue = ifelse(
      agency_num == "030130504" & year == 2011,
      25155.10, prev_year_revenue
    )
  ) %>%
  filter(!(agency_num == "030330500" & first_year == 2012)) %>%
  mutate(across(c(year, first_year), as.character)) %>%
  relocate(municipality, .after = "tif_name") %>%
  arrange(year, agency_num)

# Write to S3
arrow::write_parquet(
  x = tif_summaries,
  sink = remote_path_summ,
  compression = "zstd"
)




# Distributions ----------------------------------------------------------------

## Excel Files -----

# Get a list of all TIF distribution report spreadsheets
dist_file_names_xls <- list.files(
  path = "data-raw/tif/distributions/",
  pattern = "*Distribution.*\\.xls*",
  full.names = TRUE
)

# Load each Excel file and cleanup columns, then combine into single df
tif_distributions_xls <- map_dfr(dist_file_names_xls, function(file) {

  # Extract year from file name
  year_ext <- str_extract(file, "\\d{4}")

  # Load file based on extension
  if (tools::file_ext(file) == "xls") {
    df <- readxl::read_xls(file)
  } else if (tools::file_ext(file) == "xlsx") {
    df <- readxl::read_xlsx(file)
  }

  df %>%
    mutate(year = year_ext) %>%
    set_names(snakecase::to_snake_case(names(.))) %>%
    rename_with(~ str_remove(.x, "_\\d{4}"), contains(year_ext)) %>%
    mutate(
      across(
        c(starts_with("tax_code_"), starts_with("tif_total_")),
        as.numeric
      )
    ) %>%
    rename_with(~ str_remove(.x, "_tif"), starts_with("tax_code_tif_")) %>%
    select(
      year,
      agency_num = tif_agency, tax_code_num = tif_tax_code,
      tax_code_rate, tax_code_eav, tax_code_frozen_eav, tax_code_revenue,
      tax_code_distribution_percent
    )
})


## PDF Files -----

# Get a list of all TIF distribution report spreadsheets
dist_file_names_pdf_list <- sort(list.files(
  path = "data-raw/tif/distributions/",
  pattern = "*Distribution.*\\.pdf",
  full.names = TRUE
))

# Combine into data frame with page dimensions and column locations for
# each extract
dist_file_names_pdf_df <- tibble(
  file = dist_file_names_pdf_list,
  area = list(
    `2006` = c(78, 43, 612, 752),
    `2007` = c(78, 43, 612, 752),
    `2008` = c(78, 43, 612, 752),
    `2009` = c(78, 43, 612, 752),
    `2010` = c(78, 43, 612, 752),
    `2011` = c(78, 43, 612, 752),
    `2012` = c(78, 43, 612, 752),
    `2013` = c(78, 43, 612, 752)
  ),
  cols = list(
    `2006` = c(110, 326, 370, 404, 500, 575, 660),
    `2007` = c(110, 326, 370, 404, 500, 575, 660),
    `2008` = c(115, 322, 364, 404, 500, 575, 660),
    `2009` = c(115, 322, 370, 414, 494, 580, 660),
    `2010` = c(115, 322, 364, 404, 500, 575, 660),
    `2011` = c(115, 322, 365, 414, 500, 575, 660),
    `2012` = c(115, 322, 364, 404, 500, 575, 660),
    `2013` = c(115, 322, 370, 414, 500, 575, 660)
  )
)

# For each PDF, extract the already OCR'd text and convert it to a data frame
# Save the output for manual correction
pwalk(dist_file_names_pdf_df, function(file, area, cols) {
  message("Reading: ", file)
  year_ext <- str_extract(file, "\\d{4}")

  # Extract tables from PDFs, keeping only data frame outputs with 7 columns
  tables <- extract_tables(
    file = file,
    area = list(area),
    columns = list(cols),
    guess = FALSE
  ) %>%
    lapply(., clean_matrix) %>%
    .[as.logical(lapply(., function(x) ncol(x) == 7 & nrow(x) > 0))]

  do.call(rbind, tables) %>%
    as_tibble() %>%
    set_names(c(
      "agency_num", "tax_code_num", "tax_code_rate",
      "tax_code_eav", "tax_code_frozen_eav",
      "tax_code_revenue", "tax_code_distribution_percent"
    )) %>%
    mutate(
      year = year_ext,
      across(c(tax_code_eav, tax_code_frozen_eav), ~ str_remove_all(.x, "\\.")),
      across(
        c(tax_code_rate:tax_code_distribution_percent),
        readr::parse_number
      )
    ) %>%
    relocate(year) %>%
    openxlsx::write.xlsx(
      file.path(
        "data-raw/tif/distributions/ocr_corrected",
        paste("temp.xlsx")
      ),
      overwrite = FALSE
    )
})

# Load the manually corrected files into a single data frame
dist_file_names_pdf_corrected <- list.files(
  path = "data-raw/tif/distributions/ocr_corrected/",
  pattern = "*.xlsx",
  full.names = TRUE
)

tif_distributions_pdf <- map_dfr(dist_file_names_pdf_corrected, read_xlsx)

# Checks for manually corrected TIF data
lapply(tif_distributions_pdf, function(x) table(str_detect(x, "\\.")))
lapply(tif_distributions_pdf, function(x) table(x < 100 & x != 0))
lapply(tif_distributions_pdf, function(x) table(nchar(x)))
skimr::skim(tif_distributions)

tif_distributions <- bind_rows(
  tif_distributions_xls,
  tif_distributions_pdf
) %>%
  mutate(
    across(c(tax_code_eav, tax_code_frozen_eav, tax_code_revenue), as.integer64)
  )

# Write to S3
arrow::write_parquet(
  x = tif_distributions,
  sink = remote_path_dist,
  compression = "zstd"
)
