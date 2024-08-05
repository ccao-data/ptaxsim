library(dplyr)
library(stringr)
library(readr)
library(data.table)

# add pins/year/class after updating detail
key_cols <- readr::read_csv(
  "data-raw/sample_tax_bills/sample_tax_bills_detail.csv"
) %>%
  distinct(year, pin, class) %>%
  mutate(class = as.character(class)) %>%
  filter(year == max(year))

# new data
new_data <- tibble()
for (i in seq_len(key_cols)) {
  print(i)
  # open file
  cur_row <- key_cols[i, ]
  cur_file <- str_glue("{cur_row$year}_{cur_row$class}_{cur_row$pin}.pdf")
  system(str_glue(
    "open data-raw/sample_tax_bills/{cur_file}"
  ))
  print(cur_row)
  if (interactive()) {
    g1 <- readline("input tax_code, township_name, in_tif, in_ssa: ")
    g2 <- readline("input exe_homeowner, exe_senior, exe_freeze,
    exe_longtime_homeowner, exe_disabled,
                   exe_vet_returning, exe_vet_disabled: ")
    g3 <- readline("input eav, tax_amt_pre_exe, tax_amt_post_exe: ")
  }
  g1 <- unlist(str_split(g1, ", "))
  g2 <- unlist(str_split(g2, ", "))
  g3 <- unlist(str_split(g3, ", "))
  final_row <- tibble(
    "year" = cur_row$year,
    "pin" = cur_row$pin,
    "tax_code" = g1[1],
    "class" = cur_row$class,
    "township_name" = g1[2],
    "in_tif" = g1[3],
    "in_ssa" = g1[4],
    "exe_homeowner" = g2[1],
    "exe_senior" = g2[2],
    "exe_freeze" = g2[3],
    "exe_longtime_homeowner" = g2[4],
    "exe_disabled" = g2[5],
    "exe_vet_returning" = g2[6],
    "exe_vet_disabled" = g2[7],
    "eav" = g3[1],
    "tax_amt_pre_exe" = g3[2],
    "tax_amt_post_exe" = g3[3]
  )
  new_data <- bind_rows(new_data, final_row)
}

# save updates
new_data <- new_data %>% mutate(across(
  matches("in_|exe_"),
  ~ if_else(.x == "TRUE", TRUE, FALSE)
))
sum_path <- "data-raw/sample_tax_bills/sample_tax_bills_summary.csv"
data <- read_csv(sum_path,
  col_types = cols(
    pin = "c", tax_code = "c", class = "c",
    eav = "c", tax_amt_pre_exe = "c",
    tax_amt_post_exe = "c"
  )
) %>% bind_rows(new_data)

data %>% write_csv(sum_path)

# Load sample tax bills summary data from file
sample_tax_bills_summary <- readr::read_csv(
  sum_path,
  col_types = cols(pin = "c", tax_code = "c", class = "c")
) %>%
  mutate(
    pin = str_pad(pin, 14, "left", "0"),
    year = as.integer(year),
    eav = as.integer(eav),
    township_name = snakecase::to_title_case(township_name)
  )

# Convert the data to a data.table and use setkey to sort for faster joins
setDT(sample_tax_bills_summary)
setkey(sample_tax_bills_summary, year, pin)

# Load summary data into R package
usethis::use_data(sample_tax_bills_summary, overwrite = TRUE)
