.onAttach <- function(...) {
  ptaxsim_db_files <- list.files(pattern = "^ptaxsim.*\\.db$")

  if (!length(ptaxsim_db_files) > 0) {
    packageStartupMessage(
      "PTAXSIM requires a downloaded database file to work correctly.\n",
      paste(
        "Please download the PTAXSIM DB and assign a SQLite DB connection to",
        "ptaxsim_db_conn before using this package"
      )
    )
  }
}
