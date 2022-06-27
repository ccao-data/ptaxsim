.onAttach <- function(...) {
  packageStartupMessage(
    "PTAXSIM requires a downloaded database file to work correctly.\n",
    paste(
      "Please download the DB and assign a SQLite DB connection to",
      "ptaxsim_db_conn before using this package"
    )
  )
}
