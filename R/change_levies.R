#' Change levy values to test counterfactual scenarios
#'
#' @description The functions in this group alter levy data returned by
#'   \code{\link{lookup_levies}} to simulate counterfactual situations.
#'
#'   For example, \code{change_levy_pct()} can be used to determine how a tax
#'   bill would be affected if all levies for that bill went up by 2%.
#'
#'   **NOTE:** Levy changes are subject to Illinois' Property Tax Extension
#'   Limitation Law (PTELL). By default, these functions will throw a warning
#'   when one or more levy changes might violate PTELL.
#'
#'   Note that is is a naive/simple implementation of PTELL. See the underlying
#'   \code{\link{check_levies_ptell}} function for more details on PTELL and
#'   this specific implementation.
#'
#' @param levies_df A data frame of levy information identical to the format
#'   returned by \code{\link{lookup_levies}}.
#' @param amount A numeric vector of amounts to be added to the levies in
#'   \code{levies_df}. Must be length 1 or the same number of rows as
#'   \code{levies_df}.
#' @param n_years A length 1 numeric vector specifying the number of prior year
#'   percentage changes to average. For example, providing levies from 2019 and
#'   specifying \code{n_years = 5} will get the average levy percent change
#'   between 2014 and 2019, then multiply the 2019 levy by that average.
#' @param percent A numeric vector of percentages by which to change the levies
#'   in \code{levies_df}. Levies are changed via the following formula:
#'   \code{levy + (percent * levy)}. Must be length 1 or the same number of rows
#'   as \code{levies_df}.
#' @param ... Arguments passed directly to the underlying PTELL check function
#'   \code{\link{check_levies_ptell}}.
#'
#' @return A data frame containing altered levy values in the same format
#'   returned by \code{\link{lookup_levies}}.
#'
#' @examples
#' \donttest{
#' library(magrittr)
#' levies <- lookup_levies(2019, "73105")
#'
#' levies %>% change_levies_amt(amount = 10000)
#' levies %>% change_levies_avg(n_years = 5)
#' levies %>% change_levies_cpi()
#' levies %>% change_levies_pct(percent = 0.02)
#' }
#' @md
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @family change
#' @name change_levies
NULL


#' @describeIn change_levies Change levies by a fixed amount. The amount
#'   specified is added to the existing levy.
#'
#' @export
change_levies_amt <- function(levies_df, amount, ...) {
  check_levies_df_str(levies_df)
  stopifnot(is.numeric(amount))

  levies_df_new <- levies_df %>%
    dplyr::mutate(amount = amount) %>%
    dplyr::mutate(total_levy = .data$total_levy + .data$amount) %>%
    dplyr::select(-.data$amount)

  check_levies_ptell(levies_df, levies_df_new, ...)
  return(levies_df_new)
}


#' @describeIn change_levies Change levies by the average change from the
#'   previous N years.
#'
#' @export
change_levies_avg <- function(levies_df, n_years, ...) {
  check_levies_df_str(levies_df)
  stopifnot(
    is.numeric(n_years),
    length(n_years) == 1,
    n_years >= 2
  )

  # For each levy agency in the initial data frame, get the percentage change
  # over the previous N years
  avg_pct_changes <- levies_df %>%
    dplyr::select(target_year = .data$year, .data$agency) %>%
    dplyr::left_join(
      ptaxsim::levy_and_total_eav_by_agency %>%
        dplyr::select(lag_year = .data$year, .data$agency, .data$total_levy),
      by = "agency",
      copy = TRUE
    ) %>%
    dplyr::filter(
      .data$lag_year < .data$target_year,
      .data$lag_year >= .data$target_year - n_years
    ) %>%
    dplyr::group_by(.data$target_year, .data$agency) %>%
    dplyr::mutate(
      pct_change = (.data$total_levy -
        dplyr::lag(.data$total_levy)) / dplyr::lag(.data$total_levy)
    ) %>%
    dplyr::summarize(
      count = dplyr::n(),
      avg_pct_change = mean(.data$pct_change, na.rm = TRUE),
      .groups = "drop"
    )

  # If the number of years available to average is fewer than specified, throw
  # a warning
  if (any(avg_pct_changes$count < n_years + 1)) {
    warning(
      "One or more agencies does not have sufficient historical data to ",
      "calculate the average levy change. See n_years argument for ",
      "more information"
    )
  }

  # Return the original data with changed levies
  levies_df_new <- levies_df %>%
    dplyr::left_join(
      avg_pct_changes,
      by = c("year" = "target_year", "agency")
    ) %>%
    dplyr::mutate(
      total_levy = .data$total_levy +
        (.data$total_levy * .data$avg_pct_change)
    ) %>%
    dplyr::select(-.data$count, -.data$avg_pct_change)

  check_levies_ptell(levies_df, levies_df_new, ...)
  return(levies_df_new)
}


#' @describeIn change_levies Change levies by the prior year's rate of inflation
#'   (CPI-U).
#'
#' @export
change_levies_cpi <- function(levies_df, ...) {
  check_levies_df_str(levies_df)

  levies_df_new <- levies_df %>%
    dplyr::left_join(
      ptaxsim::cpis %>% dplyr::select(.data$levy_year, .data$ptell_cook),
      by = c("year" = "levy_year"),
      copy = TRUE
    ) %>%
    dplyr::mutate(total_levy = .data$total_levy * (1 + .data$ptell_cook)) %>%
    dplyr::select(-.data$ptell_cook)

  check_levies_ptell(levies_df, levies_df_new, ...)
  return(levies_df_new)
}


#' @describeIn change_levies Change levies by a fixed percentage.
#'
#' @export
change_levies_pct <- function(levies_df, percent, ...) {
  check_levies_df_str(levies_df)
  stopifnot(is.numeric(percent))

  levies_df_new <- levies_df %>%
    dplyr::mutate(percent = percent) %>%
    dplyr::mutate(
      total_levy = .data$total_levy +
        (.data$total_levy * .data$percent)
    ) %>%
    dplyr::select(-.data$percent)

  check_levies_ptell(levies_df, levies_df_new, ...)
  return(levies_df_new)
}
