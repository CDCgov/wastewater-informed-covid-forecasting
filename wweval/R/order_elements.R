#' Get the order of the categorical horizons for plotting
#'
#' @param df a dataframe containing the column name `horizon` which will
#' contain character strings indicating the horizon (by week, nowcast period,
#' or overall) of the score/quantile/sample.
#'
#' @return a dataframe containing the same columns as `df` with the `horizon`
#' column now an ordered factor with ordered levels as follows:
#' `"overall", "calibration", "nowcast", "1 wk", "2 wks", "3 wks", "4 wks"`
#' @export
order_horizons <- function(df) {
  horizon_order <- c(
    "overall",
    "calibration",
    "nowcast",
    "1 wk",
    "2 wks",
    "3 wks",
    "4 wks"
  )

  if (!horizon %in% colnames(df)) {
    cli::cli_abort(
      message =
        c(
          "Column named `horizon` is missing from the dataframe"
        )
    )
  }

  horizon_names <- df |>
    dplyr::distinct(horizon) |>
    dplyr::pull()

  if (any(!horizon_names %in% horizon_order)) {
    cli::cli_abort(
      message =
        c(
          "Horizon names in dataframe differ from",
          "expected names of: {horizon_order}"
        )
    )
  }


  df_w_order <- df |>
    dplyr::mutate(
      horizon = factor(horizon, ordered = TRUE, levels = horizon_order)
    )



  return(df_w_order)
}
#' Get the order of the time periods for plotting
#'
#' @param df a dataframe containing the column name `period` which will
#' contain character strings indicating what period the scores correspond to
#'
#' @return a dataframe containing the same columns as `df` but with the `period`
#' factor now ordered so that `"Oct 2023-Mar 2024"` comes first and
#' `"Feb 2024-Mar 2024"` comes second.

#' @export
order_periods <- function(df) {
  period_order <- c(
    "Oct 2023-Mar 2024",
    "Feb 2024-Mar 2024"
  )

  if (!period %in% colnames(df)) {
    cli::cli_abort(
      message =
        c(
          "Column named `period` is missing from the dataframe"
        )
    )
  }

  period_names <- df |>
    dplyr::distinct(period) |>
    dplyr::pull()


  if (any(!period_names %in% period_order)) {
    cli::cli_abort(
      message =
        c(
          "Period names in dataframe differ from",
          "expected names of: {period_order}"
        )
    )
  }


  df_w_order <- df |>
    dplyr::mutate(
      period = factor(period, ordered = TRUE, levels = period_order)
    )
  return(df_w_order)
}
