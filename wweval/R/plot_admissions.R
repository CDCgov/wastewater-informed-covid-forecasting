#' Make a figure of overall admissions (summed across locations)
#' for context
#'
#' @param eval_hosp_data Hospital admissions data for evaluating against
#' for all locations
#' @param first_forecast_date The first forecast date we are evaluating
#' @param last_forecast_date The last forecast date we are evaluating
#'
#' @return ggplot object displaying a timeseries of total
#' hospital admissions.
#' @export
plot_total_admissions <- function(
  eval_hosp_data,
  first_forecast_date,
  last_forecast_date
) {
  hosp_data <- eval_hosp_data |>
    dplyr::distinct(
      .data$location,
      .data$daily_hosp_admits,
      .data$date
    ) |>
    dplyr::group_by(.data$date) |>
    dplyr::summarise(total_hosp = sum(daily_hosp_admits))

  max_total_hosp <- max(hosp_data$total_hosp)

  date_lims <- c(
    as.Date(first_forecast_date),
    as.Date(last_forecast_date)
  )

  p <- ggplot(
    data = hosp_data,
    aes(
      x = .data$date,
      y = .data$total_hosp
    )
  ) +
    geom_point() +
    get_plot_theme(x_axis_dates = TRUE) +
    xlab("") +
    ylab("National admissions") +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_dates = TRUE
    ) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d",
      limits = date_lims
    )
  return(p)
}
