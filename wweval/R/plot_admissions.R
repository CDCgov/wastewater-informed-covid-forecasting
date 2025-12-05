#' Make a figure of overall hospital admissions eval data
#'
#' @param eval_hosp_data Location-level admissions data to summarize
#' @param first_date First date to plot
#' @param last_date Last date to plot
#'
#' @return ggplot object displaying a timeseries of total
#' hospital admissions.
#' @export
plot_total_admissions <- function(
  eval_hosp_data,
  first_date,
  last_date
) {
  hosp_data <- eval_hosp_data |>
    dplyr::distinct(
      .data$location,
      .data$daily_hosp_admits,
      .data$date
    ) |>
    dplyr::summarise(total_hosp = sum(.data$daily_hosp_admits), .by = "date") |>
    dplyr::filter(.data$date >= !!first_date, .data$date <= !!last_date)

  p <- ggplot(
    data = hosp_data,
    aes(
      x = .data$date,
      y = .data$total_hosp
    )
  ) +
    forecasttools::geom_line_point() +
    get_plot_theme(x_axis_dates = TRUE) +
    xlab("") +
    ylab("Incident hospital admissions") +
    get_plot_theme(
      y_axis_title_size = 8,
      x_axis_dates = TRUE
    ) +
    scale_x_date(
      date_breaks = "1 week",
      date_labels = "%Y-%m-%d",
      expand = 0
    )
  return(p)
}
