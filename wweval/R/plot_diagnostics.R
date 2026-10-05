#' Plot a distribution of maximum R-hat values.
#'
#' @param diagnostic_extrema data frame of diagnostic extrema,
#' as the output of [extract_diagnostic_extrema()], collated via
#' [combine_outputs()].
#' @return The plot.
#' @export
plot_max_rhat_distribution <- function(diagnostic_extrema) {
  p <- model_type_eyeplot(diagnostic_extrema, "max_rhat") +
    scale_y_continuous(transform = "log10") +
    labs(y = "Maximum R-hat value")

  return(p)
}

#' Plot a distribution of minimum ESS values.
#'
#' @param diagnostic_extrema data frame of diagnostic extrema,
#' as the output of [extract_diagnostic_extrema()], collated via
#' [combine_outputs()].
#' @param which Plot bulk ESS or tail ESS? Must be one of `"bulk"`
#' or `"tail"`.
#' @return The plot.
#' @export
plot_min_ess_distribution <- function(diagnostic_extrema, which) {
  checkmate::assert_choice(which, c("bulk", "tail"))

  p <- model_type_eyeplot(diagnostic_extrema, glue::glue("min_ess_{which}")) +
    labs(y = glue::glue("Minimum {which} ESS value"))

  return(p)
}


#' Plot model fitting clock time.
#'
#' @param chain_run_time Data frame of chain run times summarized to
#' show only the slowest values per fit.
#' @return The plot
#' @export
plot_fitting_clock_time <- function(chain_run_time) {
  p <- chain_run_time |>
    dplyr::mutate(slowest_chain_time_m = .data$slowest_total_s / 60) |>
    model_type_eyeplot("slowest_chain_time_m") +
    scale_y_continuous(transform = "log10") +
    ylab("Slowest chain run time (m)")

  return(p)
}

#' Plot model fitting clock time as a function of number
#' of wastewater sampling sites.
#'
#' @param clock_time Data frame of chain run times, summarized to
#' show only the slowest values per fit.
#' @param metadata Data frame of wastewater metadata that gives the
#' number of sampling sites (`n_sites`) by `location` and `forecast_date`.
#'
#' @return The plot
#' @export
plot_fitting_clock_time_versus_sites <- function(clock_time, metadata) {
  checkmate::assert_integer(
    metadata$n_sites,
    lower = 0,
    any.missing = FALSE,
    all.missing = FALSE
  )
  # all n_sites values must be non-negative integers, with no missing
  data <- metadata |>
    dplyr::select(
      "location",
      "forecast_date",
      "n_sites"
    ) |>
    dplyr::inner_join(clock_time, by = c("location", "forecast_date")) |>
    dplyr::mutate(time_m = .data$slowest_total_s / 60) |>
    dplyr::summarize(
      xmin = quantile(.data$n_sites, 0.025),
      x = median(.data$n_sites),
      xmax = quantile(.data$n_sites, 0.975),
      ymin = quantile(.data$time_m, 0.025),
      y = median(.data$time_m),
      ymax = quantile(.data$time_m, 0.975),
      .by = c("location", "model_type")
    )

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xmin = .data$xmin,
      xmax = .data$xmax,
      ymin = .data$ymin,
      ymax = .data$ymax,
      label = .data$location,
      fill = .data$model_type,
      group = .data$location
    )) +
    ggplot2::geom_errorbar(orientation = "horizontal") +
    ggplot2::geom_errorbar(orientation = "vertical") +
    ggplot2::geom_label(alpha = 0.65, size = 3) +
    ggplot2::facet_wrap(~ .data$model_type) +
    ggplot2::scale_y_continuous(transform = "log10") +
    scale_fill_model() +
    get_plot_theme() +
    ggplot2::labs(
      x = "Number of wastewater sampling sites",
      y = "Slowest chain run time (m)"
    )

  return(p)
}
