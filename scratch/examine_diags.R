devtools::load_all("wweval")

targets::tar_load("chain_run_time")
targets::tar_load("slowest_chain_run_time")
targets::tar_load("diagnostic_extrema_preds_scored")
targets::tar_load("granular_ww_metadata_used")
targets::tar_load("date_locs_to_compare_retro")

.model_type_density_plot <- function(df, y) {
  df |>
    ggplot(aes(
      x = .data$model_type,
      y = .data[[y]],
      fill = .data$model_type
    )) +
    ggdist::stat_halfeye(
      shape = 21,
      point_size = 10,
      interval_size_range = c(2, 5)
    ) +
    get_plot_theme()
}

visualize_rhat <- function(diagnostic_extrema) {
  diagnostic_extrema |>
    .model_type_density_plot("max_rhat") +
    scale_y_continuous(transform = "log10")
}

visualize_clock_time <- function(chain_run_time) {
  chain_run_time |>
    dplyr::mutate(slowest_chain_time_m = .data$slowest_total_s / 60) |>
    .model_type_density_plot("slowest_chain_time_m") +
    scale_y_continuous(transform = "log10") +
    get_plot_theme() +
    scale_fill_model() +
    ylab("Slowest chain run time (m)")
}

visualize_clock_time(slowest_chain_run_time)
visualize_clock_time(
  slowest_chain_run_time |>
    dplyr::inner_join(
      date_locs_to_compare_retro,
      by = c("location", "forecast_date")
    )
)

clock_time_versus_sites <- function(clock_time, metadata) {
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

  data |>
    ggplot(aes(
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
    geom_errorbar(orientation = "horizontal") +
    geom_errorbar(orientation = "vertical") +
    geom_label() +
    facet_wrap(~ .data$model_type) +
    scale_y_continuous(transform = "log10") +
    get_plot_theme()
}

clock_time_versus_sites(slowest_chain_run_time, granular_ww_metadata_used) +
  scale_x_continuous(transform = "log10")

clock_time_versus_sites(
  slowest_chain_run_time |>
    dplyr::inner_join(
      date_locs_to_compare_retro,
      by = c("forecast_date", "location")
    ),
  granular_ww_metadata_used
) +
  scale_x_continuous(transform = "log10")
