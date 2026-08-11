devtools::load_all("wweval")

targets::tar_load("chain_run_time")
targets::tar_load("slowest_chain_run_time")

visualize_clock_time <- function(chain_run_time) {
  chain_run_time |>
    ggplot(aes(
      x = .data$model_type,
      y = .data$slowest_total_s / 60,
      fill = .data$model_type
    )) +
    ggdist::stat_halfeye(
      shape = 21,
      point_size = 10,
      interval_size_range = c(2, 5)
    ) +
    scale_y_continuous(transform = "log10") +
    get_plot_theme() +
    scale_fill_model() +
    ylab("Slowest chain run time (m)")
}

visualize_clock_time(slowest_chain_run_time)
