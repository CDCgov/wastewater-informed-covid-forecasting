data <- readr::read_csv("input/ww_data/monday_datasets/2024-02-12.csv")

clean_data <- data |> clean_and_filter_nwss_data()

comparison_data <- clean_data |>
  dplyr::mutate(
    below_lod = .data$pcr_target_avg_conc < .data$lod_sewage,
    site_lab_display_name = glue::glue(
      "Site: {.data$wwtp_name}, Lab: {.data$lab_id}"
    )
  ) |>
  dplyr::select(
    "wwtp_jurisdiction",
    "sample_collect_date",
    "wwtp_name",
    "lab_id",
    "site_lab_display_name",
    "pcr_target_avg_conc",
    "pcr_target_flowpop_lin",
    "below_lod"
  ) |>
  tidyr::pivot_longer(tidyselect::starts_with("pcr"), names_to = "metric") |>
  dplyr::mutate(
    metric = dplyr::case_match(
      .data$metric,
      "pcr_target_avg_conc" ~ "Raw\n(genome copies per mL)",
      "pcr_target_flowpop_lin" ~
        "Flow-population normalized\n(genome copies per person)"
    )
  )


fig_normed_comp <- function(data) {
  p <- ggplot(
    data = data,
    mapping = aes(
      x = .data$sample_collect_date,
      y = .data$value,
      color = .data$metric,
      alpha = .data$below_lod,
      group = .data$metric,
      shape = .data$metric,
      fill = .data$metric
    )
  ) +
    forecasttools::geom_line_point(size = 4) +
    scale_y_continuous(transform = "log10") +
    facet_wrap(~ .data$site_lab_display_name, nrow = 3) +
    scale_x_weekly_iso_date() +
    scale_alpha_ordinal(range = c(1, 0.5)) +
    get_plot_theme(rotate_x_ticks = TRUE) +
    labs(x = "Date", y = "Genome concentration")

  return(p)
}

fig_oh <- comparison_data |>
  dplyr::filter(
    .data$wwtp_jurisdiction == "oh",
    .data$wwtp_name %in%
      c(127, 187, 221, 228, 255, 273, 276, 277, 36, 374, 398, 442),
    .data$lab_id == 18,
    .data$sample_collect_date >=
      lubridate::ymd("2024-02-12") - lubridate::days(90)
  ) |>
  fig_normed_comp()

fig_il <- comparison_data |>
  dplyr::filter(
    .data$wwtp_jurisdiction == "il",
    .data$wwtp_name %in%
      c(462, 632, 635, 636, 638, 639, 641, 642, 644, 645, 647, 982),
    .data$lab_id == 38,
    .data$sample_collect_date >=
      lubridate::ymd("2024-02-12") - lubridate::days(90)
  ) |>
  fig_normed_comp()


cowplot::save_plot("fig_oh_normed.png", fig_oh, base_width = 10)
cowplot::save_plot("fig_il_normed.png", fig_il, base_width = 10)
