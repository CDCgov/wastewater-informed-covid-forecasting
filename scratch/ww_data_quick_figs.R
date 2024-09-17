library(ggplot2)

nwss_az <- readr::read_csv(file.path("input", "ww_data", "monday_datasets", "2024-02-05.csv")) |>
  dplyr::filter(wwtp_jurisdiction == "az")
ny_old <- readr::read_csv(file.path("input", "ww_data", "nwss_data", "2024-02-11.csv")) |>
  dplyr::filter(wwtp_jurisdiction == "ny")

ggplot(nwss_ny) +
  geom_line(
    aes(
      x = sample_collect_date, y = pcr_target_avg_conc,
      color = pcr_target_units
    ),
    size = 0.1
  ) +
  facet_wrap(~pcr_target_units) +
  ggtitle("2024-02-13 dataset") +
  scale_y_continuous(trans = "log")

ggplot(ny_old) +
  geom_line(
    aes(
      x = sample_collect_date, y = pcr_target_avg_conc,
      color = pcr_target_units
    ),
    size = 0.1
  ) +
  facet_wrap(~pcr_target_units) +
  ggtitle("2024-02-11 dataset") +
  scale_y_continuous(trans = "log")


# Explore MN data
nwss_ga <- readr::read_csv(file.path("input", "ww_data", "nwss_data", "2024-02-24.csv")) |>
  dplyr::filter(wwtp_jurisdiction == "ga")

ggplot(nwss_ga) +
  geom_line(
    aes(
      x = sample_collect_date, y = pcr_target_avg_conc,
      color = pcr_target_units
    ),
    size = 0.1
  ) +
  facet_wrap(~pcr_target_units) +
  ggtitle("2024-02-21 dataset") +
  scale_y_continuous(trans = "log")

ggplot(ny_old) +
  geom_line(
    aes(
      x = sample_collect_date, y = pcr_target_avg_conc,
      color = pcr_target_units
    ),
    size = 0.1
  ) +
  facet_wrap(~pcr_target_units) +
  ggtitle("2024-02-11 dataset") +
  scale_y_continuous(trans = "log")
