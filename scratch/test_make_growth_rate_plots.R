# Load in DE's 2023-010-16 input data
library(ggplot2)
library(zoo)
input_hosp_data <- readr::read_tsv("output/input_hosp_data.tsv")
input_ww_data <- readr::read_tsv("output/input_ww_data.tsv")

ggplot(input_hosp_data) +
  geom_line(aes(x = date, y = daily_hosp_admits))

ggplot(input_ww_data) +
  geom_line(aes(
    x = date, y = ww,
    color = as.factor(lab_site_index)
  )) +
  facet_wrap(~lab_site_index)



ggplot(input_ww_data) +
  geom_line(aes(
    x = date, y = log(ww),
    color = as.factor(lab_site_index)
  )) +
  facet_wrap(~lab_site_index)

# Smooth hospital admissions data

hosp_data <- input_hosp_data |> dplyr::mutate(
  admits_7d_rolling = zoo::rollmean(daily_hosp_admits,
    k = 7, na.pad = TRUE,
    align = "center"
  )
)
ggplot(hosp_data) +
  geom_point(aes(x = date, y = daily_hosp_admits)) +
  geom_line(aes(x = date, y = admits_7d_rolling))

# Try linear interpolation of ww data
ww_data <- input_ww_data |>
  dplyr::group_by(lab, site, lab_site_index) |>
  tidyr::complete(date = seq.Date(min(hosp_data$date),
    max(hosp_data$date),
    by = "day"
  )) |>
  dplyr::mutate(
    ww_interpolated = na.approx(ww, na.rm = FALSE)
  ) |>
  dplyr::ungroup()

ggplot(ww_data) +
  geom_point(aes(
    x = date, y = log(ww),
    color = as.factor(lab_site_index)
  )) +
  geom_point(
    aes(
      x = date, y = log(ww_interpolated),
      color = as.factor(lab_site_index)
    ),
    size = 0.1
  ) +
  facet_wrap(~lab_site_index) +
  ylab("Log genome copies per mL") +
  theme(legend.position = "bottom")

# Get 7 day rolling average of interpolated data
ww_data <- ww_data |>
  dplyr::mutate(
    ww_7d_rolling = zoo::rollmean(ww_interpolated,
      k = 7, na.pad = TRUE,
      align = "center"
    )
  )

ggplot(ww_data) +
  geom_point(aes(
    x = date, y = log(ww),
    color = as.factor(lab_site_index)
  )) +
  geom_point(
    aes(
      x = date, y = log(ww_7d_rolling),
      color = as.factor(lab_site_index)
    ),
    size = 0.1
  ) +
  facet_wrap(~lab_site_index) +
  theme(legend.position = "bottom")

# Put the two datasets together:
comb_data <- ww_data |>
  dplyr::left_join(
    hosp_data,
    by = "date"
  ) |>
  dplyr::mutate(
    log_ww = log(ww_7d_rolling),
    log_hosp = log(admits_7d_rolling),
    prev_log_ww = dplyr::lag(log_ww, 1),
    prev_week_log_ww = dplyr::lag(log_ww, 7),
    prev_log_hosp = dplyr::lag(log_hosp, 1),
    prev_week_log_hosp = dplyr::lag(log_hosp, 7),
    daily_r_ww = log_ww - prev_log_ww,
    daily_r_hosp = log_hosp - prev_log_hosp,
    weekly_r_ww = (log_ww - prev_week_log_ww) / 7,
    weekly_r_hosp = (log_hosp - prev_week_log_hosp) / 7
  ) |>
  dplyr::mutate(
    lab_site_name = glue::glue("Site: {site}, Lab: {lab}")
  )

ggplot(comb_data) +
  geom_line(aes(x = date, y = daily_r_ww, color = lab_site_name)) +
  geom_line(aes(x = date, y = daily_r_hosp)) +
  facet_wrap(~lab_site_name) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray")
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  ylab("Daily growth rate") +
  ggtitle("DE 2023-10-16 growth rate comparison")

ggplot(comb_data) +
  geom_line(aes(x = date, y = weekly_r_ww, color = lab_site_name)) +
  geom_line(aes(x = date, y = weekly_r_hosp)) +
  facet_wrap(~lab_site_name) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray")
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  ylab("Weekly growth rate") +
  ggtitle("DE 2023-10-16 growth rate comparison")

ggplot(comb_data) +
  geom_line(aes(x = date, y = weekly_r_ww, color = lab_site_name)) +
  geom_line(aes(x = date, y = weekly_r_hosp)) +
  facet_wrap(~lab_site_name) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray")
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  ylab("Weekly growth rate") +
  ggtitle("DE 2023-10-16 growth rate comparison")
