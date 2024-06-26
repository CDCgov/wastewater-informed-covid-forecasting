library(tidyverse)


test <- tibble(
  trend = c(
    rep("increasing", 5), rep("uncertain", 5),
    rep("decreasing", 5), "increasing", "uncertain", "decreasing",
    "decreasing", "uncertain", "uncertain", "increasing", "increasing"
  ),
  date = c(
    seq(
      from = lubridate::ymd("2024-01-01"),
      to = lubridate::ymd("2024-01-15"),
      by = "days"
    ),
    seq(
      from = lubridate::ymd("2024-01-01"),
      to = lubridate::ymd("2024-01-08"),
      by = "days"
    )
  ),
  state_abbr = c(rep("MA", 15), rep("NJ", 8))
) |>
  dplyr::group_by(state_abbr) |>
  dplyr::mutate(
    groups_phase = data.table::rleid(trend)
  ) |>
  dplyr::ungroup()

summarized_by_trend <- test |>
  dplyr::distinct(groups_phase, state_abbr, trend) |>
  dplyr::arrange(state_abbr, groups_phase) |>
  dplyr::group_by(state_abbr) |>
  dplyr::mutate(
    lag_phase = dplyr::lag(trend),
    lead_phase = dplyr::lead(trend),
    phase_reclass = dplyr::case_when(
      trend == "uncertain" & lag_phase == lead_phase ~ lead_phase,
      trend == "uncertain" & lag_phase == "decreasing" & lead_phase == "increasing" ~ "nadir",
      trend == "uncertain" & lag_phase == "increasing" & lead_phase == "decreasing" ~ "peak",
      TRUE ~ trend
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::select(state_abbr, groups_phase, phase_reclass)

rt_cat <- test |> dplyr::left_join(
  summarized_by_trend,
  by = c("state_abbr", "groups_phase")
)
