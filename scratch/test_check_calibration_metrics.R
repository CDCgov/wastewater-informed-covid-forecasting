tar_load(scores_quantiles_filtered)
tar_load(hosp_quantiles_filtered)

scores_quantiles <- scores_quantiles_filtered
hosp_quantiles <- hosp_quantiles_filtered

test <- scores_quantiles |>
  data.table::as.data.table() |>
  scoringutils::summarise_scores(by = c("model", "quantile"))

ggplot(test) +
  geom_line(aes(x = quantile, y = quantile_coverage, color = model)) +
  geom_line(aes(x = quantile, y = quantile), linetype = "dashed")

# Look at an individual state and forecast date

test2 <- scores_quantiles |>
  dplyr::filter(
    location == "MA",
    forecast_date == "2024-01-15"
  ) |>
  data.table::as.data.table() |>
  scoringutils::summarise_scores(by = c("model", "quantile"))

test2_qs <- hosp_quantiles |>
  dplyr::filter(
    location == "MA",
    forecast_date == "2024-01-15"
  )

ggplot(test2) +
  geom_line(aes(x = quantile, y = quantile_coverage, color = model)) +
  geom_line(aes(x = quantile, y = quantile), linetype = "dashed")

ggplot(test2_qs) +
  geom_line(aes(x = date, y = value, group = quantile)) +
  geom_point(aes(x = date, y = eval_data)) +
  facet_wrap(~model_type)
