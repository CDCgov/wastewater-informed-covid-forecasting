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
ma_scores <- scores_quantiles |>
  dplyr::filter(
    location == "MA",
    forecast_date == "2024-01-15"
  ) |>
  data.table::as.data.table() |>
  scoringutils::summarise_scores(by = c("model", "quantile"))

ma_scores_raw <- scores_quantiles |>
  dplyr::filter(
    location == "MA",
    forecast_date == "2024-01-15"
  )


ma_quantiles <- hosp_quantiles |>
  dplyr::filter(
    location == "MA",
    forecast_date == "2024-01-15"
  )
# Calculate the percent within each interval by hand from the scores using range
# and quantile coverage
get_interval_coverage <- function(quantile_coverage) {
  # if observed data is below both quantiles, then the data is outside the
  # interval
  if (all(quantile_coverage)) {
    ic <- FALSE
  }
  # if observed data is above both quantiles, then the data is outside the
  # interval
  if (all(!quantile_coverage)) {
    ic <- FALSE
  }
  # if observed data is above the lower quantile but below the upper quartile,
  # then the data is within the interval
  if (sum(quantile_coverage) == 1) {
    ic <- TRUE
  }
  if (length(quantile_coverage) == 1) {
    ic <- FALSE
  }
  return(ic)
}

ma_scores_raw |> dplyr::filter(
  forecast_date == "2024-01-15", location == "MA",
  range == 50, date == "2024-01-06", model == "ww"
)

ma_scores_w_ic <- MA_scores_raw |>
  dplyr::group_by(date, forecast_date, location, range, model) |>
  dplyr::mutate(interval_coverage = get_interval_coverage(quantile_coverage)) |>
  dplyr::group_by(range, model) |>
  dplyr::summarise(pct_in_interval = 100 * sum(interval_coverage) / dplyr::n())

ggplot(ma_scores_w_ic) +
  geom_line(aes(x = range, y = pct_in_interval, color = model)) +
  geom_line(aes(x = range, y = range), linetype = "dashed") +
  ylab("Percent of data within interval range") +
  xlab("Interval")

ggplot(ma_scores) +
  geom_line(aes(x = quantile, y = quantile_coverage, color = model)) +
  geom_line(aes(x = quantile, y = quantile), linetype = "dashed") +
  xlab("Quantile") +
  ylab("Percent of data below the quantile")

ggplot(ma_quantiles) +
  geom_line(aes(x = date, y = value, group = quantile)) +
  geom_point(aes(x = date, y = eval_data)) +
  facet_wrap(~model_type)

# Computer yourself the quantile coverage for a specific range in MA
