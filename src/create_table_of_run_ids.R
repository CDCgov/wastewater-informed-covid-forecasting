# Create a table of model run ids that correspond to the prod run for each
# real-time forecast date

ids <- c("731d8", "5ebc5", "bb0b4", "a6e67", "f86b2", "8150f")
forecast_date <- seq(
  from = lubridate::ymd("2024-02-05"),
  to = lubridate::ymd("2024-03-11"),
  by = "week"
)
dates_run <- c(
  "2024-02-05", "2024-02-12",
  "2024-02-18", "2024-02-05",
  "2024-03-02", "2024-03-09"
)

table_of_run_ids <- tibble::tibble(ids, forecast_date, dates_run)

saveRDS(table_of_run_ids, "output/real_time_outputs/table_of_run_ids.rds")
