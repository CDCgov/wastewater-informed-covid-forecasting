# Create a table of model run ids that correspond to the prod run for each
# real-time forecast date

ids <- c("731d8", "5ebc5", "bb0b4", "a6e67", "f86b2", "8150f")
dates <- seq(
  from = lubridate::ymd("2024-02-05"),
  to = lubridate::ymd("2024-03-11"),
  by = "week"
)

table_of_run_ids <- tibble::tibble(ids, dates)

saveRDS(table_of_run_ids, "output/real_time_outputs/table_of_run_ids.rds")
