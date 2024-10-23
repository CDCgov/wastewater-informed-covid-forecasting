# Load packages required to define the pipeline:
library(targets)
library(lubridate)
library(purrr, quietly = TRUE)

# Set target options:
tar_option_set(
  workspace_on_error = TRUE,
  packages = c("wweval"),
  memory = "transient",
  garbage_collection = TRUE,
  format = "rds", # default storage format
  error = "continue" # tells errored targets to return NULL rather than
  # have whole pipeline fail
)

setup_interactive_dev_run <- function() {
  list.files(file.path("wweval", "R"), full.names = TRUE) |>
    purrr::walk(source)
  tar_option_set(
    packages = c(
      "cmdstanr",
      "rlang",
      "tibble",
      "ggplot2",
      "dplyr",
      "lubridate",
      "cmdstanr",
      "tidybayes",
      "cfaforecastrenewalww",
      "data.table",
      "ggridges",
      "ggdist",
      "patchwork",
      "RColorBrewer",
      "cowplot",
      "zoltr"
    )
  )
}

setup_interactive_dev_run()

# Set up secrets if planning on using epidatr API or NWSS API,
# otherwise if using local time stamped vintages, the secrets aren't
# necessary
# wweval::setup_secrets("secrets.yaml")#nolint

# Need to specify the evaluation variable combinations outside of targets
benchmark_config <- yaml::read_yaml(file.path(
  "input", "config",
  "eval", "benchmark_config.yaml"
))

combined_targets <- list(
  ## Scores--------------------------------------------------------------------
  tar_target(
    name = ww_scores,
    command = combine_outputs(
      output_type = "scores",
      scenarios = benchmark_config$scenario,
      forecast_dates = benchmark_config$forecast_date_ww,
      locations = benchmark_config$location_ww,
      eval_output_subdir = benchmark_config$output_dir,
      model_type = "ww"
    )
  ),
  tar_target(
    name = hosp_scores,
    command = combine_outputs(
      output_type = "scores",
      scenarios = "no_wastewater",
      forecast_dates = benchmark_config$forecast_date_hosp,
      locations = benchmark_config$location_hosp,
      eval_output_subdir = benchmark_config$output_dir,
      model_type = "hosp"
    )
  )
)



# Benchmarking----------------------------------------------------------
benchmarks <- list(
  tar_target(
    name = write_benchmark_table_subset_run,
    command = benchmark_performance(
      ww_scores = ww_scores,
      hosp_scores = hosp_scores,
      benchmark_dir = benchmark_config$benchmark_dir,
      benchmark_scope = "subset_forecasts",
      wwinference_version = benchmark_config$wwinference_version,
      overwrite_benchmark = benchmark_config$overwrite_benchmark
    )
  )
)

# Run the targets pipeline----------------------------------------------------
list(
  combined_targets,
  benchmarks
)
