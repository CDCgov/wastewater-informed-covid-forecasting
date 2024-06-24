# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # tar_render() calls
library(crew) # To run in parallel
library(lubridate)
library(purrr, quietly = TRUE)


# Run with a crew controller
# See: https://books.ropensci.org/targets/crew.html
# This defaults to 8 cores but you likely want to use
# floor(<your number of cores>/number used for MCMC)
# Currently on model fitting is run in parallel due to an assumption
# that IO costs outweighs any benefit for other tasks.
# This may not be the case. To change allocation alter the deployment
# argument of a target.
controller <- crew_controller_local(
  workers = 8,
  seconds_idle = 600
)

# Set target options:
tar_option_set(
  workspace_on_error = TRUE,
  packages = c("cfaforecastrenewalww", "wweval"),
  # Run with a pre-specified crew controller
  controller = controller,
  # Setup storage on workers vs on the main node.
  # This will only work on workers that have access to the data
  # See https://books.ropensci.org/targets/performance.html#worker-storage
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  format = "rds", # default storage format
  error = "continue" # tells errored targets to return NULL rather than
  # have whole pipeline fail
  # Set other options as needed.
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
      "data.table"
    )
  )
}

setup_interactive_dev_run()

# Set up secrets if planning on using epidatr API or NWSS API,
# otherwise if using local time stamped vintages, the secrets aren't
# necessary
# cfaforecastrenewalww::setup_secrets("secrets.yaml")#nolint

# Need to specify the evaluation variable combinations outside of targets
eval_config <- yaml::read_yaml(file.path(
  "input", "config",
  "eval", "eval_config.yaml"
))
# Get global parameter values
params <- cfaforecastrenewalww::get_params(file.path(
  "input", "params.toml"
))



# Evaluation data--------------------------------------------------------------
upstream_targets <- list(
  tar_target(
    name = eval_hosp_data,
    command = get_input_hosp_data(
      forecast_date = eval_config$eval_date,
      location = unique(eval_config$location_hosp),
      hosp_data_dir = eval_config$hosp_data_dir,
      calibration_time = 365, # Grab sufficient data for eval
      # If don't have a hospital admissions dataset from the `eval_date`,
      # can load using epidatr
      # load_from_epidatr = TRUE, #nolint
      # population_data_path = eval_config$population_data_path #nolint
    )
  ),
  tar_target(
    name = eval_ww_data,
    command = get_input_ww_data(
      forecast_date = eval_config$eval_date,
      location = unique(eval_config$location_ww),
      scenario = "status_quo",
      scenario_dir = eval_config$scenario_dir,
      ww_data_dir = eval_config$ww_data_dir,
      calibration_time = 365, # Grab sufficient data for eval
      last_hosp_data_date = eval_config$eval_date,
      ww_data_mapping = eval_config$ww_data_mapping
    )
  )
)



# Tar_combined replacement----------------------------------------------------
# Each target rowbinds all of the summarized quantiles and scores for each
# location, forecast_date, and scenario (part of each ind dataframe) for both
# wastewater and hospital admissions models
combined_targets <- list(
  ## Scores--------------------------------------------------------------------
  tar_target(
    name = all_ww_scores,
    command = combine_outputs(
      output_type = "scores",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "ww"
    )
  ),
  tar_target(
    name = all_hosp_scores,
    command = combine_outputs(
      output_type = "scores",
      scenarios = "no_wastewater",
      forecast_dates = eval_config$forecast_date_hosp,
      locations = eval_config$location_hosp,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "hosp"
    )
  ),
  ### Scores from quantiles-------------------------------------------------
  tar_target(
    name = all_ww_scores_quantiles,
    command = combine_outputs(
      output_type = "scores_quantiles",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "ww"
    )
  ),
  tar_target(
    name = all_hosp_scores_quantiles,
    command = combine_outputs(
      output_type = "scores_quantiles",
      scenarios = "no_wastewater",
      forecast_dates = eval_config$forecast_date_hosp,
      locations = eval_config$location_hosp,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "hosp"
    )
  ),
  ## Quantiles ----------------------------------------------------------------
  tar_target(
    name = all_ww_hosp_quantiles,
    command = combine_outputs(
      output_type = "hosp_quantiles",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "ww"
    )
  ),
  tar_target(
    name = all_hosp_model_quantiles,
    command = combine_outputs(
      output_type = "quantiles",
      scenarios = "no_wastewater",
      forecast_dates = eval_config$forecast_date_hosp,
      locations = eval_config$location_hosp,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "hosp"
    )
  ),
  tar_target(
    name = all_ww_quantiles,
    command = combine_outputs(
      output_type = "ww_quantiles",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "ww"
    )
  ),
  ## Errors-------------------------------------------------------------------
  tar_target(
    name = all_ww_errors,
    command = combine_outputs(
      output_type = "errors",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "ww"
    )
  ),
  tar_target(
    name = all_hosp_errors,
    command = combine_outputs(
      output_type = "errors",
      scenarios = "no_wastewater",
      forecast_dates = eval_config$forecast_date_hosp,
      locations = eval_config$location_hosp,
      eval_output_subdir = file.path("output", "eval"),
      model_type = "hosp"
    )
  )
)


# Downstream targets-------------------------------------------------------
downstream_targets <- list(
  tar_target(
    name = all_raw_scores,
    command = data.table::as.data.table(
      dplyr::bind_rows(all_hosp_scores, all_ww_scores)
    )
  ),
  tar_target(
    name = all_raw_scores_quantiles,
    command = data.table::as.data.table(
      dplyr::bind_rows(all_hosp_scores_quantiles, all_ww_scores_quantiles)
    )
  ),
  tar_target(
    name = all_errors,
    command = dplyr::bind_rows(all_hosp_errors, all_ww_errors)
  ),

  ## Raw scores-----------------------------------------
  # These are the scores from each scenario and location without buffering
  # by adding what we would have submitted for a submission which would be
  # a mix of model types
  tar_target(
    name = summarized_raw_scores,
    command = scoringutils::summarize_scores(all_raw_scores,
      by = c(
        "scenario",
        "period",
        "forecast_date",
        "location"
      )
    ) |>
      dplyr::group_by(location) |>
      targets::tar_group()
  ),
  tar_target(
    name = grouped_all_raw_scores,
    command = all_raw_scores |>
      dplyr::group_by(location) |>
      targets::tar_group(),
    iteration = "group"
  ),

  ## Submitted scores-----------------------------------------
  tar_target(
    name = mock_submission_scores,
    command = create_mock_submission_scores(all_raw_scores)
  ),
  tar_target(
    name = mock_submission_scores_quantiles,
    command = create_mock_submission_scores(all_raw_scores_quantiles)
  ),
  tar_target(
    name = summarized_scores,
    command = scoringutils::summarize_scores(mock_submission_scores,
      by = c(
        "scenario",
        "period",
        "forecast_date",
        "location"
      )
    ) |>
      dplyr::group_by(location) |>
      targets::tar_group()
  ),
  tar_target(
    name = grouped_submission_scores,
    command = mock_submission_scores |>
      dplyr::group_by(location) |>
      targets::tar_group(),
    iteration = "group"
  ),
  tar_target(
    name = final_summary_scores,
    command = scoringutils::summarize_scores(mock_submission_scores,
      by = c(
        "scenario"
      )
    )
  ),
  # Plots----------------------------------------------------
  tar_target(
    name = plot_raw_scores,
    command = get_plot_raw_scores(all_raw_scores,
      score_metric = "crps"
    ),
    deployment = "main"
  ),
  tar_target(
    name = plot_summarized_raw_scores,
    command = get_plot_summarized_scores(grouped_all_raw_scores,
      score_metric = "crps"
    ),
    pattern = map(grouped_all_raw_scores),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_summarized_scores,
    command = get_plot_summarized_scores(grouped_submission_scores,
      score_metric = "crps"
    ),
    pattern = map(grouped_submission_scores),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_summarized_scores_w_data,
    command = get_plot_scores_w_data(
      grouped_submission_scores,
      eval_hosp_data,
      figure_file_path = eval_config$figure_dir,
      score_metric = "crps"
    ),
    pattern = map(grouped_submission_scores),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = heatmap_scores,
    command = get_heatmap_scores(
      mock_submission_scores_quantiles
    )
  ),
  tar_target(
    name = final_plot,
    command = get_plot_final_scores(final_summary_scores,
      score_metric = "crps"
    ),
    deployment = "main"
  ),
  tar_target(
    name = all_hosp_quantiles,
    command = rbind(
      all_hosp_model_quantiles,
      all_ww_hosp_quantiles
    ) |>
      dplyr::group_by(location) |>
      targets::tar_group(),
    iteration = "group"
  ),
  tar_target(
    name = plot_quantile_comparison,
    command = get_plot_quantile_comparison(
      all_hosp_quantiles,
      eval_hosp_data,
      figure_file_path = eval_config$figure_dir,
      days_to_show_forecast = 7
    ),
    pattern = map(all_hosp_quantiles),
    iteration = "list"
  ),
  tar_target(
    name = box_plot_by_date_and_scenario,
    command = get_box_plot(
      mock_submission_scores,
      figure_file_path = eval_config$figure_dir
    )
  ),
  tar_target(
    name = bar_chart_n_improved,
    command = get_n_states_improved_plot(
      mock_submission_scores,
      figure_file_path = eval_config$figure_dir
    )
  ),
  tar_target(
    name = grouped_ww_quantiles,
    command = all_ww_quantiles |>
      dplyr::group_by(location, scenario) |>
      targets::tar_group(),
    iteration = "group"
  ),
  tar_target(
    name = plot_ww_quantile_comparison,
    command = get_plot_ww_comparison(
      grouped_ww_quantiles
    ),
    pattern = map(grouped_ww_quantiles),
    iteration = "list"
  )
)


# Hub targets-------------------------------------------------------
hub_targets <- list(
  tar_target(
    name = metadata_hub_submissions,
    command = create_hub_submissions(all_ww_hosp_quantiles,
      all_hosp_model_quantiles,
      forecast_dates = seq(
        from = lubridate::ymd(
          min(eval_config$forecast_date_hosp)
        ),
        to = lubridate::ymd("2024-01-29"),
        by = "week"
      ),
      hub_subdir = file.path("output", "eval", "hub"),
      model_name = "cfa-wwrenewal"
    )
  )
)



# Run the targets pipeline
list(
  upstream_targets,
  combined_targets,
  downstream_targets,
  hub_targets
)
