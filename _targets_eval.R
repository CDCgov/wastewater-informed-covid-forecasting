# This defines the evaluation pipeline

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
      "cfaforecastrenewalww"
    )
  )
}

setup_interactive_dev_run()

# Set up secrets if planning on using epidatr API or NWSS API,
# otherwise if using local time stamped vintages, the secrets aren't
# necessary
# wweval::setup_secrets("secrets.yaml")#nolint

# Need to specify the evaluation variable combinations outside of targets
eval_config <- yaml::read_yaml(file.path(
  "input", "config",
  "eval", "eval_config.yaml"
))
# Get global parameter values
params <- cfaforecastrenewalww::get_params(file.path(
  "input", "params.toml"
))



# Set up some global targets

# Get the evaluation data from the specified evaluation date ----------------
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
  ),
  tar_target(
    name = table_of_exclusions,
    command = tibble::as_tibble(eval_config$table_of_exclusions)
  )
)

# Iterate over forecast dates, locations, and scenarios separately for the two models
# For each iteration (forecast date, location):
# - load in and clean the ww and hosp dataset for each forecast date and location
# - based on the models data requirements, preprocess data and generate parameters
# - generate stan data
# - fit the model
# - extraction posterior samples for generated quantities and parameters of interest
# - join posterior draws with input data
# - summarize the generated quantities to only those we're scoring
# - score the generated quantities against evaluation data (most recent data)
# - save posterior draws and diagnostics

# Wastewater model fitting loop-----------------------------------------------
mapped_ww <- tar_map(
  values = list(
    location = eval_config$location_ww,
    forecast_date = eval_config$forecast_date_ww,
    scenario = eval_config$scenario
  ),
  tar_target(
    name = stan_model_path_target,
    command = get_model_path(
      model_type = "ww",
      stan_models_dir = eval_config$stan_models_dir
    ),
    format = "file",
    priority = 1
  ),
  tar_target(
    name = raw_input_hosp_data,
    command = get_input_hosp_data(forecast_date, location,
      hosp_data_dir = eval_config$hosp_data_dir,
      calibration_time = eval_config$calibration_time
    ),
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = input_hosp_data,
    command = exclude_hosp_outliers(
      raw_input_hosp_data = raw_input_hosp_data,
      forecast_date = forecast_date,
      table_of_exclusions = table_of_exclusions
    )
  ),
  tar_target(
    name = last_hosp_data_date,
    command = get_last_hosp_data_date(input_hosp_data),
    deployment = "main",
    priority = 1
  ),
  tar_target(input_ww_data,
    command = get_input_ww_data(
      forecast_date = forecast_date,
      location = location,
      scenario = scenario,
      scenario_dir = eval_config$scenario_dir,
      ww_data_dir = eval_config$ww_data_dir,
      calibration_time = eval_config$calibration_time,
      last_hosp_data_date = last_hosp_data_date,
      ww_data_mapping = eval_config$ww_data_mapping
    ),
    deployment = "main",
    priority = 1
  ),
  ## Get the stan data for this location, forecast_date, and scenario ----------
  tar_target(
    name = standata,
    command = get_stan_data_list(
      model_type = "ww",
      forecast_date = forecast_date,
      forecast_time = eval_config$forecast_time,
      calibration_time = eval_config$calibration_time,
      input_ww_data = input_ww_data,
      input_hosp_data = input_hosp_data,
      generation_interval = eval_config$generation_interval,
      inf_to_hosp = eval_config$inf_to_hosp,
      infection_feedback_pmf = eval_config$infection_feedback_pmf,
      params
    ),
    deployment = "main",
    priority = 1
  ),
  ## Model fitting ----------------------------------------------------------
  tar_target(
    name = init_lists,
    command = get_inits(
      model_type = "ww", standata, params,
      n_chains = eval_config$n_chains
    ),
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = ww_fit_obj,
    command = sample_model(
      standata,
      stan_model_path = stan_model_path_target,
      stan_models_dir = eval_config$stan_models_dir,
      init_lists,
      iter_warmup = eval_config$iter_warmup,
      iter_sampling = eval_config$iter_sampling,
      adapt_delta = eval_config$adapt_delta,
      n_chains = eval_config$n_chains,
      max_treedepth = eval_config$max_treedepth,
      seed = eval_config$seed
    ),
    deployment = "worker",
    priority = 1
  ),
  ## Post-processing---------------------------------------------------------
  tar_target(
    name = ww_raw_draws,
    command = ww_fit_obj$draws,
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = ww_diagnostics,
    command = ww_fit_obj$diagnostics,
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = ww_diagnostic_summary,
    command = ww_fit_obj$summary_diagnostics,
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = ww_summary,
    command = ww_fit_obj$summary,
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = errors,
    command = ww_fit_obj$error,
    deployment = "main"
  ),
  tar_target(
    name = flags,
    command = ww_fit_obj$flags,
    deployment = "main"
  ),
  # Save errors
  tar_target(
    name = save_ww_errors,
    command = save_table(
      data_to_save = errors,
      type_of_output = "errors",
      output_dir = eval_config$output_dir,
      scenario = scenario,
      forecast_date = forecast_date,
      model_type = "ww",
      location = location
    )
  ),
  # Save flags
  tar_target(
    name = save_ww_flags,
    command = save_table(
      data_to_save = flags,
      type_of_output = "flags",
      output_dir = eval_config$output_dir,
      scenario = scenario,
      forecast_date = forecast_date,
      model_type = "ww",
      location = location
    )
  ),
  # Get evaluation data from hospital admissions and wastewater
  # Join draws with data
  tar_target(
    name = hosp_draws,
    command = {
      if (is.null(ww_raw_draws)) {
        NULL
      } else {
        get_model_draws_w_data(
          model_output = "hosp",
          model_type = "ww",
          draws = ww_raw_draws,
          forecast_date = forecast_date,
          scenario = scenario,
          location = location,
          input_data = input_hosp_data,
          eval_data = eval_hosp_data,
          last_hosp_data_date = last_hosp_data_date,
          ot = eval_config$calibration_time,
          forecast_time = eval_config$forecast_time
        )
      }
    },
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = ww_draws,
    command = {
      if (is.null(ww_raw_draws)) {
        NULL
      } else {
        get_model_draws_w_data(
          model_output = "ww",
          model_type = "ww",
          draws = ww_raw_draws,
          forecast_date = forecast_date,
          scenario = scenario,
          location = location,
          input_data = input_ww_data,
          eval_data = eval_ww_data,
          last_hosp_data_date = last_hosp_data_date,
          ot = eval_config$calibration_time,
          forecast_time = eval_config$forecast_time
        )
      }
    },
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = full_hosp_quantiles,
    command = {
      if (is.null(hosp_draws)) {
        NULL
      } else {
        get_state_level_quantiles(
          draws = hosp_draws
        )
      }
    },
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = full_ww_quantiles,
    command = {
      if (is.null(ww_draws)) {
        NULL
      } else {
        get_state_level_ww_quantiles(
          ww_draws = ww_draws
        )
      }
    },
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = hosp_quantiles,
    command = {
      if (is.null(full_hosp_quantiles)) {
        NULL
      } else {
        full_hosp_quantiles |>
          dplyr::filter(period != "calibration")
      }
    },
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = ww_quantiles,
    command = {
      if (is.null(full_ww_quantiles)) {
        NULL
      } else {
        full_ww_quantiles |>
          dplyr::filter(period != "calibration")
      }
    },
    deployment = "main",
    priority = 1
  ),
  # Save forecasted quantiles locally as well as via
  # targets caching just for backup
  tar_target(
    name = save_forecasted_quantiles_ww,
    command = save_table(
      data_to_save = full_hosp_quantiles,
      type_of_output = "hosp_quantiles",
      output_dir = eval_config$output_dir,
      scenario = scenario,
      forecast_date = forecast_date,
      model_type = "ww",
      location = location
    )
  ),
  tar_target(
    name = save_forecasted_ww_quantiles_ww,
    command = save_table(
      data_to_save = full_ww_quantiles,
      type_of_output = "ww_quantiles",
      output_dir = eval_config$output_dir,
      scenario = scenario,
      forecast_date = forecast_date,
      model_type = "ww",
      location = location
    )
  ),
  ### Plot the draw comparison-------------------------------------
  tar_target(
    name = plot_hosp_draws,
    command = {
      if (is.null(hosp_draws)) {
        NULL
      } else {
        get_plot_hosp_data_comparison(
          hosp_draws,
          location,
          model_type = "ww"
        )
      }
    },
    deployment = "main",
    priority = 1
  ),
  tar_target(
    name = plot_ww_draws,
    command = {
      if (is.null(ww_draws)) {
        NULL
      } else {
        get_plot_ww_data_comparison(
          ww_draws,
          location,
          model_type = "ww"
        )
      }
    },
    deployment = "main",
    priority = 1
  ),

  ## Score hospital admissions forecasts----------------------------------
  tar_target(
    name = hosp_scores,
    command = get_full_scores(hosp_draws, scenario)
  ),
  tar_target(
    name = save_hosp_scores_ww,
    command =
      save_table(
        data_to_save = hosp_scores,
        type_of_output = "scores",
        output_dir = eval_config$output_dir,
        scenario = scenario,
        forecast_date = forecast_date,
        model_type = "ww",
        location = location
      )
  ),
  tar_target(
    name = hosp_scores_quantiles,
    command = get_scores_from_quantiles(hosp_quantiles, scenario)
  ),
  tar_target(
    name = save_hosp_scores_ww_quantiles,
    command =
      save_table(
        data_to_save = hosp_scores_quantiles,
        type_of_output = "scores_quantiles",
        output_dir = eval_config$output_dir,
        scenario = scenario,
        forecast_date = forecast_date,
        model_type = "ww",
        location = location
      )
  )
) # end tar map

# Hospital admissions model fitting loop-----------------------------------------------
mapped_hosp <- tar_map(
  values = list(
    location = eval_config$location_hosp,
    forecast_date = eval_config$forecast_date_hosp
  ),
  tar_target(
    name = stan_model_path_target,
    command = get_model_path(
      model_type = "hosp",
      stan_models_dir = eval_config$stan_models_dir
    ),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = raw_input_hosp_data,
    command = get_input_hosp_data(forecast_date, location,
      hosp_data_dir = eval_config$hosp_data_dir,
      calibration_time = eval_config$calibration_time
    ),
    deployment = "main"
  ),
  tar_target(
    name = input_hosp_data,
    command = exclude_hosp_outliers(
      raw_input_hosp_data = raw_input_hosp_data,
      forecast_date = forecast_date,
      table_of_exclusions = table_of_exclusions
    )
  ),
  tar_target(
    name = last_hosp_data_date,
    command = get_last_hosp_data_date(input_hosp_data),
    deployment = "main"
  ),
  ## Get the stan data for this location, forecast_date, and scenario ----------
  tar_target(
    name = standata,
    command = get_stan_data_list(
      model_type = "hosp",
      forecast_date = forecast_date,
      forecast_time = eval_config$forecast_time,
      calibration_time = eval_config$calibration_time,
      input_ww_data = NA,
      input_hosp_data = input_hosp_data,
      generation_interval = eval_config$generation_interval,
      inf_to_hosp = eval_config$inf_to_hosp,
      infection_feedback_pmf = eval_config$infection_feedback_pmf,
      params = params
    ),
    deployment = "main"
  ),
  ## Model fitting-----------------------------------------------------------
  tar_target(
    name = init_lists,
    command = get_inits(
      model_type = "hosp",
      standata, params,
      n_chains = eval_config$n_chains
    ),
    deployment = "main"
  ),
  tar_target(
    name = hosp_fit_obj,
    command = sample_model(
      standata,
      stan_model_path = stan_model_path_target,
      stan_models_dir = eval_config$stan_models_dir,
      init_lists,
      iter_warmup = eval_config$iter_warmup,
      iter_sampling = eval_config$iter_sampling,
      adapt_delta = eval_config$adapt_delta,
      n_chains = eval_config$n_chains,
      max_treedepth = eval_config$max_treedepth,
      seed = eval_config$seed
    ),
    deployment = "worker",
    priority = 1
  ),
  ## Post-processing---------------------------------------------------------
  tar_target(
    name = hosp_raw_draws,
    command = hosp_fit_obj$draws,
    deployment = "main"
  ),
  tar_target(
    name = hosp_diagnostics,
    command = hosp_fit_obj$diagnostics,
    deployment = "main"
  ),
  tar_target(
    name = hosp_diagnostic_summary,
    command = hosp_fit_obj$summary_diagnostics,
    deployment = "main"
  ),
  tar_target(
    name = hosp_summary,
    command = hosp_fit_obj$summary,
    deployment = "main"
  ),
  tar_target(
    name = errors,
    command = hosp_fit_obj$error,
    deployment = "main"
  ),
  tar_target(
    name = flags,
    command = hosp_fit_obj$flags,
    deployment = "main"
  ),
  # Save errors
  tar_target(
    name = save_hosp_errors,
    command = save_table(
      data_to_save = errors,
      type_of_output = "errors",
      output_dir = eval_config$output_dir,
      scenario = "no_wastewater",
      forecast_date = forecast_date,
      model_type = "hosp",
      location = location
    )
  ),
  # Save flags
  tar_target(
    name = save_hosp_flags,
    command = save_table(
      data_to_save = flags,
      type_of_output = "flags",
      output_dir = eval_config$output_dir,
      scenario = "no_wastewater",
      forecast_date = forecast_date,
      model_type = "hosp",
      location = location
    )
  ),

  # Get evaluation data from hospital admissions and wastewater
  # Join draws with data
  tar_target(
    name = hosp_model_hosp_draws,
    command = get_model_draws_w_data(
      model_output = "hosp",
      model_type = "hosp",
      draws = hosp_raw_draws,
      forecast_date = forecast_date,
      scenario = "no_wastewater",
      location = location,
      input_data = input_hosp_data,
      eval_data = eval_hosp_data,
      last_hosp_data_date = last_hosp_data_date,
      ot = eval_config$calibration_time,
      forecast_time = eval_config$forecast_time
    ),
    deployment = "main"
  ),
  tar_target(
    name = full_hosp_model_quantiles,
    command = get_state_level_quantiles(
      draws = hosp_model_hosp_draws
    ),
    deployment = "main"
  ),
  tar_target(
    name = hosp_model_quantiles,
    command = full_hosp_model_quantiles |>
      dplyr::filter(period != "calibration"),
    deployment = "main"
  ),
  tar_target(
    name = save_forecasted_quantiles_hosp,
    command = save_table(
      data_to_save = hosp_model_quantiles,
      type_of_output = "quantiles",
      output_dir = eval_config$output_dir,
      scenario = "no_wastewater",
      forecast_date = forecast_date,
      model_type = "hosp",
      location = location
    )
  ),
  ### Plot the draw comparison-------------------------------------
  tar_target(
    name = plot_hosp_draws_hosp_model,
    command = get_plot_hosp_data_comparison(
      hosp_model_hosp_draws,
      location,
      model_type = "hosp"
    ),
    deployment = "main"
  ),
  ## Score the hospital admissions only model-------------------------
  tar_target(
    name = hosp_scores,
    command = get_full_scores(hosp_model_hosp_draws,
      scenario = "no_wastewater"
    ),
    deployment = "main"
  ),
  tar_target(
    name = save_scores_hosp,
    command = save_table(
      data_to_save = hosp_scores,
      type_of_output = "scores",
      output_dir = eval_config$output_dir,
      scenario = "no_wastewater",
      forecast_date = forecast_date,
      model_type = "hosp",
      location = location
    )
  ),
  tar_target(
    name = hosp_scores_quantiles,
    command = get_scores_from_quantiles(hosp_model_quantiles,
      scenario = "no_wastewater"
    ),
    deployment = "main"
  ),
  tar_target(
    name = save_scores_hosp_quantiles,
    command = save_table(
      data_to_save = hosp_scores,
      type_of_output = "scores_quantiles",
      output_dir = eval_config$output_dir,
      scenario = "no_wastewater",
      forecast_date = forecast_date,
      model_type = "hosp",
      location = location
    )
  )
) # end tar map


# Summarize the scores and outputs across groups------------------------------
combined_ww_scores <- tar_combine(
  name = all_ww_scores,
  mapped_ww$hosp_scores,
  command = dplyr::bind_rows(!!!.x, .id = "method")
)
combined_hosp_scores <- tar_combine(
  name = all_hosp_scores,
  mapped_hosp$hosp_scores,
  command = dplyr::bind_rows(!!!.x, .id = "method")
)
combined_hosp_errors <- tar_combine(
  name = all_hosp_errors,
  mapped_hosp$errors,
  command = dplyr::bind_rows(!!!.x, .id = "method")
)
combined_ww_errors <- tar_combine(
  name = all_ww_errors,
  mapped_ww$errors,
  command = dplyr::bind_rows(!!!.x, .id = "method")
)
combined_ww_hosp_quantiles <- tar_combine(
  name = all_ww_hosp_quantiles,
  mapped_ww$full_hosp_quantiles,
  command = dplyr::bind_rows(!!!.x, .id = "method")
)
combined_hosp_model_quantiles <- tar_combine(
  name = all_hosp_model_quantiles,
  mapped_hosp$full_hosp_model_quantiles,
  command = dplyr::bind_rows(!!!.x, .id = "method")
)
combined_ww_quantiles <- tar_combine(
  name = all_ww_quantiles,
  mapped_ww$full_ww_quantiles,
  command = dplyr::bind_rows(!!!.x, .id = "method")
)


downstream_targets <- list(
  tar_target(
    name = all_raw_scores,
    command = rbind(all_hosp_scores, all_ww_scores)
  ),
  tar_target(
    name = all_errors,
    command = rbind(all_hosp_errors, all_ww_errors)
  ),

  ## Raw scores-----------------------------------------
  # These are the scores from each scenario and location without buffering
  # by adding what we would have submitted for a submission which would be
  # a mix of model types
  tar_target(
    name = summarized_raw_scores,
    command = scoringutils::summarize_scores(all_ww_scores,
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
        "scenario",
        "period"
      )
    )
  ),

  ## Summary score for model feature comparison-------------------------
  tar_target(
    name = baseline_score,
    command = make_baseline_score_table(
      mock_submission_scores |> dplyr::filter(scenario == "status_quo"),
      baseline_score_table_dir = eval_config$baseline_score_table_dir,
      overwrite_table = eval_config$overwrite_summary_table
    )
  ),
  tar_target(
    name = baseline_score_hosp,
    command = make_baseline_score_table(
      mock_submission_scores |> dplyr::filter(scenario == "no_wastewater"),
      baseline_score_table_dir = eval_config$baseline_score_table_dir,
      overwrite_table = eval_config$overwrite_summary_table
    )
  ),

  ## Plots----------------------------------------------------
  tar_target(
    name = plot_raw_scores,
    command = get_plot_raw_scores(all_raw_scores),
    deployment = "main"
  ),
  tar_target(
    name = plot_summarized_raw_scores,
    command = get_plot_summarized_scores(grouped_all_raw_scores),
    pattern = map(grouped_all_raw_scores),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_summarized_scores,
    command = get_plot_summarized_scores(grouped_submission_scores),
    pattern = map(grouped_submission_scores),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_summarized_scores_w_data,
    command = get_plot_scores_w_data(
      grouped_submission_scores,
      eval_hosp_data,
      eval_config$figure_dir
    ),
    pattern = map(grouped_submission_scores),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = heatmap_scores,
    command = get_heatmap_scores(
      mock_submission_scores
    )
  ),
  tar_target(
    name = final_plot,
    command = get_plot_final_scores(final_summary_scores),
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
      eval_config$figure_dir
    ),
    pattern = map(all_hosp_quantiles),
    iteration = "list"
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

# Generate figures and results from outputs-----------------------------------
list(
  upstream_targets,
  mapped_ww,
  mapped_hosp,
  combined_ww_scores,
  combined_hosp_scores,
  combined_ww_errors,
  combined_hosp_errors,
  combined_ww_hosp_quantiles,
  combined_hosp_model_quantiles,
  combined_ww_quantiles,
  downstream_targets
)
