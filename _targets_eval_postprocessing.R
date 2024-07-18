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
      "data.table",
      "ggridges",
      "ggdist"
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
  ),
  # Returns a dataframe with each location and date and a corresponding
  # epidemic phase
  tar_target(
    name = epidemic_phases,
    command = get_epidemic_phases_from_rt(
      locations = unique(eval_config$location_ww),
      retro_rt_path = eval_config$retro_rt_path
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
      eval_output_subdir = eval_config$output_dir,
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
      eval_output_subdir = eval_config$output_dir,
      model_type = "hosp"
    )
  ),
  ## Flags------------------------------------------------------------------
  tar_target(
    name = all_flags_ww,
    command =
      combine_outputs(
        output_type = "flags",
        scenarios = eval_config$scenario,
        forecast_dates = eval_config$forecast_date_ww,
        locations = eval_config$location_ww,
        eval_output_subdir = eval_config$output_dir,
        model_type = "ww"
      )
  ),
  tar_target(
    name = all_flags_hosp,
    command = combine_outputs(
      output_type = "flags",
      scenarios = "no_wastewater",
      forecast_dates = eval_config$forecast_date_hosp,
      locations = eval_config$location_hosp,
      eval_output_subdir = eval_config$output_dir,
      model_type = "hosp"
    )
  ),
  tar_target(
    name = all_flags,
    command = dplyr::bind_rows(all_flags_ww, all_flags_hosp)
  ),
  tar_target(
    name = convergence_df_ww,
    command = get_convergence_df(
      all_flags_ww,
      scenario = "status_quo"
    ) |>
      dplyr::rename(any_flags_ww = any_flags)
  ),
  tar_target(
    name = convergence_df_hosp,
    command = get_convergence_df(all_flags_hosp,
      scenario = "no_wastewater"
    ) |>
      dplyr::rename(any_flags_hosp = any_flags)
  ),
  ### Scores from quantiles-------------------------------------------------
  tar_target(
    name = all_ww_scores_quantiles,
    command = combine_outputs(
      output_type = "scores_quantiles",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir,
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
      eval_output_subdir = eval_config$output_dir,
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
      eval_output_subdir = eval_config$output_dir,
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
      eval_output_subdir = eval_config$output_dir,
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
      eval_output_subdir = eval_config$output_dir,
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
      eval_output_subdir = eval_config$output_dir,
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
      eval_output_subdir = eval_config$output_dir,
      model_type = "hosp"
    )
  )
)

# Head-to-head comparison targets-------------------------------------------
# This set of targets will be conditioned on the presence of sufficient
# wastewater, whereas the below targets assume that for every location and
# forecast date we had to submit a forecast, and so we used the hospital
# admissions only model if wastewater was missing.
# These are only relevant for the status quo scenario
head_to_head_targets <- list(
  tar_target(
    name = all_ww_quantiles_sq,
    command = all_ww_quantiles |>
      dplyr::filter(scenario == "status_quo")
  ),
  # Get a table of locations and forecast dates with sufficient wastewater
  tar_target(
    name = table_of_loc_dates_w_ww,
    command = get_table_sufficient_ww(all_ww_quantiles)
  ),
  # Get a table indicating whether there are locations and forecast dates with
  # convergence issues
  tar_target(
    name = convergence_df,
    command = convergence_df_hosp |>
      dplyr::left_join(convergence_df_ww,
        by = c("location", "forecast_date")
      )
  ),
  # Get the full set of quantiles, filtered down to only states and
  # forecast dates with sufficient wastewater for both ww model and hosp only
  # model. Then join the convergence df
  tar_target(
    name = last_hosp_data_date_map,
    command = get_last_hosp_data_date_map(all_hosp_model_quantiles)
  ),
  tar_target(
    name = hosp_quantiles_filtered,
    command = dplyr::bind_rows(
      all_ww_hosp_quantiles,
      all_hosp_model_quantiles
    ) |>
      dplyr::left_join(table_of_loc_dates_w_ww,
        by = c("location", "forecast_date")
      ) |>
      dplyr::filter(
        ww_sufficient # filters to location forecast dates with sufficient ww
      ) |>
      dplyr::left_join(
        convergence_df,
        by = c(
          "location",
          "forecast_date"
        )
      ) |>
      dplyr::left_join(
        last_hosp_data_date_map,
        by = c("location", "forecast_date")
      ) |>
      dplyr::left_join(
        epidemic_phases,
        by = c(
          "location" = "state_abbr",
          "date" = "reference_date"
        )
      ) |>
      add_horizons()
  ),
  # Do the same thing for the sampled scores, combining ww and hosp under
  # the status quo scenario, filtering to the locations and forecast dates
  # with sufficient wastewater, and then joining the convergence flags
  tar_target(
    name = scores_filtered,
    command = dplyr::bind_rows(
      all_hosp_scores,
      all_ww_scores |>
        dplyr::filter(scenario == "status_quo")
    ) |>
      dplyr::left_join(table_of_loc_dates_w_ww,
        by = c("location", "forecast_date")
      ) |>
      dplyr::filter(ww_sufficient) |>
      dplyr::left_join(
        convergence_df,
        by = c(
          "location",
          "forecast_date"
        )
      ) |>
      dplyr::left_join(
        last_hosp_data_date_map,
        by = c("location", "forecast_date")
      ) |>
      dplyr::left_join(
        epidemic_phases,
        by = c(
          "location" = "state_abbr",
          "date" = "reference_date"
        )
      ) |>
      add_horizons()
  ),
  # Repeat for the quantile-based scores
  tar_target(
    name = scores_quantiles_filtered,
    command = dplyr::bind_rows(
      all_hosp_scores_quantiles,
      all_ww_scores_quantiles |>
        dplyr::filter(scenario == "status_quo")
    ) |>
      dplyr::left_join(table_of_loc_dates_w_ww,
        by = c("location", "forecast_date")
      ) |>
      dplyr::filter(ww_sufficient) |>
      dplyr::left_join(
        convergence_df,
        by = c(
          "location",
          "forecast_date"
        )
      ) |>
      dplyr::left_join(
        last_hosp_data_date_map,
        by = c("location", "forecast_date")
      ) |>
      dplyr::left_join(
        epidemic_phases,
        by = c(
          "location" = "state_abbr",
          "date" = "reference_date"
        )
      ) |>
      add_horizons()
  )
)

# Manuscript figures------------------------------------------------
# Note that these are just the components of the figures, not the full
# ggarranged, properly formatted figures, and currently require
# specification for the figure components that are examples.
manuscript_figures <- list(
  ## Fig 2-----------------------------------------------------
  tar_target(
    name = fig2_hosp_t_1,
    command = make_fig2_hosp_t(
      hosp_quantiles_filtered,
      loc_to_plot = c("MA"),
      date_to_plot = "2024-01-15"
    )
  ),
  tar_target(
    name = fig2_hosp_t_2,
    command = make_fig2_hosp_t(
      hosp_quantiles_filtered,
      loc_to_plot = c("AL"),
      date_to_plot = "2024-01-15"
    )
  ),
  tar_target(
    name = fig2_hosp_t_3,
    command = make_fig2_hosp_t(
      hosp_quantiles_filtered,
      loc_to_plot = c("WA"),
      date_to_plot = "2024-01-15"
    )
  ),
  tar_target(
    name = fig2_ct_1,
    command = make_fig2_ct(
      all_ww_quantiles_sq,
      loc_to_plot = "MA",
      date_to_plot = "2024-01-15"
    )
  ),
  tar_target(
    name = fig2_ct_2,
    command = make_fig2_ct(
      all_ww_quantiles_sq,
      loc_to_plot = "AL",
      date_to_plot = "2024-01-15"
    )
  ),
  tar_target(
    name = fig2_ct_3,
    command = make_fig2_ct(
      all_ww_quantiles_sq,
      loc_to_plot = "WA",
      date_to_plot = "2024-01-15"
    )
  ),
  tar_target(
    name = fig2_forecast_comparison_nowcast1,
    command = make_hosp_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = "MA",
      horizon_to_plot = "nowcast"
    )
  ),
  tar_target(
    name = fig2_forecast_comparison_1wk1,
    command = make_hosp_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = "MA",
      horizon_to_plot = "1 wk"
    )
  ),
  tar_target(
    name = fig2_forecast_comparison_4wks1,
    command = make_hosp_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = "MA",
      horizon_to_plot = "4 wks"
    )
  ),
  tar_target(
    name = fig2_crps_underlay_nowcast1,
    command = make_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = "MA",
      horizon_to_plot = "nowcast"
    )
  ),
  tar_target(
    name = fig2_crps_underlay_1wk1,
    command = make_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = "MA",
      horizon_to_plot = "1 wk"
    )
  ),
  tar_target(
    name = fig2_crps_underlay_4wks1,
    command = make_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = "MA",
      horizon_to_plot = "4 wks"
    )
  ),

  ## Fig3------------------------------------------------
  tar_target(
    name = fig3_rel_crps_over_time,
    command = make_fig3_crps_density(
      scores_filtered
    )
  ),
  tar_target(
    name = fig3_pct_better_w_ww,
    command = make_fig3_pct_better_w_ww(
      scores_filtered,
      eval_hosp_data
    )
  ),
  tar_target(
    name = fig3_rel_crps_by_location,
    command = make_fig3_rel_crps_by_location(
      scores_filtered
    )
  ),
  tar_target(
    name = fig3_rel_crps_overall,
    command = make_fig3_rel_crps_overall(
      scores_filtered
    )
  ),
  tar_target(
    name = fig3_qq_plot_overall,
    command = make_qq_plot_overall(
      scores_quantiles_filtered
    )
  ),
  tar_target(
    name = fig3_plot_coverage_range,
    command = make_plot_coverage_range(
      scores_quantiles_filtered,
      c(30, 50, 90)
    )
  )
)


# Scenario targets------------------------------------------------
scenario_targets <- list(
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
  ## Plots----------------------------------------------------
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
        to = lubridate::ymd(max(eval_config$forecast_date_hosp)),
        by = "week"
      ),
      hub_subdir = eval_config$hub_subdir,
      model_name = "cfa-wwrenewal"
    )
  ),
  tar_target(
    name = metadata_hosp_hub_submissions,
    command = create_hub_submissions(all_hosp_model_quantiles,
      all_hosp_model_quantiles,
      forecast_dates = seq(
        from = lubridate::ymd(
          min(eval_config$forecast_date_hosp)
        ),
        to = lubridate::ymd(max(eval_config$forecast_date_hosp)),
        by = "week"
      ),
      hub_subdir = eval_config$hub_subdir,
      model_name = "cfa-hosponlyrenewal"
    )
  ),
  # Write a function that will get hub scores + all the metadata
  # horizon by week, location, forecast_date + eval data alongside it
  # for the models specified in the eval config
  tar_target(
    name = scores_list_retro_hub_submissions,
    command = score_hub_submissions(
      model_name = c("cfa-wwrenewal", "cfa-hosponlyrenewal"),
      hub_subdir = eval_config$hub_subdir,
      pull_from_github = FALSE,
      dates = seq(
        from = lubridate::ymd(
          min(eval_config$forecast_date_hosp)
        ),
        to = lubridate::ymd(max(eval_config$forecast_date_hosp)),
        by = "week"
      )
    )
  ),
  tar_target(
    name = scores_list_hub_submission_oct_mar,
    command = score_hub_submissions(
      model_name = eval_config$hub_model_names,
      pull_from_github = TRUE,
      dates = seq(
        from = lubridate::ymd(
          min(eval_config$forecast_date_hosp)
        ),
        to = lubridate::ymd(max(eval_config$forecast_date_hosp)),
        by = "week"
      )
    )
  ),
  tar_target(
    name = combine_scores_oct_mar_raw,
    command = dplyr::bind_rows(
      scores_list_retro_hub_submissions$log_scale_scores,
      scores_list_hub_submission_oct_mar$log_scale_scores
    )
  ),
  # Rename the model as retrospective
  tar_target(
    name = combine_scores_oct_mar_full,
    command = combine_scores_oct_mar_raw |> dplyr::mutate(
      model = dplyr::case_when(
        model == "cfa-wwrenewal" ~ "cfa-wwrenewal(retro)",
        model == "cfa-hosponlyrenewal" ~ "cfa-hosponlyrenewal(retro)",
        TRUE ~ model
      )
    )
  ),
  # Filter out the states that not every model has estimates for,
  # start by doing this manually, can write functions if needed as
  # we expand to other models
  tar_target(
    name = combine_scores_oct_mar,
    command = combine_scores_oct_mar_full |>
      dplyr::filter(!location_name %in% c(
        "Virgin Islands",
        "American Samoa",
        "United States"
      ))
  ),
  tar_target(
    name = save_scores_oct_mar,
    command = readr::write_csv(
      combine_scores_oct_mar,
      file.path(eval_config$score_subdir, "scores_oct_mar.csv")
    )
  ),
  tar_target(
    name = scores_list_cfa_ww_real_time,
    command = score_hub_submissions(
      model_name = "cfa-wwrenewal",
      pull_from_github = TRUE,
      dates = seq(
        from = lubridate::ymd(
          "2024-02-05"
        ),
        to = lubridate::ymd(max(eval_config$forecast_date_hosp)),
        by = "week"
      )
    )
  ),
  tar_target(
    name = cfa_real_time_scores,
    command = scores_list_cfa_ww_real_time$log_scale_scores |>
      dplyr::mutate(
        model = ifelse(
          model == "cfa-wwrenewal", "cfa-wwrenewal(real-time)", model
        )
      )
  ),
  tar_target(
    name = combine_scores_feb_mar,
    command = dplyr::bind_rows(
      cfa_real_time_scores,
      combine_scores_oct_mar |> dplyr::filter(
        forecast_date >= lubridate::ymd("2024-02-05")
      )
    )
  ),
  tar_target(
    name = save_scores_feb_mar,
    command = readr::write_csv(
      combine_scores_feb_mar,
      file.path(eval_config$score_subdir, "scores_feb_mar.csv")
    )
  )
)
## Hub comparison plots ------------------------------------------------------
hub_comparison_plots <- list(
  tar_target(
    name = plot_wis_over_time,
    command = get_plot_wis_over_time(
      all_scores = combine_scores_oct_mar,
      cfa_real_time_scores = cfa_real_time_scores,
      figure_file_path = eval_config$figure_dir
    )
  ),
  tar_target(
    name = plot_overall_performance,
    command = get_plot_hub_performance(
      all_scores = combine_scores_oct_mar,
      cfa_real_time_scores = cfa_real_time_scores,
      all_time_period = "Oct 2023-Mar 2024",
      real_time_period = "Feb 2024-Mar 2024",
      figure_file_path = eval_config$figure_dir
    )
  ),
  tar_target(
    name = heatmap_relative_wis_all_time,
    command = get_heatmap_relative_wis(
      scores = combine_scores_oct_mar,
      time_period = "Oct 2023-Mar 2024",
      baseline_model = "COVIDhub-baseline",
      figure_file_path = eval_config$figure_dir
    )
  ),
  tar_target(
    name = heatmap_relative_wis_ensemble,
    command = get_heatmap_relative_wis(
      scores = combine_scores_oct_mar,
      time_period = "Oct 2023-Mar 2024",
      baseline_model = "COVIDhub-4_week_ensemble",
      figure_file_path = eval_config$figure_dir
    )
  ),
  tar_target(
    name = heatmap_relative_wis_hosp_only,
    command = get_heatmap_relative_wis(
      scores = combine_scores_oct_mar,
      time_period = "Oct 2023-Mar 2024",
      baseline_model = "cfa-hosponlyrenewal(retro)",
      figure_file_path = eval_config$figure_dir
    )
  ),
  tar_target(
    name = qq_plot_all_time,
    command = get_qq_plot(
      scores = combine_scores_oct_mar,
      time_period = "Oct 2023-Mar 2024",
      figure_file_path = eval_config$figure_dir
    )
  ),
  tar_target(
    name = qq_plot_feb_mar,
    command = get_qq_plot(
      scores = combine_scores_feb_mar,
      time_period = "Feb 2024-Mar 2024",
      figure_file_path = eval_config$figure_dir
    )
  )
)



# Run the targets pipeline----------------------------------------------------
list(
  upstream_targets,
  combined_targets,
  head_to_head_targets,
  manuscript_figures,
  scenario_targets,
  hub_targets,
  hub_comparison_plots
)
