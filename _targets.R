# Created by use_targets().
# Prod branch test
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # tar_render() calls
library(crew) # To run in parallel


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
  packages = c(
    "tibble", "dplyr", "lubridate",
    "cmdstanr", "posterior", "yaml", "zoo",
    "tidybayes", "scales", "here",
    "fs", "scales", "gridExtra", "httr",
    "jsonlite", "cfaforecastrenewalww"
  ),
  workspace_on_error = TRUE,
  # Run with a pre-specified crew controller
  controller = controller,
  # Setup storage on workers vs on the main node.
  # This will only work on workers that have access to the data
  # See https://books.ropensci.org/targets/performance.html#worker-storage
  memory = "transient",
  storage = "worker",
  retrieval = "worker",
  format = "rds", # default storage format
  error = NULL # tells errored targets to return NULL rather than
  # have whole pipeline fail
  # Set other options as needed.
)
source("src/write_config.R")
cfaforecastrenewalww::setup_secrets(here::here("secrets.yaml"))
# For testing changes to the package without a re-install
# source("cfaforecastrenewalww/R/utils.R") #nolint
# source("cfaforecastrenewalww/R/get_data.R") #nolint
# source("cfaforecastrenewalww/R/pre_processing.R") #nolint
# source("cfaforecastrenewalww/R/fit_model.R") #nolint
# source("cfaforecastrenewalww/R/get_config_vals.R") #nolint
# source("cfaforecastrenewalww/R/plots.R") #nolint
# source("cfaforecastrenewalww/R/delay_distribs.R") #nolint
# source("cfaforecastrenewalww/R/process_model_outputs.R") #nolint
# source("cfaforecastrenewalww/R/forecasttools.R") #nolint

# Targets list:
# Get the run parameters using the date--------------------------------------
list(
  tar_target(
    name = ww_data_path,
    command = save_timestamped_nwss_data(
      ww_path_to_save =
        file.path(
          "input", "ww_data",
          "nwss_data"
        )
    ),
    deployment = "main"
  ),
  tar_target(
    name = forecast_date,
    command = get_nearest_forecast_date(),
    deployment = "main"
  ),
  tar_target(
    name = hosp_reporting_delay,
    command = get_hosp_reporting_delay(forecast_date),
    deployment = "main"
  ),
  tar_target(
    name = prod_model_type,
    command = "site-level infection dynamics",
    deployment = "main"
  ),
  tar_target(
    name = hosp_only_states,
    command = c(),
    deployment = "main"
  ),
  # If want to enforce a global change, need to tar_destroy to
  # regenerate run_id
  tar_target(
    name = run_id,
    command = get_random_string(forecast_date, date_run, ww_data_path),
    deployment = "main"
  ),
  tar_target(
    name = date_run,
    command = as.character(lubridate::today()),
    deployment = "main"
  ),

  # Write config files ------------------------------------------------------
  tar_target(
    name = fp_config_yaml_ho,
    command = write_config(
      save_config = TRUE,
      location = NULL,
      prod_run = TRUE,
      run_id = run_id,
      ww_geo_type = "state",
      date_run = date_run,
      model_type = "state-level aggregated wastewater",
      forecast_date = forecast_date,
      hosp_data_source = "NHSN",
      pull_from_local = FALSE,
      include_ww = 0,
      hosp_reporting_delay = hosp_reporting_delay,
      ww_data_path = ww_data_path
    ),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = fp_config_yaml_sa,
    command = write_config(
      save_config = TRUE,
      location = "US",
      prod_run = TRUE,
      run_id = run_id,
      ww_geo_type = "state",
      date_run = date_run,
      model_type = "state-level aggregated wastewater",
      forecast_date = forecast_date,
      hosp_data_source = "NHSN",
      pull_from_local = FALSE,
      include_ww = 1,
      hosp_reporting_delay = hosp_reporting_delay,
      ww_data_path = ww_data_path
    ),
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = fp_config_yaml_id,
    command = write_config(
      save_config = TRUE,
      location = NULL,
      prod_run = TRUE,
      run_id = run_id,
      date_run = date_run,
      model_type = "site-level infection dynamics",
      forecast_date = forecast_date,
      hosp_data_source = "NHSN",
      pull_from_local = FALSE,
      include_ww = 1,
      hosp_reporting_delay = hosp_reporting_delay,
      ww_data_path = ww_data_path
    ),
    format = "file",
    deployment = "main"
  ),


  # Hospital admissions only model for all states for a comparison-----------
  tar_target(
    name = config_vars_ho,
    command = get_config_vals(fp_config_yaml_ho),
    deployment = "main"
  ),
  tar_target(
    name = output_dir_ho,
    command = config_vars_ho$output_dir,
    deployment = "main"
  ),
  tar_target(
    name = figure_file_path,
    command = get_figure_file_path(
      output_dir_ho
    ),
    deployment = "main"
  ),
  tar_target(
    name = create_output_dir_ho,
    command = create_dir(output_dir_ho),
    deployment = "main"
  ),
  tar_target(
    name = ww_data_raw_ho,
    command = do.call(get_ww_data, config_vars_ho),
    deployment = "main"
  ),
  tar_target(
    name = train_data_orig_ho,
    command = do.call(get_all_training_data, c(
      list(
        ww_data_raw = ww_data_raw_ho
      ),
      config_vars_ho
    )),
    deployment = "main"
  ),
  tar_target(
    name = train_data_ho,
    command = do.call(manual_removal_of_hosp_data, c(
      list(train_data = train_data_orig_ho),
      config_vars_ho
    )),
    deployment = "main"
  ),
  tar_target(
    name = model_file_path_ho,
    command = config_vars_ho$model_file_path,
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = model_file_ho,
    command = ww_model(model_file_path_ho),
    deployment = "main"
  ),
  tar_target(
    name = grouped_data,
    command = train_data_ho %>%
      group_by(location, include_ww) %>%
      targets::tar_group(),
    iteration = "group",
    deployment = "main"
  ),
  tar_target(
    name = param_file_path_ho,
    command = config_vars_ho$param_file_path,
    format = "file"
  ),
  tar_target(
    name = params_ho,
    command = get_params(param_file_path_ho),
    deployment = "main"
  ),

  ## Fit the model ------------------------------------------------------------
  # get a stacked long dataframe containing the quantiles(estimated
  # from all draws) and 100 samples of the draws from the posterior for the
  # generated quantities and the parameters
  tar_target(
    name = df_of_filepaths_ho,
    command = do.call(fit_aggregated_model, c(list(train_data = grouped_data),
      list(params = params_ho),
      model_file = model_file_ho,
      config_vars_ho
    )),
    pattern = map(grouped_data),
    iteration = "group",
    deployment = "worker"
  ),
  tar_target(
    name = grouped_df,
    command = df_of_filepaths_ho %>%
      group_by(location, model_type) %>%
      distinct() %>%
      targets::tar_group(),
    iteration = "group"
  ),
  tar_target(
    name = plot_single_location_hosp_draws_ho,
    command = get_plot_draws(grouped_df,
      "pred_hosp",
      figure_file_path,
      show_calibration_data = FALSE
    ),
    pattern = map(grouped_df),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_rt_ho,
    command = get_rt_from_draws(
      grouped_df,
      figure_file_path
    ),
    pattern = map(grouped_df),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_params_ho,
    command = get_plot_param_distribs(
      grouped_df,
      figure_file_path
    ),
    pattern = map(grouped_df),
    iteration = "list",
    deployment = "main"
  ),

  # National aggregated model for the US only--------------------------------
  tar_target(
    name = config_vars_sa,
    command = get_config_vals(fp_config_yaml_sa),
    deployment = "main"
  ),
  tar_target(
    name = output_dir_sa,
    command = config_vars_sa$output_dir,
    deployment = "main"
  ),
  tar_target(
    name = create_output_dir_sa,
    command = create_dir(output_dir_sa),
    deployment = "main"
  ),
  tar_target(
    name = ww_data_raw_sa,
    command = do.call(get_ww_data, config_vars_sa),
    deployment = "main"
  ),
  tar_target(
    name = train_data_orig_sa,
    command = do.call(get_all_training_data, c(
      list(
        ww_data_raw = ww_data_raw_sa
      ),
      config_vars_sa
    )),
    deployment = "main"
  ),
  tar_target(
    name = train_data_sa,
    command = do.call(manual_removal_of_hosp_data, c(
      list(train_data = train_data_orig_sa),
      config_vars_sa
    )),
    deployment = "main"
  ),
  tar_target(
    name = model_file_path_sa,
    command = config_vars_sa$model_file_path,
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = model_file_sa,
    command = ww_model(model_file_path_sa),
    deployment = "main"
  ),
  tar_target(
    name = grouped_data_sa,
    command = train_data_sa %>%
      group_by(location, include_ww) %>%
      targets::tar_group(),
    iteration = "group",
    deployment = "main"
  ),
  tar_target(
    name = single_plot_data_sa,
    command = plot_combined_data(grouped_data_sa, figure_file_path),
    pattern = map(grouped_data_sa),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = param_file_path_sa,
    command = config_vars_sa$param_file_path,
    format = "file"
  ),
  tar_target(
    name = params_sa,
    command = get_params(param_file_path_sa),
    deployment = "main"
  ),

  ## Fit the model ------------------------------------------------------------
  # get a stacked long dataframe containing the quantiles(estimated
  # from all draws) and 100 samples of the draws from the posterior for the
  # generated quantities and the parameters
  tar_target(
    name = df_of_filepaths_sa,
    command = do.call(fit_aggregated_model, c(list(train_data = grouped_data_sa),
      list(params = params_sa),
      model_file = model_file_sa,
      config_vars_sa
    )),
    pattern = map(grouped_data_sa),
    iteration = "group",
    deployment = "worker"
  ),
  tar_target(
    name = grouped_df_sa,
    command = df_of_filepaths_sa %>%
      group_by(location, model_type) %>%
      distinct() %>%
      targets::tar_group(),
    iteration = "group"
  ),
  tar_target(
    name = df_of_filepaths_us,
    command = df_of_filepaths_sa %>%
      filter(model_type == "state-level aggregated wastewater"),
    deployment = "main"
  ),
  tar_target(
    name = plot_single_location_hosp_draws_sa,
    command = get_plot_draws(grouped_df_sa,
      "pred_hosp",
      figure_file_path,
      show_calibration_data = FALSE
    ),
    pattern = map(grouped_df_sa),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_single_location_ww_draws_sa,
    command = get_plot_draws(df_of_filepaths_us,
      "pred_ww",
      figure_file_path,
      show_calibration_data = FALSE
    ),
    deployment = "main"
  ),
  tar_target(
    name = plot_rt_sa,
    command = get_rt_from_draws(
      grouped_df_sa,
      figure_file_path
    ),
    pattern = map(grouped_df_sa),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_params_sa,
    command = get_plot_param_distribs(
      grouped_df_sa,
      figure_file_path
    ),
    pattern = map(grouped_df_sa),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_single_location_comb_quantiles_sa,
    command = get_combo_quantile_plot(
      df_of_filepaths_us,
      figure_file_path
    ),
    deployment = "main"
  ),

  # Site level infection dynamics  -----------------------------------
  tar_target(
    name = config_vars_id,
    command = get_config_vals(fp_config_yaml_id),
    deployment = "main"
  ),
  tar_target(
    name = output_dir_id,
    command = config_vars_id$output_dir,
    deployment = "main"
  ),
  tar_target(
    name = create_output_dir_id,
    command = create_dir(output_dir_id),
    deployment = "main"
  ),
  tar_target(
    name = ww_data_raw_id,
    command = do.call(get_ww_data, config_vars_id),
    deployment = "main"
  ),
  tar_target(
    name = train_data_orig_id,
    command = do.call(get_all_training_data, c(
      list(
        ww_data_raw = ww_data_raw_id
      ),
      config_vars_id
    )),
    deployment = "main"
  ),
  tar_target(
    name = train_data_id,
    command = do.call(manual_removal_of_hosp_data, c(
      list(train_data = train_data_orig_id),
      config_vars_id
    )),
    deployment = "main"
  ),
  tar_target(
    name = model_file_path_id,
    command = config_vars_id$model_file_path,
    format = "file",
    deployment = "main"
  ),
  tar_target(
    name = model_file_id,
    command = ww_model(model_file_path_id),
    deployment = "main"
  ),
  tar_target(
    name = grouped_data_id,
    command = train_data_id %>%
      group_by(location) %>%
      targets::tar_group(),
    iteration = "group",
    deployment = "main"
  ),
  tar_target(
    name = param_file_path_id,
    command = config_vars_id$param_file_path,
    format = "file"
  ),
  tar_target(
    name = params_id,
    command = get_params(param_file_path_id),
    deployment = "main"
  ),


  ## Fit the model ------------------------------------------------------------
  # # get a stacked long dataframe containing the quantiles(estimated
  # # from all draws) and 100 samples of the draws from the posterior for the
  # # generated quantities and the parameters
  tar_target(
    name = df_of_filepaths_id,
    command = do.call(
      fit_site_level_model,
      c(list(train_data = grouped_data_id),
        list(params = params_id),
        model_file = model_file_id,
        config_vars_id
      )
    ),
    pattern = map(grouped_data_id),
    iteration = "group",
    deployment = "worker"
  ),
  #  For plotting generated quantities
  tar_target(
    name = grouped_df_id,
    command = df_of_filepaths_id %>%
      filter(model_type == "site-level infection dynamics") %>%
      group_by(location) %>%
      targets::tar_group(),
    iteration = "group",
    deployment = "main"
  ),
  tar_target(
    name = plot_single_location_hosp_draws_id,
    command = get_plot_draws(grouped_df_id,
      "pred_hosp",
      figure_file_path,
      show_calibration_data = FALSE
    ),
    pattern = map(grouped_df_id),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_single_location_comb_quantiles_id,
    command = get_combo_quantile_plot(
      grouped_df_id,
      figure_file_path
    ),
    pattern = map(grouped_df_id),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_ww_site_level_quantiles_id,
    command = get_ww_site_plots(
      grouped_df_id,
      figure_file_path
    ),
    pattern = map(grouped_df_id),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_single_location_ww_draws_id,
    command = get_plot_labsite_ww_draws(
      grouped_df_id,
      figure_file_path
    ),
    pattern = map(grouped_df_id),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_rt_id,
    command = get_rt_from_draws(
      grouped_df_id,
      figure_file_path
    ),
    pattern = map(grouped_df_id),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_rt_site_level,
    command = get_rt_site_level(
      grouped_df_id,
      figure_file_path
    ),
    pattern = map(grouped_df_id),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = plot_params_id,
    command = get_plot_param_distribs(
      grouped_df_id,
      figure_file_path
    ),
    pattern = map(grouped_df_id),
    iteration = "list",
    deployment = "main"
  ),


  # Combine the model outputs-----------------------------------------
  tar_target(
    name = comb_df_filepaths,
    command = rbind(
      df_of_filepaths_ho,
      df_of_filepaths_sa,
      df_of_filepaths_id
    ),
    deployment = "main"
  ),
  tar_target(
    name = grouped_df_comb,
    command = comb_df_filepaths %>%
      ungroup() %>%
      group_by(location) %>%
      targets::tar_group(),
    iteration = "group",
    deployment = "main"
  ),
  tar_target(
    name = plot_mult_models,
    command = get_plot_draws(
      grouped_df_comb,
      "pred_hosp",
      grouping_var = "model_type",
      figure_file_path,
      show_calibration_data = FALSE
    ),
    pattern = map(grouped_df_comb),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = rt_box_plot_id,
    command = get_rt_boxplot_across_states(
      comb_df_filepaths %>%
        ungroup() %>%
        filter(model_type == "site-level infection dynamics"),
      figure_file_path = file.path(figure_file_path, "combined_outputs")
    ),
    deployment = "main"
  ),
  tar_target(
    name = rt_box_plot_ho,
    command = get_rt_boxplot_across_states(
      comb_df_filepaths %>%
        ungroup() %>%
        filter(model_type == "hospital admissions only"),
      figure_file_path = file.path(figure_file_path, "combined_outputs")
    ),
    deployment = "main"
  ),
  tar_target(
    name = pdf_file_path,
    command = get_pdf_file_path(
      output_dir_id
    ),
    deployment = "main"
  ),
  tar_target(
    name = pdf_of_site_level_conc_id,
    command = do.call(
      save_to_pdf,
      c(
        list(
          list_of_plots =
            plot_ww_site_level_quantiles_id
        ),
        type_of_output = "site_level_inf_dynamics",
        pdf_file_path = file.path(pdf_file_path, "internal"),
        n_row = 1,
        n_col = 1,
        config_vars_id
      )
    ),
    deployment = "main"
  ),
  tar_target(
    name = pdf_of_forecasts_id,
    command = do.call(
      save_to_pdf,
      c(
        list(
          list_of_plots =
            plot_single_location_comb_quantiles_id
        ),
        type_of_output = "hosp_and_ww_forecasts_site_level_inf_dyn",
        pdf_file_path = file.path(pdf_file_path, "internal"),
        n_row = 3,
        n_col = 1,
        config_vars_id
      )
    ),
    deployment = "main"
  ),
  tar_target(
    name = pdf_of_forecast_comparisons,
    command = do.call(
      save_to_pdf,
      c(list(list_of_plots = plot_mult_models),
        type_of_output = "forecasts_from_mult_model_types",
        pdf_file_path = file.path(pdf_file_path, "internal"),
        n_row = 3,
        n_col = 1,
        config_vars_id
      )
    ),
    deployment = "main"
  ),
  # Get model run diagnostics for submission -------------------------------------
  tar_target(
    name = full_diagnostics_df,
    command = read_diagnostics_df(df_of_filepaths_id),
    deployment = "main"
  ),
  tar_render(
    name = diagnostic_report,
    path = file.path("model_diagnostics", "diagnostic_report.Rmd"),
    deployment = "main"
  ),

  # Format data for the hub ----------------------------------------------------
  tar_target(
    name = df_of_filepaths,
    command = get_submission_filepath_df(
      prod_model_type = prod_model_type,
      hosp_only_states = hosp_only_states,
      df_of_filepaths_inf_dyn = df_of_filepaths_id,
      df_of_filepaths_agg = df_of_filepaths_us,
      df_of_filepaths_hosp_only = df_of_filepaths_ho,
    ),
    deployment = "main"
  ),
  tar_target(
    name = submission_file_path,
    command = get_submission_file_path(
      output_dir_id
    ),
    deployment = "main"
  ),
  tar_target(
    name = repo_file_path,
    command = get_relative_forecast_dir(forecast_date),
    deployment = "main"
  ),
  tar_target(
    name = hub_submission_df,
    command = do.call(
      get_df_for_hub_submission,
      c(
        list(df_of_filepaths = df_of_filepaths),
        submission_file_path = submission_file_path,
        repo_file_path = repo_file_path,
        config_vars_id
      )
    ),
    deployment = "main"
  ),
  tar_target(
    name = table_of_state_model_designations,
    command = get_summarized_table(
      hub_submission_df,
      full_diagnostics_df,
      hosp_only_states,
      repo_file_path
    ),
    deployment = "main"
  ),
  tar_target(
    name = pipeline_ww_metadata,
    command = get_metadata_yaml(
      full_diagnostics_df,
      repo_file_path,
      hosp_only_states
    )
  ),
  tar_target(
    name = rt_box_plot_hub,
    command = get_rt_boxplot_across_states(
      df_of_filepaths,
      figure_file_path = file.path(pdf_file_path, "internal")
    ),
    deployment = "main"
  ),
  tar_target(
    name = us_run_complete,
    command = "US" %in% hub_submission_df$location,
    deployment = "main"
  ),
  tar_target(
    name = pipeline_metadata,
    command = do.call(
      get_pipeline_metadata,
      c(
        us_run = us_run_complete,
        root_dir = fs::path_wd(),
        config_vars_id
      )
    ),
    deployment = "main"
  ),
  tar_target(
    name = grouped_hub_submission_df,
    command = hub_submission_df %>%
      ungroup() %>%
      group_by(location) %>%
      targets::tar_group(),
    iteration = "group",
    deployment = "main"
  ),
  tar_target(
    name = plot_covidhub_submission,
    command = get_plot_covidhub_submission(grouped_hub_submission_df),
    pattern = map(grouped_hub_submission_df),
    iteration = "list",
    deployment = "main"
  ),
  tar_target(
    name = pdf_of_hub_submissions,
    command = do.call(
      save_to_pdf,
      c(
        list(
          list_of_plots =
            plot_covidhub_submission
        ),
        type_of_output = "COVID_hub_submissions",
        pdf_file_path = file.path(pdf_file_path, "external"),
        n_row = 3,
        n_col = 1,
        config_vars_id
      )
    ),
    deployment = "main"
  )
) # end targets list
