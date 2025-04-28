library(targets)
library(tarchetypes) # tar_render() calls

controller <- crew::crew_controller_local(
  workers = 8,
  seconds_idle = 600,
  seconds_timeout = 120, # default is 60
)

# Set target options:
tar_option_set(
  workspace_on_error = TRUE,
  packages = c("wweval"),
  # Run with a pre-specified crew controller
  controller = controller,
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  format = "rds",
  error = "continue"
)

## Evaluation data

upstream_targets <- list(
  tar_target(
    name = eval_config,
    command = yaml::read_yaml(fs::path("input",
      "config",
      "eval",
      "eval_config",
      ext = "yaml"
    ))
  ),
  tar_target(
    name = params,
    command = wwinference::get_params(fs::path("input",
      "params",
      ext = "toml"
    )) |>
      tibble::as_tibble()
  ),
  tar_target(
    name = eval_hosp_data,
    command = get_input_hosp_data(
      forecast_date_i = eval_config$eval_date,
      location_i = unique(eval_config$location_hosp),
      hosp_data_dir = eval_config$hosp_data_dir,
      calibration_time = 365, # Grab sufficient data for eval
      for_eval = TRUE # So we don't run wwinference::preprocess
      # If don't have a hospital admissions dataset from the `eval_date`,
      # can load using epidatr
      # population_data_path = eval_config$population_data_path #nolint
    )
  ),
  tar_target(
    name = eval_ww_data,
    command = get_input_ww_data(
      forecast_date_i = eval_config$eval_date,
      location_i = unique(eval_config$location_ww),
      scenario_i = "status_quo",
      scenario_dir = eval_config$scenario_dir,
      ww_data_dir = eval_config$ww_data_dir,
      calibration_time = 365, # Grab sufficient data for eval
      last_hosp_data_date = eval_config$eval_date,
      ww_data_mapping = "most recent",
      for_eval = TRUE
    )
  ),
  tar_target(
    name = grouped_eval_ww_data,
    command = eval_ww_data |>
      dplyr::group_by(location) |>
      targets::tar_group(),
    iteration = "group"
  ),
  tar_target(
    name = plot_ww_eval_data,
    command = get_plot_ww_data(
      grouped_eval_ww_data
    ),
    pattern = map(grouped_eval_ww_data),
    iteration = "list"
  ),
  tar_target(
    name = save_pdf_of_ww_data,
    command = ggplot2::ggsave(
      filename = file.path(
        eval_config$figure_dir,
        glue::glue("eval_ww_data.pdf")
      ),
      plot = gridExtra::marrangeGrob(plot_ww_eval_data, nrow = 1, ncol = 1),
      width = 8.5, height = 11, create.dir = TRUE
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
    ) |> scoringutils:::as_scores(metrics = wweval::sample_metrics)
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
    ) |> scoringutils:::as_scores(metrics = wweval::sample_metrics)
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
  tar_target(
    name = all_ww_data_flags,
    command = combine_outputs(
      output_type = "ww_data_flags",
      scenarios = "status_quo",
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir,
      model_type = "ww"
    )
  ),
  ## Wastewater metadata
  tar_target(
    name = granular_ww_metadata,
    command = combine_and_summarize_ww_data(
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir
    )
  ),

  ## Scores from quantiles
  tar_target(
    name = all_ww_scores_quantiles,
    command = combine_outputs(
      output_type = "scores_quantiles",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir,
      model_type = "ww"
    ) |> scoringutils:::as_scores(metrics = wweval::quantile_metrics)
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
    ) |> scoringutils:::as_scores(metrics = wweval::quantile_metrics)
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
  # Get a table of locations and forecast dates with sufficient wastewater
  tar_target(
    name = table_of_loc_dates_w_ww,
    command = get_table_sufficient_ww(all_ww_data_flags)
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
  tar_target(
    name = ww_forecast_date_locs_to_excl,
    command = as.data.frame(eval_config$ww_forecast_date_locs_to_excl) |>
      dplyr::mutate(forecast_date = lubridate::ymd(forecast_date))
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
      dplyr::filter(
        any_flags_ww == FALSE,
        any_flags_hosp == FALSE
      ) |>
      dplyr::left_join(
        last_hosp_data_date_map,
        by = c("location", "forecast_date")
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
      dplyr::filter(scale == "log") |>
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
      dplyr::filter(
        any_flags_ww == FALSE,
        any_flags_hosp == FALSE
      ) |>
      dplyr::left_join(
        last_hosp_data_date_map,
        by = c("location", "forecast_date")
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
      dplyr::filter(scale == "log") |>
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
      dplyr::filter(
        any_flags_ww == FALSE,
        any_flags_hosp == FALSE
      ) |>
      dplyr::left_join(
        last_hosp_data_date_map,
        by = c("location", "forecast_date")
      ) |>
      add_horizons()
  )
)

# Manuscript analyses ------------------------------------------------
# Note that these are just the components of the figures, not the full
# ggarranged, properly formatted figures, and currently require
# specification for the figure components that are examples.
manuscript_figures <- list(
  ## Forecast date location combinations----------------------------
  tar_target(
    name = table_of_forecast_date_locs,
    command = scores_filtered |>
      dplyr::distinct(forecast_date, location) |>
      dplyr::select(forecast_date, location)
  ),
  tar_target(
    name = scores_filtered_grouped,
    command = scores_filtered |>
      group_by(forecast_date, location) |>
      targets::tar_group(),
    iteration = "group"
  ),
  tar_target(
    name = plot_scores_w_forecasts,
    command = get_plot_scores_and_forecasts(
      scores_filtered_grouped,
      eval_output_subdir = eval_config$output_dir
    ),
    pattern = map(scores_filtered_grouped),
    iteration = "list"
  ),
  ## Summary metadata table-----------------------------------------
  tar_target(
    name = granular_ww_metadata_used,
    command = get_add_ww_metadata(
      granular_ww_metadata,
      ww_forecast_date_locs_to_excl,
      convergence_df,
      table_of_loc_dates_w_ww,
      include_manual_exclusions = FALSE
    )
  ),
  tar_target(
    name = summary_metadata,
    command = get_summary_metadata(
      granular_ww_metadata_used
    )
  ),
  tar_target(
    name = sfig_heatmap_metadata_comp,
    command = get_heatmap_metadata(
      granular_ww_metadata_used,
      type_of_analysis = "retro_comparison",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = sfig_heatmap_metadata_hub_retro,
    command = get_heatmap_metadata_hub(
      granular_ww_metadata_used,
      fig_file_dir = eval_config$ms_fig_dir,
      analysis_type = "retro"
    )
  ),
  tar_target(
    name = sfig_heatmap_metadata_hub_rt,
    command = get_heatmap_metadata_hub(
      granular_ww_metadata_used,
      fig_file_dir = eval_config$ms_fig_dir,
      analysis_type = "real_time"
    )
  ),
  tar_target(
    name = list_of_summary_ww_tables,
    command = get_summary_ww_table(
      granular_ww_metadata_used,
      hosp_quantiles_filtered,
      output_dir = eval_config$output_dir
    )
  ),
  ## Figure specifications----------------------------------------
  tar_target(
    name = locs_to_plot,
    command = c("CA", "VA", "WA")
  ),
  tar_target(
    name = forecast_date_to_plot,
    command = "2024-01-15"
  ),
  tar_target(
    name = quantile_levels_to_plot,
    command = c(0.025, 0.25, 0.5, 0.75, 0.975)
  ),
  tar_target(
    name = hosp_quants_plot,
    command = hosp_quantiles_filtered |>
      dplyr::filter(
        quantile %in% quantile_levels_to_plot,
        location %in% locs_to_plot
      )
  ),
  tar_target(
    name = ww_quants_plot,
    command = combine_outputs(
      output_type = "ww_quantiles",
      scenarios = "status_quo",
      forecast_dates = forecast_date_to_plot,
      locations = locs_to_plot,
      eval_output_subdir = eval_config$output_dir,
      model_type = "ww"
    )
  ),

  ## Figure showing example forecasts for 3 locs, 1 forecast date
  tar_target(
    name = example_hosp_t_1,
    command = plot_model_hosp_t_comparison(
      hosp_quants_plot,
      loc_to_plot = locs_to_plot[1],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = example_hosp_t_2,
    command = plot_model_hosp_t_comparison(
      hosp_quants_plot,
      loc_to_plot = locs_to_plot[2],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = example_hosp_t_3,
    command = plot_model_hosp_t_comparison(
      hosp_quants_plot,
      loc_to_plot = locs_to_plot[3],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = example_ww_conc_1,
    command = plot_ww_conc_by_site(
      ww_quants_plot,
      loc_to_plot = locs_to_plot[1],
      date_to_plot = forecast_date_to_plot,
      site_lab_names_to_show = c(
        "Site: 2590, Lab: 34",
        "Site: 2487, Lab: 34",
        "Site: 2490, Lab: 34"
      )
    )
  ),
  tar_target(
    name = example_ww_conc_2,
    command = plot_ww_conc_by_site(
      ww_quants_plot,
      loc_to_plot = locs_to_plot[2],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = example_ww_conc_3,
    command = plot_ww_conc_by_site(
      ww_quants_plot,
      loc_to_plot = locs_to_plot[3],
      date_to_plot = forecast_date_to_plot
    )
  ),
  ### Fig combined--------------------------------------------
  tar_target(
    name = three_location_forecast_fig,
    command = three_location_forecast_fig(
      hosp1 = example_hosp_t_1,
      hosp2 = example_hosp_t_2,
      hosp3 = example_hosp_t_3,
      ww_conc1 = example_ww_conc_1,
      ww_conc2 = example_ww_conc_2,
      ww_conc3 = example_ww_conc3
    )
  ),

  ## Fig: Example 3 locs, all forecast dates------------------------------
  tar_target(
    name = summary_table_crps,
    command = get_summary_table_fig3(
      scores_filtered,
      locs_to_plot,
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = ex_CA_forecast_score,
    command = get_ind_forecast_score(
      scores_filtered,
      "CA",
      "2024-02-05"
    )
  ),
  tar_target(
    name = ex_WA_forecast_score,
    command = get_ind_forecast_score(
      scores_filtered,
      "WA",
      "2023-11-06"
    )
  ),
  tar_target(
    ### First location --------
    name = fig3_crps_single_loc1,
    command = make_fig3_single_loc_comp(
      scores_filtered,
      loc_to_plot = locs_to_plot[1]
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_nowcast1,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[1],
      horizon_to_plot = "nowcast",
      horizon_days_ahead = -10
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_1wk1,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[1],
      horizon_to_plot = "1 wk",
      horizon_days_ahead = 7
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_4wks1,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[1],
      horizon_to_plot = "4 wks",
      horizon_days_ahead = 28
    )
  ),
  tar_target(
    name = fig3_crps_underlay_nowcast1,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[1],
      horizon_to_plot = "nowcast",
      horizon_days_ahead = -10,
      days_to_shift = -8
    )
  ),
  tar_target(
    name = fig3_crps_underlay_1wk1,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[1],
      horizon_to_plot = "1 wk",
      horizon_days_ahead = 7,
      days_to_shift = 2
    )
  ),
  tar_target(
    name = fig3_crps_underlay_4wks1,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[1],
      horizon_to_plot = "4 wks",
      horizon_days_ahead = 28,
      days_to_shift = 24
    )
  ),
  # This is supplementary but useful alongside
  # the forecasts I think
  tar_target(
    name = sfig3_interval_coverage1,
    command = make_plot_coverage_range(
      scores_quantiles_filtered |>
        dplyr::filter(location == locs_to_plot[1]),
      ranges = c(30, 60, 90)
    )
  ),
  tar_target(
    name = sfig3_qq_plot1,
    command = make_qq_plot_overall(
      scores_quantiles_filtered |>
        dplyr::filter(location == locs_to_plot[1])
    )
  ),

  ### Second loc--------------
  tar_target(
    name = fig3_crps_single_loc2,
    command = make_fig3_single_loc_comp(
      scores_filtered,
      loc_to_plot = locs_to_plot[2]
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_nowcast2,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[2],
      horizon_to_plot = "nowcast",
      horizon_days_ahead = -10
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_1wk2,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[2],
      horizon_to_plot = "1 wk",
      horizon_days_ahead = 7
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_4wks2,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[2],
      horizon_to_plot = "4 wks",
      horizon_days_ahead = 28
    )
  ),
  tar_target(
    name = fig3_crps_underlay_nowcast2,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[2],
      horizon_to_plot = "nowcast",
      days_to_shift = -8,
      horizon_days_ahead = -10
    )
  ),
  tar_target(
    name = fig3_crps_underlay_1wk2,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[2],
      horizon_to_plot = "1 wk",
      days_to_shift = 2,
      horizon_days_ahead = 7
    )
  ),
  tar_target(
    name = fig3_crps_underlay_4wks2,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[2],
      horizon_to_plot = "4 wks",
      days_to_shift = 24,
      horizon_days_ahead = 28
    )
  ),
  # Supplementary
  tar_target(
    name = sfig3_interval_coverage2,
    command = make_plot_coverage_range(
      scores_quantiles_filtered |>
        dplyr::filter(location == locs_to_plot[2]),
      ranges = c(30, 60, 90)
    )
  ),
  tar_target(
    name = sfig3_qq_plot2,
    command = make_qq_plot_overall(
      scores_quantiles_filtered |>
        dplyr::filter(location == locs_to_plot[2])
    )
  ),
  ### Third loc----
  tar_target(
    name = fig3_crps_single_loc3,
    command = make_fig3_single_loc_comp(
      scores_filtered,
      loc_to_plot = locs_to_plot[3]
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_nowcast3,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[3],
      horizon_to_plot = "nowcast",
      horizon_days_ahead = -10
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_1wk3,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[3],
      horizon_to_plot = "1 wk",
      horizon_days_ahead = 7
    )
  ),
  tar_target(
    name = fig3_forecast_comparison_4wks3,
    command = make_fig3_forecast_comp_fig(
      hosp_quantiles_filtered,
      loc_to_plot = locs_to_plot[3],
      horizon_to_plot = "4 wks",
      horizon_days_ahead = 28
    )
  ),
  tar_target(
    name = fig3_crps_underlay_nowcast3,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[3],
      horizon_to_plot = "nowcast",
      days_to_shift = -8,
      horizon_days_ahead = -10
    )
  ),
  tar_target(
    name = fig3_crps_underlay_1wk3,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[3],
      horizon_to_plot = "1 wk",
      days_to_shift = 2,
      horizon_days_ahead = 7
    )
  ),
  tar_target(
    name = fig3_crps_underlay_4wks3,
    command = make_fig3_crps_underlay_fig(
      scores_filtered,
      loc_to_plot = locs_to_plot[3],
      horizon_to_plot = "4 wks",
      days_to_shift = 24,
      horizon_days_ahead = 28
    )
  ),
  # Supplement to fig 3
  tar_target(
    name = sfig3_interval_coverage3,
    command = make_plot_coverage_range(
      scores_quantiles_filtered |>
        dplyr::filter(location == locs_to_plot[3]),
      ranges = c(30, 60, 90)
    )
  ),
  tar_target(
    name = sfig3_qq_plot3,
    command = make_qq_plot_overall(
      scores_quantiles_filtered |>
        dplyr::filter(location == locs_to_plot[3])
    )
  ),

  ### Fig combined---------------------------------------
  tar_target(
    name = fig3,
    command = make_fig3(
      fig3_crps_single_loc1 = fig3_crps_single_loc1,
      fig3_forecast_comparison_nowcast1 = fig3_forecast_comparison_nowcast1,
      fig3_forecast_comparison_1wk1 = fig3_forecast_comparison_1wk1,
      fig3_forecast_comparison_4wks1 = fig3_forecast_comparison_4wks1,
      fig3_crps_underlay_nowcast1 = fig3_crps_underlay_nowcast1,
      fig3_crps_underlay_1wk1 = fig3_crps_underlay_1wk1,
      fig3_crps_underlay_4wks1 = fig3_crps_underlay_4wks1,
      fig3_crps_single_loc2 = fig3_crps_single_loc2,
      fig3_forecast_comparison_nowcast2 = fig3_forecast_comparison_nowcast2,
      fig3_forecast_comparison_1wk2 = fig3_forecast_comparison_1wk2,
      fig3_forecast_comparison_4wks2 = fig3_forecast_comparison_4wks2,
      fig3_crps_underlay_nowcast2 = fig3_crps_underlay_nowcast2,
      fig3_crps_underlay_1wk2 = fig3_crps_underlay_1wk2,
      fig3_crps_underlay_4wks2 = fig3_crps_underlay_4wks2,
      fig3_crps_single_loc3 = fig3_crps_single_loc3,
      fig3_forecast_comparison_nowcast3 = fig3_forecast_comparison_nowcast3,
      fig3_forecast_comparison_1wk3 = fig3_forecast_comparison_1wk3,
      fig3_forecast_comparison_4wks3 = fig3_forecast_comparison_4wks3,
      fig3_crps_underlay_nowcast3 = fig3_crps_underlay_nowcast3,
      fig3_crps_underlay_1wk3 = fig3_crps_underlay_1wk3,
      fig3_crps_underlay_4wks3 = fig3_crps_underlay_4wks3,
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),



  ## Fig: Retrospective relative performance---------------------------------
  tar_target(
    name = fig4_results_tables,
    command = make_fig4_results_table(
      scores_filtered
    )
  ),
  tar_target(
    name = fig4_rel_crps_over_time,
    command = make_fig4_rel_crps_over_time(
      scores_filtered
    )
  ),
  tar_target(
    name = loc_summary,
    command = get_loc_rel_crps(
      scores_filtered,
      locs = c("DC", "OH", "NH", "CO", "IL", "IN")
    )
  ),
  tar_target(fig4_rel_crps_heatmap,
    command = get_plot_rel_crps_heatmap(
      scores = scores_filtered,
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = fig4_rel_crps_hist,
    command = get_plot_rel_crps_distrib(
      scores = scores_filtered,
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = fig4_natl_admissions,
    command = make_fig4_admissions_overall(
      eval_hosp_data,
      first_forecast_date = min(eval_config$forecast_date_ww),
      last_forecast_date = max(eval_config$forecast_date_ww)
    )
  ),
  tar_target(
    name = fig4_avg_crps,
    command = make_fig4_avg_crps_over_time(
      scores_filtered
    )
  ),
  tar_target(
    name = fig4_pct_better_w_ww,
    command = make_fig4_pct_better_w_ww(
      scores_filtered,
      eval_hosp_data
    )
  ),
  tar_target(
    name = fig4_rel_crps_by_phase,
    command = make_fig4_rel_crps_by_phase(
      scores_filtered
    )
  ),
  tar_target(
    name = fig4_rel_crps_by_location,
    command = make_fig4_rel_crps_by_location(
      scores_filtered
    )
  ),
  tar_target(
    name = fig4_rel_crps_overall,
    command = make_fig4_rel_crps_overall(
      scores_filtered,
      fig_file_dir = eval_config$ms_fig_dir,
      write_files = TRUE
    )
  ),
  tar_target(
    name = fig4_qq_plot_overall,
    command = make_qq_plot_overall(
      scores_quantiles_filtered,
      time_period = "retro_all_time",
      fig_file_dir = eval_config$ms_fig_dir,
      write_files = TRUE
    )
  ),
  tar_target(
    name = fig4_plot_coverage_range,
    command = make_plot_coverage_range(
      scores_quantiles_filtered,
      ranges = c(30, 60, 90),
      time_period = "retro_all_time",
      fig_file_dir = eval_config$ms_fig_dir,
      write_files = TRUE
    )
  ),
  ### Fig combined---------------------------------------------
  tar_target(
    name = fig4,
    command = make_fig4(
      fig4_rel_crps_heatmap = fig4_rel_crps_heatmap,
      fig4_rel_crps_hist = fig4_rel_crps_hist,
      fig4_avg_crps = fig4_avg_crps,
      fig4_natl_admissions = fig4_natl_admissions,
      fig4_rel_crps_over_time = fig4_rel_crps_over_time,
      fig4_rel_crps_by_location = fig4_rel_crps_by_location,
      time_period = "all_time",
      fig_file_dir = eval_config$ms_fig_dir
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
  )
)

# Real-time relative targets--------------------------------------
real_time_rel_targets <- list(
  tar_target(
    name = real_time_crps_both_models,
    command = score_real_time_outputs(
      score_type = "crps",
      real_time_output_dir = eval_config$real_time_output_dir,
      table_of_run_ids = as.data.frame(eval_config$table_of_run_ids),
      locations = unique(eval_config$location_ww),
      eval_data = eval_hosp_data,
      model_types = c("ww", "hosp")
    )
  ),
  tar_target(
    name = real_time_wis_hosp_only,
    command = score_real_time_outputs(
      score_type = "wis",
      real_time_output_dir = eval_config$real_time_output_dir,
      table_of_run_ids = as.data.frame(eval_config$table_of_run_ids),
      locations = unique(eval_config$location_ww),
      eval_data = eval_hosp_data,
      model_types = "hosp"
    )
  ),
  tar_target(
    name = real_time_wis_both_models_raw,
    command = combine_hub_and_local_wis(
      cfa_real_time_scores,
      real_time_wis_hosp_only
    )
  ),
  # For the real-time comparison, we exclude the forecasts that are the
  # same
  tar_target(
    name = real_time_wis_both_models,
    command = real_time_wis_both_models_raw |>
      dplyr::anti_join(ww_forecast_date_locs_to_excl) |>
      # Could eventually replace this with what is on the Hub
      dplyr::left_join(table_of_loc_dates_w_ww) |>
      dplyr::filter(ww_sufficient)
  ),
  tar_target(
    name = rel_mean_wis_real_time,
    command = real_time_wis_both_models |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(by = c("model")) |>
      dplyr::select(model, interval_score) |>
      tidyr::pivot_wider(
        names_from = model,
        values_from = interval_score
      ) |>
      dplyr::mutate(rel_wis = ww / hosp)
  ),
  tar_target(
    name = rel_mean_wis_rt_locs,
    command = real_time_wis_both_models |>
      dplyr::filter(location %in% c("TX", "FL", "IL", "MI")) |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores(by = c("model", "location")) |>
      dplyr::select(model, interval_score, location) |>
      tidyr::pivot_wider(
        names_from = model,
        values_from = interval_score
      ) |>
      dplyr::mutate(rel_wis = ww / hosp)
  )
)

# Hub targets-------------------------------------------------------
hub_targets <- list(
  # Exclude the same locations and forecast dates that we exclude in the
  # retrospective head to head analysis, for only the wastewater model.
  # This mirrors real-time production workflow, where we replaced with
  # hospital admissions model.
  tar_target(
    name = filtered_ww_hosp_quantiles,
    command = hosp_quantiles_filtered |>
      dplyr::filter(model_type == "ww") |>
      dplyr::select(colnames(all_ww_hosp_quantiles))
  ),
  tar_target(
    name = metadata_hub_submissions,
    command = create_hub_submissions(
      filtered_ww_hosp_quantiles,
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
    command = create_hub_submissions(
      all_hosp_model_quantiles,
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
  # Get the models that we will include in the analysis
  tar_target(
    name = covidhub_models_to_score,
    command = query_and_select_models(
      prop_dates_for_incl_hub = eval_config$prop_dates_for_incl_hub,
      prop_locs_for_incl_hub = eval_config$prop_locs_for_incl_hub,
      locations = unique(eval_config$location_hosp),
      forecast_dates = seq(
        from = lubridate::ymd(
          min(eval_config$forecast_date_hosp)
        ),
        to = lubridate::ymd(max(eval_config$forecast_date_hosp)),
        by = "week"
      )
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
      ) # Ensure that local retrospective hub submission files have been made
    ) |> with_dependencies(
      metadata_hub_submissions,
      metadata_hosp_hub_submissions
    )
  ),
  tar_target(
    name = scores_list_hub_submission_oct_mar,
    command = score_hub_submissions(
      model_name = covidhub_models_to_score,
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
  tar_target(
    name = hosp_quantiles_filtered_grouped,
    command = hosp_quantiles_filtered |>
      group_by(forecast_date, location) |>
      targets::tar_group(),
    iteration = "group"
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
      ) |>
      dplyr::filter(location != "US")
  ),
  tar_target(
    name = cfa_hosp_real_time_scores,
    command = format_scores_for_comparison(
      real_time_scores = real_time_wis_both_models_raw,
      other_real_time_scores = cfa_real_time_scores
    )
  ),
  tar_target(
    name = combine_scores_feb_mar,
    command = dplyr::bind_rows(
      cfa_real_time_scores,
      cfa_hosp_real_time_scores,
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
## Hub comparison  ------------------------------------------------------
hub_comparison_plots <- list(
  tar_target(
    name = fig5_summary_table,
    command = make_fig5_table_and_plot(
      combine_scores_oct_mar,
      time_period = "Oct-Mar",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = fig5_summary_table_Feb_Mar,
    command = make_fig5_table_and_plot(
      combine_scores_feb_mar |>
        dplyr::filter(!model %in% c(
          "cfa-hosponlyrenewal(retro)",
          "cfa-wwrenewal(retro)"
        )),
      time_period = "Feb-Mar",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = summarized_scores_oct_mar,
    command = combine_scores_oct_mar |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores()
  ),
  tar_target(
    name = summarized_scores_feb_mar,
    command = combine_scores_feb_mar |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores()
  ),
  tar_target(
    name = summarized_scores_cfa_real_time,
    command = cfa_real_time_scores |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores()
  ),
  tar_target(
    name = models_to_plot,
    command = c(
      "UMass-sarix",
      "CMU-TimeSeries",
      "COVIDhub-4_week_ensemble",
      "cfa-wwrenewal(real-time)",
      "cfa-hosponlyrenewal(real-time)*",
      "cfa-wwrenewal(retro)",
      "cfa-hosponlyrenewal(retro)"
    )
  ),
  ## Fig: Real-time Hub comparison ------------------------------------------
  # This will be the real-time density of relative CRPS compared
  # to covidhub baseline (will need to get the summary stats for this too)
  tar_target(
    name = fig5_density_real_time,
    command = make_fig5_density(
      all_scores = summarized_scores_feb_mar |>
        dplyr::filter(!model %in% c(
          "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal(retro)"
        )),
      models_to_show = models_to_plot,
      analysis_type = "Real-time",
    )
  ),
  # This will be the average WIS across forecast dates for the real-time
  # scores
  tar_target(
    name = fig5_plot_wis_t_real_time,
    command = make_fig5_average_wis(
      all_scores = summarized_scores_feb_mar |>
        dplyr::filter(!model %in% c(
          "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal(retro)"
        )),
      models_to_show = models_to_plot,
      time_period = "Feb-Mar 2024"
    )
  ),
  ## Fig:Real-time relative-----------------------------------------
  tar_target(
    name = wis_scores_rt_summarized,
    command = real_time_wis_both_models |>
      data.table::as.data.table() |>
      scoringutils::summarise_scores()
  ),
  tar_target(
    name = bias_summary,
    command = wis_scores_rt_summarized |>
      dplyr::filter(scale == "log") |>
      dplyr::group_by(model) |>
      dplyr::summarize(avg_bias = mean(bias))
  ),
  tar_target(
    name = fig4_rel_wis_heatmap,
    command = make_fig4_heatmap_rel_wis(
      wis_scores = wis_scores_rt_summarized,
      time_period = "Feb-Mar 2024",
      analysis_type = "Real-time"
    )
  ),
  tar_target(
    name = fig4_rel_wis_hist,
    command = get_plot_rel_wis_distrib(
      wis_scores = wis_scores_rt_summarized
    )
  ),
  tar_target(
    name = fig4_natl_admissions_rt,
    command = make_fig4_admissions_overall(
      eval_hosp_data,
      first_forecast_date = lubridate::ymd("2024-02-05") - lubridate::days(7),
      last_forecast_date = max(eval_config$forecast_date_ww)
    )
  ),
  tar_target(
    name = fig4_avg_wis,
    command = make_fig4_avg_wis_over_time(
      wis_scores_rt_summarized
    )
  ),
  tar_target(
    name = fig4_rel_wis_over_time,
    command = make_fig4_rel_wis_over_time(
      wis_scores_rt_summarized
    )
  ),
  tar_target(
    name = fig4_rel_wis_by_location,
    command = make_fig4_rel_wis_by_location(
      wis_scores_rt_summarized
    )
  ),
  tar_target(
    name = fig4_qq_plot_rt,
    command = make_qq_plot_overall(
      real_time_wis_both_models,
      time_period = "real_time",
      fig_file_dir = eval_config$ms_fig_dir,
      write_files = TRUE
    )
  ),
  tar_target(
    name = fig4_plot_coverage_range_rt,
    command = make_plot_coverage_range(
      scores_quantiles = real_time_wis_both_models |>
        dplyr::mutate(
          horizon_days = as.integer(date - forecast_date),
          horizon = case_when(
            horizon_days <= 7 ~ "1 wk",
            horizon_days <= 14 & horizon_days > 7 ~ "2 wks",
            horizon_days <= 21 & horizon_days > 14 ~ "3 wks",
            horizon_days <= 28 & horizon_days > 21 ~ "4 wks"
          )
        ),
      ranges = c(30, 60, 90),
      time_period = "real_time",
      fig_file_dir = eval_config$ms_fig_dir,
      write_files = TRUE
    )
  ),
  ### Fig combined---------------------------------------------
  tar_target(
    name = fig4_rt,
    command = make_fig4(
      fig4_rel_crps_heatmap = fig4_rel_wis_heatmap,
      fig4_rel_crps_hist = fig4_rel_wis_hist,
      fig4_avg_crps = fig4_avg_wis,
      fig4_natl_admissions = fig4_natl_admissions_rt,
      fig4_rel_crps_over_time = fig4_rel_wis_over_time,
      fig4_rel_crps_by_location = fig4_rel_wis_by_location,
      time_period = "real_time",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),


  ## Fig: Retrospective Hub comparison-------------------------------------------
  tar_target(
    name = fig5_density_all_time,
    command = make_fig5_density(
      all_scores = summarized_scores_oct_mar,
      models_to_show = models_to_plot,
      analysis_type = "Retrospective",
    )
  ),
  tar_target(
    name = fig5_plot_wis_t_all_time,
    command = make_fig5_average_wis(
      all_scores = summarized_scores_oct_mar,
      models_to_show = models_to_plot,
      time_period = "Oct 2023-Mar 2024"
    )
  ),
  tar_target(
    name = sfig5_plot_wis_t_all_time,
    command = make_fig5_average_wis(
      all_scores = summarized_scores_oct_mar,
      models_to_show = unique(combine_scores_oct_mar$model),
      time_period = "Oct 2023-Mar 2024",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = fig5_overall_performance,
    command = make_fig5_hub_performance(
      all_scores = summarized_scores_oct_mar,
      cfa_real_time_scores = summarized_scores_cfa_real_time,
      models_to_show = models_to_plot,
      all_time_period = "Oct 2023-Mar 2024",
      real_time_period = "Feb 2024-Mar 2024",
    )
  ),
  tar_target(
    name = fig5_heatmap_rel_wis_all_time,
    command = make_fig5_heatmap_relative_wis(
      scores = summarized_scores_oct_mar,
      models_to_show = models_to_plot,
      time_period = "Oct 2023-Mar 2024",
      baseline_model = "COVIDhub-4_week_ensemble"
    )
  ),
  tar_target(
    name = fig5_heatmap_rel_wis_feb_mar,
    command = make_fig5_heatmap_relative_wis(
      scores = summarized_scores_feb_mar |>
        dplyr::filter(!model %in% c(
          "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal(retro)"
        )),
      models_to_show = models_to_plot,
      time_period = "Feb 2024-Mar 2024",
      baseline_model = "COVIDhub-4_week_ensemble"
    )
  ),
  tar_target(
    name = fig5_qq_plot_all_time,
    command = make_fig5_qq_plot(
      scores = combine_scores_oct_mar,
      models_to_show = models_to_plot,
      time_period = "Oct 2023-Mar 2024"
    )
  ),
  tar_target(
    name = fig5_qq_plot_feb_mar,
    command = make_fig5_qq_plot(
      scores = combine_scores_feb_mar |>
        dplyr::filter(!model %in% c(
          "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal(retro)"
        )),
      models_to_show = models_to_plot,
      time_period = "Feb-Mar 2024"
    )
  ),
  tar_target(
    name = fig5_all_time_bar_chart,
    make_fig5_bar_chart(
      combine_scores_oct_mar,
      time_period = "Oct-Mar"
    )
  ),
  tar_target(
    name = fig5_real_time_bar_chart,
    make_fig5_bar_chart(
      combine_scores_feb_mar |>
        dplyr::filter(!model %in% c(
          "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal(retro)"
        )),
      time_period = "Oct-Mar"
    )
  ),
  tar_target(
    name = fig5_std_rank_feb_mar,
    command = make_fig5_density_rank(
      scores = summarized_scores_feb_mar |>
        dplyr::filter(!model %in% c(
          "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal(retro)"
        )),
      models_to_show = models_to_plot,
      time_period = "Feb-Mar 2024",
      tp_fp = "rt",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = std_rank_summary_table_rt,
    command = summarize_std_rank(summarized_scores_feb_mar |>
      dplyr::filter(!model %in% c(
        "cfa-wwrenewal(retro)",
        "cfa-hosponlyrenewal(retro)"
      )))
  ),
  tar_target(
    name = fig5_std_rank_all_time,
    command = make_fig5_density_rank(
      scores = summarized_scores_oct_mar,
      models_to_show = models_to_plot,
      time_period = "Oct 2023-Mar 2024",
      tp_fp = "at",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = std_rank_summary_table_at,
    command = summarize_std_rank(summarized_scores_oct_mar)
  ),
  ### Fig Real-time and retro Hub combined---------------------------------------------------
  tar_target(
    name = fig5,
    command = make_fig5(
      fig5_plot_wis_t_real_time = fig5_plot_wis_t_real_time,
      fig5_density_real_time = fig5_density_real_time,
      fig5_density_all_time = fig5_density_all_time,
      fig5_plot_wis_t_all_time = fig5_plot_wis_t_all_time,
      fig5_all_time_bar_chart = fig5_all_time_bar_chart,
      fig5_heatmap_rel_wis_all_time = fig5_heatmap_rel_wis_all_time,
      fig5_heatmap_rel_wis_feb_mar = fig5_heatmap_rel_wis_feb_mar,
      fig5_qq_plot_all_time = fig5_qq_plot_all_time,
      fig5_qq_plot_feb_mar = fig5_qq_plot_feb_mar,
      fig5_real_time_bar_chart = fig5_real_time_bar_chart,
      fig_file_dir = eval_config$ms_fig_dir
    )
  )
)

# Benchmarking----------------------------------------------------------
benchmarks <- list(
  tar_target(
    name = benchmark_table_full_run,
    command = benchmark_performance(
      ww_scores = all_ww_scores,
      hosp_scores = all_hosp_scores,
      benchmark_dir = eval_config$benchmark_dir,
      benchmark_scope = "all_forecasts",
      wwinference_version = eval_config$wwinference_version,
      overwrite_benchmark = eval_config$overwrite_benchmark
    )
  ),
  tar_target(
    name = plot_benchmark_by_loc,
    command = plot_benchmarks(
      grouping_var = "location",
      benchmark_scope = "all_forecasts",
      benchmark_dir = eval_config$benchmark_dir,
      scores_list = benchmark_table_full_run
    )
  ),
  tar_target(
    name = plot_benchmark_by_forecast_date,
    command = plot_benchmarks(
      grouping_var = "forecast_date",
      benchmark_scope = "all_forecasts",
      benchmark_dir = eval_config$benchmark_dir,
      scores_list = benchmark_table_full_run
    )
  )
)

# Supplement ----------------------------------------------------------
# Make some tables with summary stats to include in results
supp_targets <- list(
  tar_target(sfig_hub_perf_heatmap,
    command = get_plot_hub_perf_heatmap(
      scores = summarized_scores_oct_mar,
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(sfig_comb_perf_heatmap,
    command = get_plot_comb_perf_heatmap(
      scores = scores_filtered,
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = sfig_bias_over_time_comparison,
    command = get_plot_bias_over_time(scores_filtered,
      fig_subscript = "comp",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = sfig_bias_over_time_Hub,
    command = get_plot_bias_over_time(combine_scores_oct_mar,
      fig_subscript = "Hub",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = sfig_crps_over_time_comp,
    command = get_plot_score_by_horizon_t(scores_filtered,
      score_type = "crps",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = sfig_n_sites_vs_performance,
    command = get_plot_sites_vs_performance(
      scores_filtered,
      granular_ww_metadata_used,
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = sfig_wis_over_time_Hub,
    command = get_plot_score_by_horizon_t(combine_scores_oct_mar,
      score_type = "interval_score",
      fig_file_dir = eval_config$ms_fig_dir
    )
  ),
  tar_target(
    name = avg_crps_by_horizon,
    command = get_avg_scores_model_horizon(
      scores_filtered,
      "crps"
    )
  ),
  tar_target(
    name = avg_wis_by_horizon_oct_mar,
    command = get_avg_scores_model_horizon(
      combine_scores_oct_mar,
      "interval_score"
    )
  ),
  tar_target(
    name = avg_wis_by_horizon_feb_mar,
    command = get_avg_scores_model_horizon(
      combine_scores_feb_mar,
      "interval_score"
    )
  ),
  tar_target(
    name = comp_stats,
    command = get_stats_improved_forecasts(
      scores = scores_filtered,
      threshold = 1.1
    )
  ),
  tar_target(
    name = comp_stats_rt,
    command = get_stats_imp_forecasts_wis(
      scores = wis_scores_rt_summarized,
      threshold = 1.1
    )
  ),
  tar_target(
    name = ww_quants_feb_OH_IL,
    command = combine_outputs(
      output_type = "ww_quantiles",
      scenarios = "status_quo",
      forecast_dates = c("2024-02-12"),
      locations = c("OH", "IL"),
      eval_output_subdir = eval_config$output_dir,
      model_type = "ww"
    )
  ),
  tar_target(
    name = plot_ww_feb_OH,
    command = plot_ww_conc_by_site(
      ww_quants_feb_OH_IL,
      loc_to_plot = "OH",
      max_n_site_labs_to_show = 12,
      date_to_plot = "2024-02-12"
    )
  ),
  tar_target(
    name = plot_ww_feb_IL,
    command = plot_ww_conc_by_site(
      ww_quants_feb_OH_IL,
      loc_to_plot = "IL",
      max_n_site_labs_to_show = 12,
      date_to_plot = "2024-02-12",
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
  hub_comparison_plots,
  supp_targets,
  benchmarks,
  real_time_rel_targets
)
