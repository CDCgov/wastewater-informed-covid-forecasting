library(targets)
library(tarchetypes) # tar_render() calls

controller <- crew::crew_controller_local(
  workers = 8,
  seconds_idle = 600,
  seconds_timeout = 120,
)

# Set target options:
tar_option_set(
  workspace_on_error = TRUE,
  packages = c("wweval"),
  controller = controller,
  memory = "transient",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker",
  format = "rds",
  error = "continue"
)

save_figure <- function(
  fig,
  dir = fs::path_wd(),
  fig_name = NULL,
  ext = "jpg",
  ...
) {
  if (is.null(fig_name)) {
    fig_name <- deparse(substitute(fig))
  }
  outpath <- fs::path(dir, fig_name, ext = ext)
  cowplot::save_plot(
    filename = outpath,
    plot = fig,
    ...
  )
  return(outpath)
}


configuration_targets <- list(
  tar_target(
    name = eval_config,
    command = yaml::read_yaml(fs::path(
      "input",
      "config",
      "eval",
      "eval_config",
      ext = "yaml"
    ))
  ),
  tar_target(
    name = params,
    command = wwinference::get_params(fs::path(
      "input",
      "params",
      ext = "toml"
    )) |>
      tibble::as_tibble()
  ),
  tar_target(
    name = first_scored_forecast_date,
    command = lubridate::ymd("2023-10-16")
  ),
  tar_target(
    name = last_scored_forecast_date,
    command = lubridate::ymd("2024-03-25")
  ),
  tar_target(
    name = first_real_time_forecast_date,
    command = lubridate::ymd("2024-02-05")
  ),
  tar_target(
    name = last_real_time_forecast_date,
    command = lubridate::ymd("2024-03-25")
  ),
  tar_target(
      name = real_time_forecast_dates,
      command = seq(
      from = first_real_time_forecast_date,
      to = last_real_time_forecast_date,
      by = "week")
  ),
  tar_target(
      name = exclusions_real_time,
      command = parse_real_time_exclusions(
          eval_config$real_time_metadata_dir)
  ),
  tar_target(
    name = date_locs_manual_exclude_ww_real_time,
    command = dplyr::filter(exclusions_real_time,
                            .data$exclusion == "manual_exclude_ww") |>
        dplyr::select("forecast_date", "location")
  ),
  tar_target(
    name = date_locs_absent_ww_real_time,
    command = dplyr::filter(exclusions_real_time,
                            .data$exclusion == "absent_ww") |>
        dplyr::select("forecast_date", "location")
  ),
  tar_target(
    name = date_locs_insufficient_ww_real_time,
    command = dplyr::filter(exclusions_real_time,
                            .data$exclusion == "insufficient_ww") |>
        dplyr::select("forecast_date", "location")
  ),
  tar_target(
    name = date_locs_manual_exclude_both_real_time,
    command = dplyr::filter(exclusions_real_time,
                            .data$exclusion == "manual_exclude_both") |>
        dplyr::select("forecast_date", "location")
  ),
  tar_target(
    name = fig_output_dir,
    command = fs::dir_create(eval_config$figure_dir)
  ),
  tar_target(
    name = fig_main_dir,
    command = fs::dir_create(fs::path(fig_output_dir, "main"))
  ),
  tar_target(
    name = fig_supp_dir,
    command = fs::dir_create(fs::path(fig_output_dir, "supp"))
  ),
  tar_target(
    name = save_fig_main,
    command = purrr::partial(save_figure, dir = fig_main_dir)
  ),
  tar_target(
    name = save_fig_supp,
    command = purrr::partial(save_figure, dir = fig_supp_dir)
  ),
  tar_target(
    name = scored_forecast_dates,
    command = unique(eval_config$forecast_date_hosp) |>
      lubridate::ymd() |>
      purrr::discard(\(x) {
        (x < first_scored_forecast_date) |
          (x > last_scored_forecast_date)
      })
  ),
  tar_target(
    name = scored_fcst_dates_real_time,
    command = scored_forecast_dates |>
      purrr::discard(\(x) {
        (x < first_real_time_forecast_date) |
          (x > last_real_time_forecast_date)
      })
  ),
  tar_target(
    name = eval_hosp_data,
    command = get_input_hosp_data(
      forecast_date_i = eval_config$eval_date,
      location_i = unique(eval_config$location_hosp),
      hosp_data_dir = eval_config$hosp_data_dir,
      calibration_time = 365, # Grab sufficient data for eval
      for_eval = TRUE # So we don't run wwinference::preprocess
    )
  )
)

data_targets <- list(
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
      dplyr::group_by(.data$location) |>
      targets::tar_group(),
    iteration = "group"
  )
)

collated_output_targets <- list(
  tar_target(
    name = all_ww_scores,
    command = combine_outputs(
      output_type = "scores",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir,
      model_type = "ww"
    ) |>
      dplyr::mutate(
        model = dplyr::case_match(
          .data$model,
          "ww" ~ "cfa-wwrenewal(retro)",
          "hosp" ~ "cfa-hosponlyrenewal(retro)",
          .default = .data$model
        )
      ) |>
      scoringutils:::as_scores(
        metrics = names(wweval::sample_metrics)
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
    ) |>
      dplyr::mutate(
        model = dplyr::case_match(
          .data$model,
          "ww" ~ "cfa-wwrenewal(retro)",
          "hosp" ~ "cfa-hosponlyrenewal(retro)",
          .default = .data$model
        )
      ) |>
      scoringutils:::as_scores(
        metrics = names(wweval::sample_metrics)
      )
  ),
  tar_target(
    name = all_flags_ww,
    command = combine_outputs(
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
    command = get_convergence_df(
      all_flags_hosp,
      scenario = "no_wastewater"
    ) |>
      dplyr::rename(any_flags_hosp = "any_flags")
  ),
  tar_target(
    name = all_ww_data_flags,
    command = combine_outputs(
      output_type = "ww_data_flags",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir,
      model_type = "ww"
    )
  ),
  tar_target(
    name = granular_ww_metadata,
    command = combine_and_summarize_ww_data(
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir
    )
  ),
  tar_target(
    name = all_ww_scores_quantiles,
    command = combine_outputs(
      output_type = "scores_quantiles",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir,
      model_type = "ww"
    ) |>
      dplyr::mutate(
        model = dplyr::case_match(
          .data$model,
          "ww" ~ "cfa-wwrenewal(retro)",
          "hosp" ~ "cfa-hosponlyrenewal(retro)",
          .default = .data$model
        )
      ) |>
      scoringutils:::as_scores(
        metrics = names(wweval::quantile_metrics)
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
    ) |>
      dplyr::mutate(
        model = dplyr::case_match(
          .data$model,
          "ww" ~ "cfa-wwrenewal(retro)",
          "hosp" ~ "cfa-hosponlyrenewal(retro)",
          .default = .data$model
        )
      ) |>
      scoringutils:::as_scores(
        metrics = names(wweval::quantile_metrics)
      )
  ),
  tar_target(
    name = quantile_fcsts_ww_retro,
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
    name = quantile_fcsts_hosp_retro,
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
    name = forecast_diff_draws,
    command = purrr::pmap_df(
      list(
        forecast_date = eval_config$forecast_date_ww,
        location = eval_config$location_ww,
        scenario = eval_config$scenario
      ),
      \(forecast_date, location, scenario) {
        loader <- get_object_loader(
          location,
          forecast_date,
          scenario,
          eval_config$raw_output_dir
        )
        return(loader("forecast_posterior_total_diffs"))
      }
    )
  ),
  tar_target(
    name = trend_draws,
    command = combine_outputs(
      output_type = "trend_draws",
      scenarios = eval_config$scenario,
      forecast_dates = eval_config$forecast_date_ww,
      locations = eval_config$location_ww,
      eval_output_subdir = eval_config$output_dir,
      model_type = "ww"
    )
  ),
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
  ),
  tar_target(
    name = all_raw_scores,
    command = dplyr::bind_rows(all_hosp_scores, all_ww_scores)
  ),
  tar_target(
    name = all_raw_scores_quantiles,
    command = dplyr::bind_rows(
      all_hosp_scores_quantiles,
      all_ww_scores_quantiles
    )
  ),
  tar_target(
    name = all_errors,
    command = dplyr::bind_rows(all_hosp_errors, all_ww_errors)
  ),

  tar_target(
    name = convergence_df,
    command = dplyr::left_join(
      convergence_df_hosp,
      convergence_df_ww,
      by = c("location", "forecast_date")
    )
  ),
  tar_target(
    name = ww_sufficiency_table,
    command = get_table_sufficient_ww(all_ww_data_flags)
  ),
  tar_target(
    name = date_locs_sufficient_ww,
    command = dplyr::filter(ww_sufficiency_table, .data$ww_sufficient) |>
      dplyr::select("forecast_date", "location")
  ),
  tar_target(
    name = date_locs_ww_converged_retro,
    command = dplyr::filter(
      convergence_df_ww,
      .data$any_flags_ww == FALSE
    ) |>
      dplyr::select("forecast_date", "location")
  ),
  tar_target(
    name = date_locs_hosp_converged_retro,
    command = dplyr::filter(
      convergence_df_hosp,
      .data$any_flags_hosp == FALSE
    ) |>
      dplyr::select("forecast_date", "location")
  ),
  tar_target(
    name = date_locs_both_converged_retro,
    command = dplyr::inner_join(
      date_locs_ww_converged_retro,
      date_locs_hosp_converged_retro,
      by = c("forecast_date", "location")
    )
  ),
  tar_target(
    name = date_locs_submit_ww_retro,
    command = dplyr::inner_join(
      date_locs_ww_converged_retro,
      date_locs_sufficient_ww,
      by = c("forecast_date", "location")
    ) |>
      dplyr::inner_join(
        tibble::tibble(forecast_date = scored_forecast_dates),
        by = "forecast_date"
      ) |>
      dplyr::anti_join(
        date_locs_manual_exclude_ww_real_time,
        by = c("forecast_date", "location")
      )
  ),
  tar_target(
    name = date_locs_submit_hosp_retro,
    command = date_locs_hosp_converged_retro
  ),
  tar_target(
    name = date_locs_submit_both_retro,
    command = dplyr::inner_join(
      date_locs_submit_ww_retro,
      date_locs_submit_hosp_retro,
      by = c("forecast_date", "location")
    )
  ),
  tar_target(
    name = date_locs_to_compare_retro,
    command = dplyr::inner_join(
      date_locs_submit_both_retro,
      tibble::tibble(forecast_date = scored_forecast_dates),
      by = "forecast_date"
    )
  ),
  tar_target(
    name = last_hosp_data_date_map,
    command = get_last_hosp_data_date_map(
      quantile_fcsts_hosp_retro
    )
  ),
  tar_target(
    name = submitted_fcsts_hosp_retro,
    command = dplyr::inner_join(
      quantile_fcsts_hosp_retro,
      date_locs_submit_hosp_retro,
      by = c("forecast_date", "location")
    )
  ),
  tar_target(
    name = submitted_fcsts_ww_retro,
    command = dplyr::inner_join(
      dplyr::filter(
        quantile_fcsts_ww_retro,
        .data$scenario == "status_quo"
      ),
      date_locs_submit_ww_retro,
      by = c("forecast_date", "location")
    )
  ),
  tar_target(
    name = submitted_fcsts_cfa_retro,
    command = dplyr::bind_rows(
      submitted_fcsts_ww_retro,
      submitted_fcsts_hosp_retro
    ) |>
      dplyr::left_join(
        last_hosp_data_date_map,
        by = c("location", "forecast_date")
      ) |>
      add_horizons(target_end_date_col = "date") |>
      dplyr::select(-"scenario") |>
      scoringutils::as_forecast_quantile(
        predicted = "value",
        observed = "eval_data",
        quantile_level = "quantile"
      )
  ),
  tar_target(
    name = crps_cfa_models_retro,
    command = dplyr::bind_rows(
      all_hosp_scores,
      all_ww_scores |>
        dplyr::filter(.data$scenario == "status_quo")
    ) |>
      dplyr::filter(.data$scale == "log") |>
      dplyr::inner_join(
        date_locs_to_compare_retro,
        by = c("location", "forecast_date")
      ) |>
      dplyr::left_join(
        last_hosp_data_date_map,
        by = c("location", "forecast_date")
      ) |>
      add_horizons(target_end_date_col = "date") |>
      dplyr::select(-"scenario") |>
      scoringutils:::as_scores(
        metrics = names(wweval::sample_metrics)
      )
  ),
  tar_target(
    name = crps_cfa_models_retro_grouped,
    command = crps_cfa_models_retro |>
      dplyr::group_by(
        .data$forecast_date,
        .data$location
      ) |>
      targets::tar_group(),
    iteration = "group"
  )
)


real_time_rel_targets <- list(
  tar_target(
    name = unfiltered_crps_cfa_models_real_time, # nolint
    command = score_real_time_outputs(
      score_type = "crps",
      real_time_output_dir = eval_config$real_time_output_dir,
      table_of_run_ids = as.data.frame(
        eval_config$table_of_run_ids
      ),
      locations = unique(eval_config$location_ww),
      eval_data = eval_hosp_data,
      offset = eval_config$scoring_offset,
      model_types = c("ww", "hosp")
    )
  ),
  tar_target(
    name = date_locs_to_compare_real_time,
    command = unfiltered_crps_cfa_models_real_time |>
      forecasttools::filter_to_shared_forecasts() |> # both models present
      dplyr::distinct(.data$forecast_date, .data$location) |>
      dplyr::anti_join(
        exclusions_real_time,
        by = c("forecast_date", "location")
      ) |>
      dplyr::filter(.data$forecast_date %in% scored_fcst_dates_real_time)
  ),
  tar_target(
    name = crps_cfa_models_real_time,
    command = dplyr::inner_join(
      unfiltered_crps_cfa_models_real_time,
      date_locs_to_compare_real_time,
      by = c("forecast_date", "location")
    )
  ),
  tar_target(
    name = wis_cfa_models_real_time,
    command = score_real_time_outputs(
      score_type = "wis",
      real_time_output_dir = eval_config$real_time_output_dir,
      table_of_run_ids = as.data.frame(
        eval_config$table_of_run_ids
      ),
      locations = unique(eval_config$location_ww),
      eval_data = eval_hosp_data,
      offset = eval_config$scoring_offset,
      model_types = c("ww", "hosp")
    ) |>
      dplyr::inner_join(
        date_locs_to_compare_real_time,
        by = c("forecast_date", "location")
      ) |>
      scoringutils:::as_scores(
        metrics = names(
          wweval::quantile_metrics
        )
      ) |>
      dplyr::mutate(
        model = dplyr::case_match(
          .data$model,
          "ww" ~ "cfa-wwrenewal(real-time)",
          "hosp" ~ "cfa-hosponlyrenewal(real-time*)",
          .default = .data$model
        )
      )
  ),
  tar_target(
    name = rel_wis_real_time,
    command = wis_cfa_models_real_time |>
      forecasttools::summarise_scores_with_baseline(
        compare = "model",
        baseline = "cfa-hosponlyrenewal(real-time*)",
      ) |>
      dplyr::rename(rel_wis = "mean_scores_ratio")
  )
)

# Hub targets-------------------------------------------------------
hub_comparison_targets <- list(
  # Exclude the same locations and forecast dates that we exclude in the
  # retrospective head to head analysis, for only the wastewater model.
  # This mirrors real-time production workflow, where we replaced with
  # hospital admissions model.
  tar_target(
    name = metadata_hub_submissions,
    command = create_hub_submissions(
      submitted_fcsts_ww_retro,
      submitted_fcsts_hosp_retro,
      forecast_dates = scored_forecast_dates,
      hub_subdir = eval_config$hub_subdir,
      model_name = "cfa-wwrenewal"
    )
  ),
  tar_target(
    name = metadata_hosp_hub_submissions,
    command = create_hub_submissions(
      submitted_fcsts_hosp_retro,
      submitted_fcsts_hosp_retro,
      forecast_dates = scored_forecast_dates,
      hub_subdir = eval_config$hub_subdir,
      model_name = "cfa-hosponlyrenewal"
    )
  ),
  tar_target(
    name = non_cfa_hub_models_to_score,
    command = select_hub_models(
      min_submissions_per_model = eval_config$min_submissions_hub,
      min_locations_per_submission = eval_config$min_locs_per_submission_hub,
      locations = unique(eval_config$location_hosp),
      forecast_dates = scored_forecast_dates
    )
  ),
  tar_target(
    name = hub_locations_to_exclude,
    command = c(
      "VI",
      "AS",
      "US"
    )
  ),
  tar_target(
    name = hub_forecasts_cfa_retro,
    command = pull_hub_forecasts(
      model_name = c("cfa-wwrenewal", "cfa-hosponlyrenewal"),
      dates = scored_forecast_dates,
      eval_data = eval_hosp_data,
      hub_subdir = eval_config$hub_subdir,
      pull_from_github = FALSE
    ) |>
      with_dependencies(
        metadata_hub_submissions,
        metadata_hosp_hub_submissions
      ) |>
      dplyr::mutate(
        model = dplyr::case_match(
          .data$model,
          "cfa-wwrenewal" ~ "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal" ~ "cfa-hosponlyrenewal(retro)",
          .default = .data$model
        )
      ) |>
      dplyr::filter(
        !location %in% .env$hub_locations_to_exclude
      ) |>
      with_dependencies(hub_locations_to_exclude)
  ),
  tar_target(
    name = hub_forecasts_cfa_ww_real_time,
    command = pull_hub_forecasts(
      model_name = "cfa-wwrenewal",
      dates = scored_fcst_dates_real_time,
      eval_data = eval_hosp_data,
      pull_from_github = TRUE
    ) |>
      dplyr::mutate(
        model = dplyr::case_match(
          .data$model,
          "cfa-wwrenewal" ~ "cfa-wwrenewal(real-time)",
          .default = .data$model
        )
      ) |>
      dplyr::filter(
        !location %in% .env$hub_locations_to_exclude
      ) |>
      with_dependencies(hub_locations_to_exclude)
  ),
  tar_target(
    name = hub_forecasts_cfa_hosp_real_time,
    command = load_real_time_quantile_fcsts(
      real_time_output_dir = eval_config$real_time_output_dir,
      table_of_run_ids = as.data.frame(
        eval_config$table_of_run_ids
      ),
      locations = unique(eval_config$location_ww),
      eval_data = eval_hosp_data,
      model_type = "hosp"
    ) |>
      dplyr::mutate(
        model = "cfa-hosponlyrenewal(real-time*)"
      ) |>
      dplyr::filter(
        !location %in% .env$hub_locations_to_exclude
      ) |>
      with_dependencies(hub_locations_to_exclude)
  ),
  tar_target(
    name = hub_forecasts_cfa_real_time,
    command = dplyr::bind_rows(
      hub_forecasts_cfa_ww_real_time,
      hub_forecasts_cfa_hosp_real_time |>
        select_like(hub_forecasts_cfa_ww_real_time)
    )
  ),
  tar_target(
    name = hub_forecasts_non_cfa,
    command = pull_hub_forecasts(
      model_name = non_cfa_hub_models_to_score,
      dates = scored_forecast_dates,
      eval_data = eval_hosp_data,
      pull_from_github = TRUE
    ) |>
      dplyr::filter(
        !location %in% .env$hub_locations_to_exclude
      ) |>
      with_dependencies(hub_locations_to_exclude)
  ),
  tar_target(
    name = hub_forecasts,
    command = dplyr::bind_rows(
      hub_forecasts_cfa_retro,
      hub_forecasts_cfa_real_time |>
        select_like(hub_forecasts_cfa_retro),
      hub_forecasts_non_cfa |>
        select_like(hub_forecasts_cfa_retro)
    )
  ),
  tar_target(
    name = hub_scores,
    command = score_hub_forecasts(hub_forecasts)
  ),
  tar_target(
    name = save_hub_scores,
    command = {
      fp <- fs::path(
        eval_config$score_subdir,
        "hub_scores_all_time",
        ext = "parquet"
      )
      forecasttools::write_tabular_file(hub_scores, fp)
      fp
    },
    format = "file"
  ),
  tar_target(
    name = hub_scores_real_time,
    command = dplyr::filter(
      hub_scores,
      .data$forecast_date >= .env$first_real_time_forecast_date
    ) |>
      with_dependencies(first_real_time_forecast_date)
  ),
  tar_target(
    name = save_scores_real_time,
    command = {
      fp <- fs::path(
        eval_config$score_subdir,
        "hub_scores_real_time",
        ext = "parquet"
      )
      forecasttools::write_tabular_file(
        hub_scores_real_time,
        fp
      )
      fp
    },
    format = "file"
  ),
  tar_target(
    name = hub_average_score_table_all_time,
    command = hub_average_score_table(
      hub_scores
    )
  ),
  tar_target(
    name = hub_average_score_table_real_time,
    command = hub_average_score_table(
      hub_scores_real_time |>
        dplyr::filter(
          !model %in%
            c(
              "cfa-hosponlyrenewal(retro)",
              "cfa-wwrenewal(retro)"
            )
        )
    )
  ),
  tar_target(
    name = models_to_plot,
    command = c(
      "COVIDhub-4_week_ensemble",
      "UMass-sarix",
      "CMU-TimeSeries",
      "cfa-hosponlyrenewal(retro)",
      "cfa-wwrenewal(retro)",
      "cfa-hosponlyrenewal(real-time*)",
      "cfa-wwrenewal(real-time)"
    )
  ),
  tar_target(
    name = wis_summary_cfa_models_real_time,
    command = wis_cfa_models_real_time |>
      scoringutils::summarise_scores()
  ),
  tar_target(
    name = rel_wis_heatmap_real_time,
    command = plot_rel_score_heatmap(
      scores = wis_cfa_models_real_time,
      target_models = "cfa-wwrenewal(real-time)",
      baseline_model = "cfa-hosponlyrenewal(real-time*)",
      metric_to_compare = "wis"
    )
  ),
  tar_target(
    name = rel_wis_dist_real_time,
    command = plot_rel_score_dists(
      scores = wis_cfa_models_real_time,
      target_models = "cfa-wwrenewal(real-time)",
      baseline_model = "cfa-hosponlyrenewal(real-time*)",
      metric_to_compare = "wis",
      x = NULL,
      by = c("location", "forecast_date")
    )
  ),
  tar_target(
    name = total_admissions_real_time,
    command = plot_total_admissions(
      eval_hosp_data,
      first_forecast_date = lubridate::ymd("2024-02-05") -
        lubridate::days(7),
      last_forecast_date = max(eval_config$forecast_date_ww)
    )
  ),
  tar_target(
    name = fig_wis_t_cfa_models_real_time,
    command = plot_score_t(
      wis_cfa_models_real_time,
      metric = "wis",
      model_z_order = models_to_plot
    )
  ),
  tar_target(
    name = save_fig_wis_t_cfa_models_real_time,
    command = save_fig_supp(
      fig_wis_t_cfa_models_real_time,
      base_width = 8,
      base_height = 4
    )
  ),
  tar_target(
    name = fig_rwis_t_cfa_models_real_time,
    command = plot_rel_score_t(
      wis_cfa_models_real_time,
      target_models = "cfa-wwrenewal(real-time)",
      baseline_model = "cfa-hosponlyrenewal(real-time*)",
      metric_to_compare = "wis"
    ) +
      ggplot2::theme(legend.position = "none")
  ),
  tar_target(
    name = save_fig_rwis_t_cfa_models_real_time,
    command = save_fig_supp(
      fig_rwis_t_cfa_models_real_time,
      base_width = 8,
      base_height = 4
    )
  ),
  tar_target(
    name = decomposed_wis_t_cfa_models_real_time,
    command = wis_cfa_models_real_time |>
      scoringutils::summarise_scores(
        by = c("forecast_date", "model")
      ) |>
      plot_score_decomposed_bars(x = "forecast_date")
  ),
  tar_target(
    name = decomposed_wis_loc_cfa_models_real_time,
    command = wis_cfa_models_real_time |>
      scoringutils::summarise_scores(
        by = c("location", "model")
      ) |>
      dplyr::arrange(.data$model, .data$wis) |>
      dplyr::mutate(
        location = factor(
          .data$location,
          levels = unique(.data$location),
          ordered = TRUE
        )
      ) |>
      plot_score_decomposed_bars(
        x = "location",
        width = 0.5
      )
  ),
  tar_target(
    name = qq_plot_real_time,
    command = forecast_qq_plot(
      hub_forecasts_cfa_real_time
    )
  ),
  tar_target(
    name = save_qq_plot_real_time,
    command = save_fig_supp(
      qq_plot_real_time,
      base_width = 7,
      base_height = 7
    ),
    format = "file"
  ),
  tar_target(
    name = interval_coverage_plot_real_time,
    command = forecast_interval_coverage_plot(
      hub_forecasts_cfa_real_time |>
        dplyr::inner_join(
          last_hosp_data_date_map,
          by = c("location", "forecast_date")
        ) |>
        add_horizons(),
      ranges = c(30, 60, 90)
    )
  ),
  tar_target(
    name = save_interval_coverage_plot_real_time,
    command = save_fig_supp(
      interval_coverage_plot_real_time,
      base_width = 10,
      base_height = 6
    ),
    format = "file"
  ),
  tar_target(
    name = figure_rel_performance_real_time,
    command = compose_rel_performance_fig(
      rel_score_heatmap = rel_wis_heatmap_real_time,
      rel_score_dist = rel_wis_dist_real_time,
      abs_score_by_time = fig_rwis_t_cfa_models_real_time,
      total_admissions = total_admissions_real_time,
      scores_by_time = decomposed_wis_t_cfa_models_real_time,
      scores_by_location = decomposed_wis_loc_cfa_models_real_time
    )
  ),
  tar_target(
    name = save_figure_rel_performance_real_time,
    command = save_fig_main(
      figure_rel_performance_real_time,
      base_width = 10,
      base_height = 12
    ),
    format = "file"
  ),
  tar_target(
    name = hub_hist_rwis_all_time,
    command = relative_wis_histogram(
      scores = hub_scores,
      models_to_show = models_to_plot
    )
  ),
  tar_target(
    name = hub_hist_rwis_real_time,
    command = relative_wis_histogram(
      scores = hub_scores_real_time |>
        dplyr::filter(
          !.data$model %in%
            c(
              "cfa-wwrenewal(retro)",
              "cfa-hosponlyrenewal(retro)"
            )
        ),
      models_to_show = models_to_plot
    )
  ),
  tar_target(
    name = hub_wis_t_all_time,
    command = plot_score_t(
      scores = hub_scores |>
        dplyr::filter(
          .data$model %in% .env$models_to_plot
        ),
      metric = "wis",
      model_z_order = models_to_plot
    )
  ),
  tar_target(
    name = hub_wis_t_real_time,
    command = plot_score_t(
      scores = hub_scores_real_time |>
        dplyr::filter(
          !.data$model %in%
            c(
              "cfa-wwrenewal(retro)",
              "cfa-hosponlyrenewal(retro)"
            ),
          .data$model %in% .env$models_to_plot
        ),
      metric = "wis",
      model_z_order = models_to_plot
    )
  ),
  tar_target(
    name = hub_heatmap_rel_wis_all_time,
    command = plot_heatmap_relative_wis(
      scores = hub_scores,
      models_to_show = setdiff(
        models_to_plot,
        c(
          "cfa-wwrenewal(real-time)",
          "cfa-hosponlyrenewal(real-time*)"
        )
      ),
      time_period = "Oct 2023-Mar 2024",
      baseline_model = "COVIDhub-4_week_ensemble"
    )
  ),
  tar_target(
    name = hub_heatmap_rel_wis_real_time,
    command = plot_heatmap_relative_wis(
      scores = hub_scores_real_time,
      models_to_show = setdiff(
        models_to_plot,
        c(
          "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal(retro)"
        )
      ),
      time_period = "Feb 2024-Mar 2024",
      baseline_model = "COVIDhub-4_week_ensemble"
    )
  ),
  tar_target(
    name = hub_qq_plot_all_time,
    command = forecast_qq_plot(
      hub_forecasts |>
        dplyr::filter(
          .data$model %in% .env$models_to_plot
        ),
      model_z_order = models_to_plot
    )
  ),
  tar_target(
    name = hub_qq_plot_real_time,
    command = forecast_qq_plot(
      hub_forecasts |>
        dplyr::filter(
          .data$forecast_date >= .env$first_real_time_forecast_date,
          .data$model %in%
            setdiff(
              .env$models_to_plot,
              c(
                "cfa-wwrenewal(retro)",
                "cfa-hosponlyrenewal(retro)"
              )
            )
        ),
      model_z_order = models_to_plot
    ) |>
      with_dependencies(
        first_real_time_forecast_date
      )
  ),
  tar_target(
    name = hub_scores_plot_all_time,
    command = dplyr::filter(
      hub_scores,
      !.data$model %in%
        c(
          "cfa-wwrenewal(real-time)",
          "cfa-hosponlyrenewal(real-time*)"
        )
    )
  ),
  tar_target(
    name = hub_scores_plot_real_time,
    command = dplyr::filter(
      hub_scores,
      !.data$model %in%
        c(
          "cfa-wwrenewal(retro)",
          "cfa-hosponlyrenewal(retro)"
        ),
      .data$forecast_date >= .env$first_real_time_forecast_date
    ) |>
      with_dependencies(first_real_time_forecast_date)
  ),
  tar_target(
    name = hub_barplot_wis_all_time,
    command = hub_scores_plot_all_time |>
      scoringutils::summarise_scores(by = "model") |>
      dplyr::arrange(.data$wis) |>
      order_col("model") |>
      plot_score_decomposed_bars(color = "black")
  ),
  tar_target(
    name = hub_barplot_wis_real_time,
    command = hub_scores_plot_real_time |>
      scoringutils::summarise_scores(by = "model") |>
      dplyr::arrange(.data$wis) |>
      order_col("model") |>
      plot_score_decomposed_bars(color = "black")
  ),
  tar_target(
    name = hub_wis_t_all_time_all_models,
    command = plot_score_t(
      scores = hub_scores,
      metric = "wis"
    )
  ),
  tar_target(
    name = hub_performance_by_period,
    command = plot_hub_performance_by_period(
      scores = hub_scores,
      models_to_show = models_to_plot,
      all_time_period = "Oct 2023-Mar 2024",
      real_time_period = "Feb 2024-Mar 2024"
    )
  ),
  tar_target(
    name = std_rank_summary_table_all_time,
    command = summarize_std_rank(hub_scores)
  ),
  tar_target(
    name = std_rank_summary_table_real_time,
    command = summarize_std_rank(
      hub_scores_real_time |>
        dplyr::filter(
          !model %in%
            c(
              "cfa-wwrenewal(retro)",
              "cfa-hosponlyrenewal(retro)"
            )
        )
    )
  ),
  tar_target(
    name = fig_std_rank_all_time,
    command = plot_std_rank_distribution(
      scores = hub_scores_plot_all_time,
      models_to_show = models_to_plot
    )
  ),
  tar_target(
    name = save_fig_std_rank_all_time,
    command = save_fig_supp(
      fig_std_rank_all_time,
      base_width = 7,
      base_height = 7
    ),
    format = "file"
  ),
  tar_target(
    name = fig_std_rank_real_time,
    command = plot_std_rank_distribution(
      scores = hub_scores_plot_real_time,
      models_to_show = models_to_plot
    )
  ),
  tar_target(
    name = save_fig_std_rank_real_time,
    command = save_fig_supp(
      fig_std_rank_real_time,
      base_width = 7,
      base_height = 7
    ),
    format = "file"
  ),
  tar_target(
    name = figure_hub_comparison_all_time,
    command = compose_hub_fig(
      plot_wis_t = hub_wis_t_all_time,
      hist_rwis = hub_hist_rwis_all_time,
      barplot_wis = hub_barplot_wis_all_time,
      heatmap_rel_wis = hub_heatmap_rel_wis_all_time,
      qq_plot = hub_qq_plot_all_time
    )
  ),
  tar_target(
    name = figure_hub_comparison_real_time,
    command = compose_hub_fig(
      plot_wis_t = hub_wis_t_real_time,
      hist_rwis = hub_hist_rwis_real_time,
      barplot_wis = hub_barplot_wis_real_time,
      heatmap_rel_wis = hub_heatmap_rel_wis_real_time,
      qq_plot = hub_qq_plot_real_time
    )
  ),
  tar_target(
    name = save_figure_hub_comparison_real_time,
    command = save_fig_main(
      figure_hub_comparison_real_time,
      base_width = 10,
      base_height = 12
    ),
    format = "file"
  ),
  tar_target(
    name = save_figure_hub_comparison_all_time,
    command = save_fig_main(
      figure_hub_comparison_all_time,
      base_width = 10,
      base_height = 12
    ),
    format = "file"
  )
)

trend_analysis_targets <- list(
  tar_target(
    name = diff_and_trend_draws,
    command = dplyr::inner_join(
      forecast_diff_draws,
      trend_draws |>
        dplyr::rename(draw = ".draw") |>
        dplyr::select(-c(".chain", ".iteration")),
      by = c(
        "draw",
        "forecast_date",
        "location"
      )
    ) |>
      dplyr::inner_join(
        date_locs_to_compare_retro,
        by = c("forecast_date", "location")
      ) |>
      dplyr::inner_join(
        crps_cfa_models_retro |>
          forecasttools::summarise_scores_with_baseline(
            baseline = "cfa-hosponlyrenewal(retro)",
            compare = "model",
            by = c("forecast_date", "location")
          ) |>
          dplyr::filter(.data$model == "cfa-wwrenewal(retro)") |>
          dplyr::select(
            "forecast_date",
            "location",
            rel_crps = "mean_scores_ratio"
          ),
        by = c(
          "forecast_date",
          "location"
        )
      )
  ),
  tar_target(
    name = diff_and_trend_qi,
    command = diff_and_trend_draws |>
      dplyr::group_by(
        .data$forecast_date,
        .data$location,
        .data$scenario,
      ) |>
      ggdist::mean_qi(.exclude = "draw") |>
      dplyr::mutate(
        global_slope_hosp_normed = .data$global_slope_hosp /
          sd(.data$global_slope_hosp),
        global_slope_ww_normed = .data$global_slope_ww /
          sd(.data$global_slope_ww),
        diff_slope_ww_hosp = global_slope_ww_normed -
          global_slope_hosp_normed
      )
  ),
  tar_map(
    tidyr::crossing(
      tibble::tibble(
        trend_metric = c(
          "global_slope_hosp",
          "global_slope_ww",
          "sd_slope_ww",
          "diff_slope_ww_hosp",
          "log_diff_ww_hosp"
        ),
        x_transform = c(
          "identity",
          "identity",
          "log10",
          "identity",
          "identity"
        ),
        x_center = c(0, 0, 0.03, 0, 0)
      ),
      tibble::tibble(
        diff_metric = c("log_diff_ww_hosp", "rel_crps", "global_slope_ww"),
        y_transform = c("identity", "log10", "identity"),
        fill_metric = c("rel_crps", "rel_crps", "rel_crps")
      )
    ) |>
      dplyr::filter(.data$trend_metric != .data$diff_metric),
    tar_target(
      name = fig_trend_diff_scatter,
      command = plot_trend_versus_diff(
        diff_and_trend_qi,
        x_metric = trend_metric,
        y_metric = diff_metric,
        fill_metric = fill_metric,
        x_transform = x_transform,
        y_transform = y_transform,
        x_center = x_center,
        shape = 21,
        size = 3,
        color = "black",
        alpha = 0.5
      )
    ),
    tar_target(
      name = save_fig_trend_diff_scatter,
      command = save_fig_supp(
        fig_trend_diff_scatter
      ),
      format = "file"
    ),
    names = c("trend_metric", "diff_metric")
  )
)

composite_figure_targets <- list(
  tar_target(
    name = granular_ww_metadata_used,
    command = combine_ww_and_run_metadata(
      granular_ww_metadata,
      date_locs_manual_exclude_ww_real_time,
      convergence_df,
      ww_sufficiency_table,
      include_manual_exclusions = TRUE
    )
  ),
  tar_target(
    name = summary_metadata,
    command = get_summary_metadata(
      granular_ww_metadata_used
    )
  ),
  tar_target(
    name = fig_heatmap_metadata_retro,
    command = plot_heatmap_metadata_retro(
      granular_ww_metadata_used
    )
  ),
  tar_target(
    name = save_fig_heatmap_metadata_retro,
    command = save_fig_supp(
      fig_heatmap_metadata_retro,
      base_width = 8,
      base_height = 6
    ),
    format = "file"
  ),
  tar_target(
    name = fig_heatmap_metadata_hub_retro,
    command = plot_heatmap_metadata_hub(
      granular_ww_metadata_used,
      analysis_type = "retro"
    )
  ),
  tar_target(
    name = save_fig_heatmap_metadata_hub_retro,
    command = save_fig_supp(
      fig_heatmap_metadata_hub_retro,
      base_width = 8,
      base_height = 6
    ),
    format = "file"
  ),
  tar_target(
    name = fig_heatmap_metadata_hub_real_time,
    command = plot_heatmap_metadata_hub(
      granular_ww_metadata_used,
      analysis_type = "real-time"
    )
  ),
  tar_target(
    name = save_fig_heatmap_metadata_hub_real_time,
    command = save_fig_supp(
      fig_heatmap_metadata_hub_real_time,
      base_width = 8,
      base_height = 6
    ),
    format = "file"
  ),
  tar_target(
    name = list_of_summary_ww_tables,
    command = get_summary_ww_table(
      granular_ww_metadata_used,
      submitted_fcsts_cfa_retro,
      output_dir = eval_config$output_dir
    )
  ),
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
    command = submitted_fcsts_cfa_retro |>
      dplyr::filter(
        quantile_level %in% quantile_levels_to_plot,
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
  tar_target(
    name = example_hosp_t_1,
    command = plot_pred_actual_hosp(
      hosp_quants_plot,
      loc_to_plot = locs_to_plot[1],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = example_hosp_t_2,
    command = plot_pred_actual_hosp(
      hosp_quants_plot,
      loc_to_plot = locs_to_plot[2],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = example_hosp_t_3,
    command = plot_pred_actual_hosp(
      hosp_quants_plot,
      loc_to_plot = locs_to_plot[3],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = example_ww_conc_1,
    command = plot_pred_actual_ww(
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
    command = plot_pred_actual_ww(
      ww_quants_plot,
      loc_to_plot = locs_to_plot[2],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = example_ww_conc_3,
    command = plot_pred_actual_ww(
      ww_quants_plot,
      loc_to_plot = locs_to_plot[3],
      date_to_plot = forecast_date_to_plot
    )
  ),
  tar_target(
    name = figure_pred_act_three_locs,
    command = compose_pred_actual_fig(
      hosp1 = example_hosp_t_1,
      hosp2 = example_hosp_t_2,
      hosp3 = example_hosp_t_3,
      ww1 = example_ww_conc_1,
      ww2 = example_ww_conc_2,
      ww3 = example_ww_conc_3
    )
  ),
  tar_target(
    name = save_figure_pred_act_three_locs,
    command = save_fig_main(
      figure_pred_act_three_locs,
      base_width = 10,
      base_height = 12
    ),
    format = "file"
  ),
  tar_target(
    name = fig_crps_three_example_locs,
    command = plot_score_model_loc(
      crps_cfa_models_retro,
      locs_to_plot
    )
  ),
  tar_target(
    name = save_fig_crps_three_example_locs,
    command = save_fig_supp(
      fig_crps_three_example_locs,
      base_width = 10,
      base_height = 4
    ),
    format = "file"
  ),
  tar_target(
    name = ex_CA_forecast_score,
    command = get_ind_forecast_score(
      crps_cfa_models_retro,
      "CA",
      "2024-02-05"
    )
  ),
  tar_target(
    name = ex_WA_forecast_score,
    command = get_ind_forecast_score(
      crps_cfa_models_retro,
      "WA",
      "2023-11-06"
    )
  ),
  tar_map(
    list(loc = c("CA", "VA", "WA")),
    tar_target(
      name = plot_score_single_loc,
      command = plot_rel_crps_by_horizon(
        crps_cfa_models_retro,
        loc_to_plot = loc
      )
    ),
    tar_target(
      name = plot_forecast_comparison_nowcast,
      command = plot_forecast_comparison_t(
        submitted_fcsts_cfa_retro,
        loc_to_plot = loc,
        horizon_to_plot = "nowcast",
        horizon_days_ahead = -10
      )
    ),
    tar_target(
      name = plot_forecast_comparison_1wk,
      command = plot_forecast_comparison_t(
        submitted_fcsts_cfa_retro,
        loc_to_plot = loc,
        horizon_to_plot = "1 wk",
        horizon_days_ahead = 7
      )
    ),
    tar_target(
      name = plot_forecast_comparison_4wk,
      command = plot_forecast_comparison_t(
        submitted_fcsts_cfa_retro,
        loc_to_plot = loc,
        horizon_to_plot = "4 wks",
        horizon_days_ahead = 28
      )
    ),
    tar_target(
      name = plot_score_underlay_nowcast,
      command = plot_crps_underlay(
        crps_cfa_models_retro,
        loc_to_plot = loc,
        horizon_to_plot = "nowcast",
        horizon_days_ahead = -10,
        days_to_shift = -8
      )
    ),
    tar_target(
      name = plot_score_underlay_1wk,
      command = plot_crps_underlay(
        crps_cfa_models_retro,
        loc_to_plot = loc,
        horizon_to_plot = "1 wk",
        horizon_days_ahead = 7,
        days_to_shift = 2
      )
    ),
    tar_target(
      name = plot_score_underlay_4wk,
      command = plot_crps_underlay(
        crps_cfa_models_retro,
        loc_to_plot = loc,
        horizon_to_plot = "4 wks",
        horizon_days_ahead = 28,
        days_to_shift = 24
      )
    ),
    tar_target(
      name = plot_loc_interval_coverage,
      command = forecast_interval_coverage_plot(
        dplyr::filter(
          calibration_input_cfa_all_time,
          .data$location == loc
        ),
        ranges = c(30, 60, 90)
      )
    ),
    tar_target(
      plot_loc_qq,
      command = forecast_qq_plot(
        dplyr::filter(
          calibration_input_cfa_all_time,
          .data$location == loc
        )
      )
    )
  ),
  tar_target(
    name = figure_example_scores,
    command = compose_example_scores_fig(
      score_single_loc1 = plot_score_single_loc_CA,
      forecast_comparison_nowcast1 = plot_forecast_comparison_nowcast_CA,
      forecast_comparison_1wk1 = plot_forecast_comparison_1wk_CA,
      forecast_comparison_4wk1 = plot_forecast_comparison_4wk_CA,
      score_underlay_nowcast1 = plot_score_underlay_nowcast_CA,
      score_underlay_1wk1 = plot_score_underlay_1wk_CA,
      score_underlay_4wk1 = plot_score_underlay_4wk_CA,
      score_single_loc2 = plot_score_single_loc_VA,
      forecast_comparison_nowcast2 = plot_forecast_comparison_nowcast_VA,
      forecast_comparison_1wk2 = plot_forecast_comparison_1wk_VA,
      forecast_comparison_4wk2 = plot_forecast_comparison_4wk_VA,
      score_underlay_nowcast2 = plot_score_underlay_nowcast_VA,
      score_underlay_1wk2 = plot_score_underlay_1wk_VA,
      score_underlay_4wk2 = plot_score_underlay_4wk_VA,
      score_single_loc3 = plot_score_single_loc_WA,
      forecast_comparison_nowcast3 = plot_forecast_comparison_nowcast_WA,
      forecast_comparison_1wk3 = plot_forecast_comparison_1wk_WA,
      forecast_comparison_4wk3 = plot_forecast_comparison_4wk_WA,
      score_underlay_nowcast3 = plot_score_underlay_nowcast_WA,
      score_underlay_1wk3 = plot_score_underlay_1wk_WA,
      score_underlay_4wk3 = plot_score_underlay_4wk_WA
    )
  ),
  tar_target(
    name = save_figure_example_scores,
    command = save_fig_main(
      figure_example_scores,
      base_width = 10,
      base_height = 12
    ),
    format = "file"
  ),
  tar_target(
    name = score_summary_tables_cfa_models,
    command = get_score_summary_tables(
      crps_cfa_models_retro
    )
  ),
  tar_target(
    name = rel_crps_distribution_t_cfa_models,
    command = plot_rel_score_dists(
      scores = crps_cfa_models_retro,
      target_models = "cfa-wwrenewal(retro)",
      baseline_model = "cfa-hosponlyrenewal(retro)",
      metric_to_compare = "crps",
      x = "forecast_date",
      by = "location"
    )
  ),
  tar_target(
    name = rel_crps_cfa_models_by_loc,
    command = forecasttools::summarise_scores_with_baseline(
      crps_cfa_models_retro,
      compare = "model",
      baseline = "cfa-hosponlyrenewal(retro)",
      by = "location"
    )
  ),
  tar_target(
    rel_crps_heatmap_cfa_models,
    command = plot_rel_score_heatmap(
      scores = crps_cfa_models_retro,
      target_models = "cfa-wwrenewal(retro)",
      baseline_model = "cfa-hosponlyrenewal(retro)",
      metric_to_compare = "crps"
    )
  ),
  tar_target(
    name = rel_crps_distribution_overall_cfa_models,
    command = plot_rel_score_dists(
      scores = crps_cfa_models_retro,
      target_models = "cfa-wwrenewal(retro)",
      baseline_model = "cfa-hosponlyrenewal(retro)",
      metric_to_compare = "crps",
      x = NULL,
      by = c("location", "forecast_date")
    )
  ),
  tar_target(
    name = total_admissions_all_time,
    command = plot_total_admissions(
      eval_hosp_data,
      first_forecast_date = min(eval_config$forecast_date_ww),
      last_forecast_date = max(eval_config$forecast_date_ww)
    )
  ),
  tar_target(
    name = crps_t_cfa_models_all_time,
    command = plot_score_t(
      crps_cfa_models_retro,
      metric = "crps",
      model_z_order = models_to_plot
    )
  ),
  tar_target(
    name = save_fig_crps_t_cfa_models_all_time,
    command = save_fig_supp(
      crps_t_cfa_models_all_time,
      base_width = 8,
      base_height = 4
    )
  ),
  tar_target(
    name = fig_rcrps_t_cfa_models_retro,
    command = plot_rel_score_t(
      crps_cfa_models_retro,
      target_models = "cfa-wwrenewal(retro)",
      baseline_model = "cfa-hosponlyrenewal(retro)",
      metric_to_compare = "crps"
    ) +
      ggplot2::theme(legend.position = "none")
  ),
  tar_target(
    name = save_fig_rcrps_t_cfa_models_retro,
    command = save_fig_supp(
      fig_rcrps_t_cfa_models_retro,
      base_width = 8,
      base_height = 4
    )
  ),
  tar_target(
    name = decomposed_crps_t_cfa_models_all_time,
    command = crps_cfa_models_retro |>
      scoringutils::summarise_scores(
        by = c("forecast_date", "model")
      ) |>
      plot_score_decomposed_bars(
        x = "forecast_date",
        width = 5
      )
  ),
  tar_target(
    name = save_decomposed_crps_t_cfa_models_all_time,
    command = save_fig_supp(
      decomposed_crps_t_cfa_models_all_time,
      base_width = 7,
      base_height = 5
    ),
    format = "file"
  ),
  tar_target(
    name = decomposed_crps_loc_cfa_models_all_time,
    command = crps_cfa_models_retro |>
      scoringutils::summarise_scores(
        by = c("location", "model")
      ) |>
      dplyr::arrange(.data$model, .data$crps) |>
      dplyr::mutate(
        location = factor(
          .data$location,
          levels = unique(.data$location),
          ordered = TRUE
        )
      ) |>
      plot_score_decomposed_bars(
        x = "location",
        width = 0.5
      )
  ),
  tar_target(
    name = save_decomposed_crps_loc_cfa_models_all_time,
    command = save_fig_supp(
      decomposed_crps_loc_cfa_models_all_time,
      base_width = 10,
      base_height = 5
    ),
    format = "file"
  ),
  tar_target(
    name = rel_crps_dist_by_horizon,
    command = plot_rel_score_dists_by_horizon(
      scores = crps_cfa_models_retro,
      target_models = "cfa-wwrenewal(retro)",
      baseline_model = "cfa-hosponlyrenewal(retro)",
      metric_to_compare = "crps"
    )
  ),
  tar_target(
    name = calibration_input_cfa_all_time,
    command = submitted_fcsts_cfa_retro |>
      ## "calibration" in a different sense here,
      ## i.e. training period
      dplyr::filter(
        .data$period != "calibration",
        !is.na(.data$observed),
        !is.na(.data$predicted)
      ) |>
      dplyr::select(
        -tidyselect::any_of(c(
          "calib_data",
          "pop",
          "name",
          "horizon_days",
          "period"
        ))
      ) |>
      dplyr::rename(model = "model_type")
  ),
  tar_target(
    name = qq_plot_retro_all_time,
    command = forecast_qq_plot(
      calibration_input_cfa_all_time
    )
  ),
  tar_target(
    name = save_qq_plot_retro_all_time,
    command = save_fig_supp(qq_plot_retro_all_time),
    format = "file"
  ),
  tar_target(
    name = coverage_plot_retro_all_time,
    command = forecast_interval_coverage_plot(
      calibration_input_cfa_all_time,
      ranges = c(30, 60, 90),
      time_period = "retro_all_time"
    )
  ),
  tar_target(
    name = save_coverage_plot_retro_all_time,
    command = save_fig_supp(
      coverage_plot_retro_all_time,
      base_width = 10,
      base_height = 6
    ),
    format = "file"
  ),
  tar_target(
    name = figure_rel_performance_all_time,
    command = compose_rel_performance_fig(
      rel_score_heatmap = rel_crps_heatmap_cfa_models,
      rel_score_dist = rel_crps_distribution_overall_cfa_models,
      abs_score_by_time = fig_rcrps_t_cfa_models_retro,
      total_admissions = total_admissions_all_time,
      scores_by_time = decomposed_crps_t_cfa_models_all_time,
      scores_by_location = decomposed_crps_loc_cfa_models_all_time
    )
  ),
  tar_target(
    name = save_figure_rel_performance_all_time,
    command = save_fig_main(
      figure_rel_performance_all_time,
      base_width = 10,
      base_height = 12
    )
  )
)

additional_figure_targets <- list(
  tar_target(
    name = plot_heatmap_hub_wis_retro,
    command = heatmap_scores_by_loc_date(
      scores = hub_scores,
      metric = "wis",
      models_to_plot = c(
        "cfa-wwrenewal(retro)",
        "cfa-hosponlyrenewal(retro)"
      )
    )
  ),
  tar_target(
    name = save_heatmap_hub_wis_retro,
    command = save_fig_supp(
      plot_heatmap_hub_wis_retro,
      base_width = 10,
      base_height = 5
    ),
    format = "file"
  ),
  tar_target(
    name = plot_heatmap_crps_retro,
    command = heatmap_scores_by_loc_date(
      scores = crps_cfa_models_retro,
      metric = "crps",
      models_to_plot = c(
        "cfa-wwrenewal(retro)",
        "cfa-hosponlyrenewal(retro)"
      )
    )
  ),
  tar_target(
    name = save_heatmap_crps_retro,
    command = save_fig_supp(
      plot_heatmap_crps_retro,
      base_width = 10,
      base_height = 5
    ),
    format = "file"
  ),
  tar_target(
    name = plot_heatmap_wis_real_time,
    command = heatmap_scores_by_loc_date(
      scores = hub_scores_real_time,
      metric = "wis",
      models_to_plot = c(
        "cfa-wwrenewal(real-time)",
        "cfa-hosponlyrenewal(real-time*)"
      )
    )
  ),
  tar_target(
    name = save_heatmap_wis_real_time,
    command = save_fig_supp(
      plot_heatmap_wis_real_time,
      base_width = 10,
      base_height = 5
    ),
    format = "file"
  ),
  tar_target(
    name = fig_bias_t_cfa_retro,
    command = plot_bias_t(
      crps_cfa_models_retro
    )
  ),
  tar_target(
    name = save_fig_bias_t_cfa_retro,
    command = save_fig_supp(
      fig_bias_t_cfa_retro,
      base_width = 10,
      base_height = 5
    ),
    format = "file"
  ),
  tar_target(
    name = fig_bias_t_hub,
    command = plot_bias_t(
      hub_scores
    )
  ),
  tar_target(
    name = save_fig_bias_t_hub,
    command = save_fig_supp(
      fig_bias_t_hub,
      base_width = 10,
      base_height = 5
    ),
    format = "file"
  ),
  tar_target(
    name = fig_crps_by_horizon,
    command = plot_score_by_horizon_t(
      crps_cfa_models_retro,
      score_type = "crps"
    )
  ),
  tar_target(
    name = save_fig_crps_by_horizon,
    command = save_fig_supp(
      fig_crps_by_horizon,
      base_height = 8,
      base_width = 4
    ),
    format = "file"
  ),
  tar_target(
    name = fig_wis_by_horizon_hub,
    command = plot_score_by_horizon_t(
      hub_scores,
      score_type = "wis"
    )
  ),
  tar_target(
    name = save_fig_wis_by_horizon_hub,
    command = save_fig_supp(
      fig_wis_by_horizon_hub,
      base_height = 8,
      base_width = 4
    ),
    format = "file"
  ),
  tar_target(
    name = avg_crps_by_horizon,
    command = scoringutils::summarise_scores(
      crps_cfa_models_retro,
      by = c("model", "horizon")
    )
  ),
  tar_target(
    name = avg_wis_by_horizon_all_time,
    command = scoringutils::summarise_scores(
      hub_scores,
      by = c("model", "horizon")
    )
  ),
  tar_target(
    name = avg_wis_by_horizon_real_time,
    command = scoringutils::summarise_scores(
      hub_scores_real_time,
      by = c("model", "horizon")
    )
  ),
  tar_target(
    name = comp_stats_all_time,
    command = get_stats_improved_forecasts(
      scores = crps_cfa_models_retro,
      target_model = "cfa-wwrenewal(retro)",
      baseline_model = "cfa-hosponlyrenewal(retro)",
      metric_to_compare = "crps",
      threshold = 1.1
    )
  ),
  tar_target(
    name = comp_stats_real_time,
    command = get_stats_improved_forecasts(
      scores = wis_cfa_models_real_time,
      target_model = "cfa-wwrenewal(real-time)",
      baseline_model = "cfa-hosponlyrenewal(real-time*)",
      metric_to_compare = "wis",
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
  tar_map(
    list(loc = c("OH", "IL")),
    tar_target(
      name = plot_ww_feb,
      command = plot_ribbon_ww_quantiles(
        ww_quants_feb_OH_IL,
        loc_to_plot = loc,
        max_n_site_labs_to_show = 12,
        date_to_plot = "2024-02-12"
      )
    ),
    tar_target(
      name = save_plot_ww_feb,
      command = save_fig_supp(
        plot_ww_feb,
        base_width = 7,
        base_height = 7
      ),
      format = "file"
    )
  ),
  tar_map(
    tibble::tibble(
      by = list(
        c("forecast_date", "location"),
        "forecast_date",
        "location"
      ),
      label = c(NA, "forecast_date", "location"),
      by_suffix = c("forecast_date_location", "forecast_date", "location")
    ),
    names = "by_suffix",
    tar_target(
      name = plot_score_scatter_real_time,
      command = plot_score_scatter(
        wis_cfa_models_real_time,
        metric = "wis",
        model_x = "cfa-hosponlyrenewal(real-time*)",
        model_y = "cfa-wwrenewal(real-time)",
        by = by,
        label = label,
        shape = 21,
        size = 3,
        color = "black",
        fill = "darkblue",
        alpha = 0.5
      )
    ),
    tar_target(
      name = save_score_scatter_real_time,
      command = save_fig_supp(
        plot_score_scatter_real_time,
        base_width = 5,
        base_height = 5
      ),
      format = "file"
    )
  ),
  tar_map(
    tibble::tibble(
      by = list(
        c("forecast_date", "location"),
        "forecast_date",
        "location"
      ),
      label = c(NA, "forecast_date", "location"),
      by_suffix = c("forecast_date_location", "forecast_date", "location")
    ),
    names = "by_suffix",
    tar_target(
      name = plot_score_scatter_retro,
      command = plot_score_scatter(
        crps_cfa_models_retro,
        metric = "crps",
        model_x = "cfa-hosponlyrenewal(retro)",
        model_y = "cfa-wwrenewal(retro)",
        by = by,
        label = label,
        shape = 21,
        size = 3,
        color = "black",
        fill = "darkblue",
        alpha = 0.5
      )
    ),
    tar_target(
      name = save_score_scatter_retro,
      command = save_fig_supp(
        plot_score_scatter_retro,
        base_width = 5,
        base_height = 5
      ),
      format = "file"
    )
  )
)

reported_quantities_targets <- list(
  tar_target(
    name = n_scored_dates_real_time,
    command = dplyr::n_distinct(scored_fcst_dates_real_time)
  ),
  tar_target(
      name = n_date_locs_to_compare_real_time,
      command = dplyr::n_distinct(date_locs_to_compare_real_time)
  ),
  tar_target(
      name = n_manual_exclude_ww_real_time,
      command = dplyr::n_distinct(date_locs_manual_exclude_ww_real_time)
  ),
  tar_target(
      name = n_manual_exclude_both_real_time,
      command = dplyr::n_distinct(date_locs_manual_exclude_both_real_time)
  ),
  tar_target(
      name = n_absent_ww_real_time,
      command = dplyr::n_distinct(date_locs_absent_ww_real_time)
  ),
  tar_target(
      name = n_insufficient_ww_real_time,
      command = dplyr::n_distinct(date_locs_insufficient_ww_real_time)
  ),
  tar_target(
      name = n_date_locs_to_compare_retro,
      command = dplyr::n_distinct(
                           date_locs_to_compare_retro)
  ),
  tar_target(
    name = n_scored_dates_all_time,
    command = dplyr::n_distinct(scored_forecast_dates)
  ),
  tar_target(
    name = n_scored_hub_models_non_cfa,
    command = dplyr::n_distinct(non_cfa_hub_models_to_score)
  )
)


list(
  configuration_targets,
  data_targets,
  collated_output_targets,
  real_time_rel_targets,
  hub_comparison_targets,
  trend_analysis_targets,
  composite_figure_targets,
  additional_figure_targets,
  reported_quantities_targets
)
