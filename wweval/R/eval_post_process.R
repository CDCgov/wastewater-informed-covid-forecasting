#' Post Process Wastewater Model for Evaluation
#'
#' @param config_index Index of eval_config to evaluate
#' @param eval_config_path Path to eval_config (created with `write_eval_config`)
#' @param params_path Path to params.toml
#'
#' @return NULL
#' @export
#'
eval_post_process_ww <- function(config_index,
                                 eval_config_path,
                                 params_path) {
  eval_config <- yaml::read_yaml(eval_config_path)
  output_dir <- eval_config$output_dir
  raw_output_dir <- eval_config$raw_output_dir

  save_object <- function(object_name, output_file_suffix) {
    saveRDS(
      object = get(object_name),
      file = file.path(raw_output_dir, paste0(object_name, output_file_suffix))
    )
  }
  load_object <- function(object_name, output_file_suffix) {
    readRDS(file.path(raw_output_dir, paste0(object_name, output_file_suffix)))
  }

  wwinference::create_dir(output_dir)
  wwinference::create_dir(raw_output_dir)


  params <- wwinference::get_params(params_path)
  location <- eval_config$location_ww[config_index]
  forecast_date <- eval_config$forecast_date_ww[config_index]
  scenario <- eval_config$scenario[config_index]

  output_file_suffix <- paste("", location, format(as.Date(forecast_date), "%Y.%m.%d"), scenario,
    sep = "_"
  ) |> paste0(".rds")



  input_hosp_data <- load_object("input_hosp_data", output_file_suffix)
  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)
  eval_hosp_data <- load_object("eval_hosp_data", output_file_suffix)
  input_ww_data <- load_object("input_ww_data", output_file_suffix)
  eval_ww_data <- load_object("eval_ww_data", output_file_suffix)
  ww_fit_obj_wwinference <- load_object("ww_fit_obj", output_file_suffix)
  ww_fit_obj <- ww_fit_obj_wwinference$fit$result

  ww_raw_draws <- ww_fit_obj$draws()
  save_object("ww_raw_draws", output_file_suffix)
  ww_diagnostics <- ww_fit_obj$sampler_diagnostics(format = "df")
  save_object("ww_diagnostics", output_file_suffix)
  ww_diagnostic_summary <- ww_fit_obj$diagnostic_summary()
  save_object("ww_diagnostic_summary", output_file_suffix)
  errors <- ww_fit_obj$error
  save_object("errors", output_file_suffix)
  metadata <- ww_fit_obj$metadata()
  raw_flags <- get_diagnostic_flags(
    ww_fit_obj,
    metadata$num_chains,
    metadata$iter_sampling
  )
  save_object("raw_flags", output_file_suffix)

  # Make the data look like it did in wweval-------------------------------
  input_hosp_data_wweval <- input_hosp_data |>
    dplyr:::rename(
      "daily_hosp_admits" = "count",
      "pop" = "total_pop"
    )
  input_ww_data_wweval <- input_ww_data |>
    dplyr::mutate(
      "ww" = exp(.data$log_genome_copies_per_ml),
      "lod_sewage" = exp(.data$log_lod)
    ) |>
    dplyr::rename(
      "ww_pop" = "site_pop",
      "below_LOD" = "below_lod"
    )

  eval_ww_data_wweval <- eval_ww_data |>
    dplyr::mutate(
      "ww" = exp(.data$log_genome_copies_per_ml),
      "lod_sewage" = exp(.data$log_lod)
    ) |>
    dplyr::rename(
      "ww_pop" = "site_pop",
      "below_LOD" = "below_lod"
    )


  # Get table of wastewater data flags
  ww_data_flags <- get_ww_data_flags(
    input_ww_data_wweval,
    forecast_date
  )
  # save the flags alongside the input wastewater data and admissions data
  save_table(
    data_to_save = ww_data_flags,
    type_of_output = "ww_data_flags",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  save_table(
    data_to_save = input_ww_data_wweval,
    type_of_output = "input_ww_data",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  save_table(
    data_to_save = input_hosp_data_wweval,
    type_of_output = "input_hosp_data",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )

  # Save errors
  save_table(
    data_to_save = errors,
    type_of_output = "errors",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  # Get evaluation data from hospital admissions and wastewater
  # Join draws and flags with data and metadata
  flags <- raw_flags |> dplyr::mutate(
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  # Save flags
  save_table(
    data_to_save = flags,
    type_of_output = "flags",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  # Plots of overlaid exponential growth rates in ww vs hosp
  plot_growth_rates <- get_growth_rate_plot(input_hosp_data_wweval,
    input_ww_data_wweval,
    location,
    forecast_date,
    rate = "weekly"
  )

  ggsave(plot_growth_rates, filename = file.path(
    output_dir, scenario,
    forecast_date, "ww", location,
    "plot_growth_rates.png"
  ))


  hosp_draws <- {
    if (!is.null(ww_fit_obj_wwinference$error)) {
      NULL
    } else {
      # Call a function that uses wwinference::get_draws(), joins the
      # evaluation data to it, and renames so everything looks the same
      # as is expected by downstream wweval functions.
      get_model_draws_w_data(
        fit_obj_wwinference = ww_fit_obj_wwinference,
        model_output = "hosp",
        model_type = "ww",
        forecast_date = forecast_date,
        scenario = scenario,
        location = location,
        eval_data = eval_hosp_data
      )
    }
  }
  save_object("hosp_draws", output_file_suffix)

  ww_draws <- {
    if (!is.null(ww_fit_obj_wwinference$error)) {
      NULL
    } else {
      get_model_draws_w_data(
        fit_obj_wwinference = ww_fit_obj_wwinference,
        model_output = "ww",
        model_type = "ww",
        forecast_date = forecast_date,
        scenario = scenario,
        location = location,
        eval_data = eval_ww_data
      )
    }
  }
  save_object("ww_draws", output_file_suffix)

  full_hosp_quantiles <- {
    if (is.null(hosp_draws)) {
      NULL
    } else {
      get_state_level_quantiles(
        draws = hosp_draws
      )
    }
  }
  save_object("full_hosp_quantiles", output_file_suffix)



  full_ww_quantiles <- {
    if (is.null(ww_draws)) {
      NULL
    } else {
      get_state_level_ww_quantiles(
        ww_draws = ww_draws
      )
    }
  }
  save_object("full_ww_quantiles", output_file_suffix)

  hosp_quantiles <- {
    if (is.null(full_hosp_quantiles)) {
      NULL
    } else {
      full_hosp_quantiles |>
        dplyr::filter(period != "calibration")
    }
  }
  save_object("hosp_quantiles", output_file_suffix)

  ww_quantiles <- {
    if (is.null(full_ww_quantiles)) {
      NULL
    } else {
      full_ww_quantiles |>
        dplyr::filter(period != "calibration")
    }
  }
  save_object("ww_quantiles", output_file_suffix)
  # Save forecasted quantiles locally as well as via
  # targets caching just for backup
  save_table(
    data_to_save = full_hosp_quantiles,
    type_of_output = "hosp_quantiles",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )

  save_table(
    data_to_save = full_ww_quantiles,
    type_of_output = "ww_quantiles",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )

  ### Plot the draw comparison-------------------------------------
  plot_hosp_draws <- {
    if (is.null(hosp_draws)) {
      NULL
    } else {
      get_plot_hosp_data_comparison(
        hosp_draws,
        location,
        model_type = "ww"
      )
    }
  }

  ggsave(plot_hosp_draws, filename = file.path(
    output_dir, scenario,
    forecast_date, "ww", location,
    "plot_hosp_draws.png"
  ))

  plot_hosp_t <- make_fig2_hosp_t(
    hosp_quantiles = full_hosp_quantiles,
    loc_to_plot = location,
    date_to_plot = forecast_date
  ) +
    ggtitle(glue::glue("{location} on {forecast_date}")) +
    theme_bw()

  ggsave(plot_hosp_t, filename = file.path(
    output_dir, scenario,
    forecast_date, "ww", location,
    "plot_hosp_t.png"
  ))

  save_object("plot_hosp_draws", output_file_suffix)

  plot_ww_draws <- {
    if (is.null(ww_draws)) {
      NULL
    } else {
      get_plot_ww_data_comparison(
        ww_draws,
        location,
        model_type = "ww"
      )
    }
  }

  ggsave(plot_ww_draws, filename = file.path(
    output_dir, scenario,
    forecast_date, "ww",
    location, "plot_ww_draws.png"
  ))

  plot_ww_t <- make_fig2_ct(
    full_ww_quantiles,
    loc_to_plot = location,
    date_to_plot = forecast_date,
    max_n_site_labs_to_show = length(unique(full_ww_quantiles$lab_site_index))
  ) +
    facet_wrap(~site_lab_name) +
    ggtitle(glue::glue("{location} on {forecast_date}")) +
    theme_bw()

  ggsave(plot_ww_t, filename = file.path(
    output_dir, scenario,
    forecast_date, "ww",
    location, "plot_ww_t.png"
  ))

  save_object("plot_ww_draws", output_file_suffix)

  ## Score hospital admissions forecasts----------------------------------
  hosp_scores <- get_full_scores(hosp_draws, scenario)
  save_object("hosp_scores", output_file_suffix)
  save_table(
    data_to_save = hosp_scores,
    type_of_output = "scores",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  hosp_scores_quantiles <- get_scores_from_quantiles(hosp_quantiles, scenario)
  save_object("hosp_scores_quantiles", output_file_suffix)
  save_table(
    data_to_save = hosp_scores_quantiles,
    type_of_output = "scores_quantiles",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "ww",
    location = location
  )
  # Get a subset of samples for plotting
  # Get a subset of quantiles for plotting
}
#' Post Process Hospitalizations Model for Evaluation
#'
#' @param config_index Index of eval_config to evaluate
#' @param eval_config_path Path to eval_config (created with `write_eval_config`)
#' @param params_path Path to params.toml
#'
#' @return NULL
#' @export
#'
eval_post_process_hosp <- function(config_index,
                                   eval_config_path,
                                   params_path) {
  eval_config <- yaml::read_yaml(eval_config_path)
  output_dir <- eval_config$output_dir
  raw_output_dir <- eval_config$raw_output_dir

  save_object <- function(object_name, output_file_suffix) {
    saveRDS(
      object = get(object_name),
      file = file.path(raw_output_dir, paste0(object_name, output_file_suffix))
    )
  }
  load_object <- function(object_name, output_file_suffix) {
    readRDS(file.path(raw_output_dir, paste0(object_name, output_file_suffix)))
  }


  wwinference::create_dir(output_dir)
  wwinference::create_dir(raw_output_dir)


  params <- wwinference::get_params(params_path)
  location <- eval_config$location_hosp[config_index]
  forecast_date <- eval_config$forecast_date_hosp[config_index]
  scenario <- "no_wastewater"

  output_file_suffix <- paste("", location, format(as.Date(forecast_date), "%Y.%m.%d"), scenario,
    sep = "_"
  ) |> paste0(".rds")

  input_hosp_data <- load_object("input_hosp_data", output_file_suffix)
  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)
  eval_hosp_data <- load_object("eval_hosp_data", output_file_suffix)
  hosp_fit_obj_wwinference <- load_object(
    "hosp_fit_obj",
    output_file_suffix
  )
  hosp_fit_obj <- hosp_fit_obj_wwinference$fit$result

  hosp_raw_draws <- hosp_fit_obj$draws()
  save_object("hosp_raw_draws", output_file_suffix)
  hosp_diagnostics <- hosp_fit_obj$sampler_diagnostics(format = "df")
  save_object("hosp_diagnostics", output_file_suffix)
  hosp_diagnostic_summary <- hosp_fit_obj$diagnostic_summary()
  save_object("hosp_diagnostic_summary", output_file_suffix)
  errors <- hosp_fit_obj$error
  save_object("errors", output_file_suffix)
  metadata <- hosp_fit_obj$metadata()
  raw_flags <- get_diagnostic_flags(
    hosp_fit_obj,
    metadata$num_chains,
    metadata$iter_sampling
  )
  save_object("raw_flags", output_file_suffix)
  # Save errors
  save_table(
    data_to_save = errors,
    type_of_output = "errors",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )


  # Get evaluation data from hospital admissions and wastewater
  # Join draws with flags + data and metadata
  flags <- raw_flags |> dplyr::mutate(
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )
  # Save flags
  save_table(
    data_to_save = flags,
    type_of_output = "flags",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )
  hosp_model_hosp_draws <- get_model_draws_w_data(
    fit_obj_wwinference = hosp_fit_obj_wwinference,
    model_output = "hosp",
    model_type = "hosp",
    forecast_date = forecast_date,
    scenario = scenario,
    location = location,
    eval_data = eval_hosp_data
  )
  save_object("hosp_model_hosp_draws", output_file_suffix)
  full_hosp_model_quantiles <- get_state_level_quantiles(
    draws = hosp_model_hosp_draws
  )
  save_object("full_hosp_model_quantiles", output_file_suffix)

  hosp_model_quantiles <- full_hosp_model_quantiles |>
    dplyr::filter(period != "calibration")
  save_object("hosp_model_quantiles", output_file_suffix)

  # Save forecasted quantiles locally as well as via
  # targets caching just for backup
  save_table(
    data_to_save = full_hosp_model_quantiles,
    type_of_output = "quantiles",
    output_dir = output_dir,
    scenario = "no_wastewater",
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )

  ### Plot the draw comparison-------------------------------------
  plot_hosp_draws_hosp_model <- get_plot_hosp_data_comparison(
    hosp_model_hosp_draws,
    location,
    model_type = "hosp"
  )
  save_object("plot_hosp_draws_hosp_model", output_file_suffix)
  ggsave(plot_hosp_draws_hosp_model,
    filename = file.path(
      output_dir, scenario,
      forecast_date, "hosp", location,
      "plot_hosp_draws.png"
    ),
    bg = "white"
  )

  plot_hosp_t <- make_fig2_hosp_t(
    hosp_quantiles = full_hosp_model_quantiles,
    loc_to_plot = location,
    date_to_plot = forecast_date
  ) +
    ggtitle(glue::glue("{location} on {forecast_date}"))

  ggsave(plot_hosp_t, filename = file.path(
    output_dir, scenario,
    forecast_date, "hosp", location,
    "plot_hosp_t.png"
  ))

  ## Score the hospital admissions only model-------------------------
  hosp_scores <- get_full_scores(hosp_model_hosp_draws,
    scenario = "no_wastewater"
  )
  save_object("hosp_scores", output_file_suffix)
  save_table(
    data_to_save = hosp_scores,
    type_of_output = "scores",
    output_dir = output_dir,
    scenario = "no_wastewater",
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )
  hosp_scores_quantiles <- get_scores_from_quantiles(hosp_model_quantiles,
    scenario = "no_wastewater"
  )
  save_object("hosp_scores_quantiles", output_file_suffix)
  save_table(
    data_to_save = hosp_scores_quantiles,
    type_of_output = "scores_quantiles",
    output_dir = output_dir,
    scenario = "no_wastewater",
    forecast_date = forecast_date,
    model_type = "hosp",
    location = location
  )
}
