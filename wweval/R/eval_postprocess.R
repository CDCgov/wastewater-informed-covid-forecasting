#' Postprocess a successful eval fitting job.
#'
#' Helper function called within the [eval_postprocess()] wrapper function and
#' only if the fit was successful. This handles the bulk of the postprocessing,
#' but there are some postprocessing tasks we wish to perform regardless of
#' whether the model fit was successful.
#'
#' @param wwinference_fit_obj Fit object to process, as the output of
#' [wwinference::wwinference()].
#' @param stan_fit_obj Corresponding stan fit object.
#' @param model Which model the object represents. One of `"ww"` and `"hosp"`.
#' @param location location for the forecasting problem.
#' @param forecast_date "as-of" date for the forecasting problem.
#' @param scenario data availability scenario for the forecasting problem.
#' @param output_dir Directory in which to save processed output.
#' @param raw_output_dir Directory in which to archive objects from the environment
#' as serialized `.rds` files.
#' @param input_hosp_data_wweval Input hospital admissions data in legacy wweval format.
#' @param input_ww_data_wweval Input wastewater data in legacy `wweval` format.
#' @param eval_hosp_data Evaluation hospital admissions data in newer `wwinference` format.
#' @param eval_ww_data Evaluation wastewater admissions data in newer `wwinference` format.
#' @return Nothing, saving results to disk as a side effect.
#' @export
process_successful_fit <- function(wwinference_fit_obj,
                                   stan_fit_obj,
                                   model,
                                   location,
                                   forecast_date,
                                   scenario,
                                   output_dir,
                                   raw_output_dir,
                                   input_hosp_data_wweval,
                                   input_ww_data_wweval,
                                   eval_hosp_data,
                                   eval_ww_data) {
  checkmate::assert_names(model, subset.of = c("ww", "hosp"))
  ww_model <- model == "ww"
  raw_output_suffix <- get_raw_output_suffix(
    location,
    forecast_date,
    scenario
  )

  save_object <- purrr::partial(
    to_rds_with_suffix,
    output_dir = raw_output_dir,
    save_suffix = raw_output_suffix
  )

  fig_save_dir <- fs::path(
    output_dir,
    scenario,
    forecast_date,
    model,
    location
  )
  fs::dir_create(fig_save_dir)

  ggsave_plot <- function(plot,
                          save_basename = NULL,
                          ext = "png") {
    if (is.null(save_basename)) {
      save_basename <- deparse(substitute(plot))
    }
    ggsave(
      filename = fs::path(fig_save_dir,
        save_basename,
        ext = ext
      ),
      plot = plot
    )
  }

  message("Saving raw draws and diagnostics...")
  raw_draws <- stan_fit_obj$draws()
  save_object(raw_draws, save_basename = glue::glue("{model}_raw_draws"))
  diagnostic_df <- stan_fit_obj$sampler_diagnostics(format = "df")
  save_object(diagnostic_df, save_basename = glue::glue("{model}_diagnostics"))
  diagnostic_summary <- stan_fit_obj$diagnostic_summary()
  save_object(diagnostic_summary, save_basename = glue::glue("{model}_diagnostic_summary"))

  metadata <- stan_fit_obj$metadata()
  raw_flags <- get_diagnostic_flags(
    stan_fit_obj,
    metadata$num_chains,
    metadata$iter_sampling
  )
  save_object(raw_flags)

  flags <- raw_flags |> dplyr::mutate(
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = model,
    location = location
  )
  # Save flags
  save_table(
    data_to_save = flags,
    type_of_output = "flags",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = model,
    location = location
  )

  message("Done with raw draws and diagnostics.")

  message("Plotting histograms of marginal posteriors...")
  hist_table_params <- c(
    "inf_feedback" = "infection_feedback",
    "sigma_rt" = "sigma_rt",
    "eta_sd" = "eta_sd"
  )

  plot_and_save_param <- function(param_name, save_name) {
    param_draws <- raw_draws |>
      tidybayes::spread_draws(!!str2lang(param_name)) |>
      dplyr::mutate(
        draw = .data$`.draw`
      ) |>
      dplyr::select(!!param_name, "draw")
    param_plot <- param_draws |>
      ggplot(aes(x = .data[[param_name]])) +
      geom_histogram()
    ggsave_plot(param_plot,
      save_basename = save_name
    )
    save_table(
      data_to_save = param_draws,
      type_of_output = save_name,
      output_dir = output_dir,
      scenario = scenario,
      forecast_date = forecast_date,
      model_type = model,
      location = location
    )
  }

  purrr::iwalk(hist_table_params, plot_and_save_param)
  message("Done plotting histograms.")

  if (ww_model) {
    ## Plots of overlaid exponential growth rates in ww vs hosp
    plot_growth_rates <- get_growth_rate_plot(
      input_hosp_data_wweval,
      input_ww_data_wweval,
      location,
      forecast_date,
      rate = "weekly"
    )
    ggsave_plot(plot_growth_rates)
  }

  hosp_draws <- NULL
  ww_draws <- NULL

  if (is.null(wwinference_fit_obj$error)) {
    ## Call a function that uses wwinference::get_draws(), joins the
    ## evaluation data to it, and renames so everything looks the same
    ## as is expected by downstream wweval functions.
    hosp_draws <- get_model_draws_w_data(
      fit_obj_wwinference = wwinference_fit_obj,
      model_output = "hosp",
      model_type = model,
      forecast_date = forecast_date,
      scenario = scenario,
      location = location,
      eval_data = eval_hosp_data
    )
    if (ww_model) {
      ww_draws <- get_model_draws_w_data(
        fit_obj_wwinference = wwinference_fit_obj,
        model_output = "ww",
        model_type = model,
        forecast_date = forecast_date,
        scenario = scenario,
        location = location,
        eval_data = eval_ww_data
      )
    }
  }
  save_object(ww_draws)
  save_object(hosp_draws)

  full_hosp_quantiles <- {
    if (is.null(hosp_draws)) {
      NULL
    } else {
      get_state_level_quantiles(
        draws = hosp_draws
      )
    }
  }
  hosp_quant_savename <- ifelse(
    ww_model,
    "full_hosp_quantiles",
    "full_hosp_model_quantiles"
  )
  save_object(full_hosp_quantiles)


  full_ww_quantiles <- {
    if (is.null(ww_draws)) {
      NULL
    } else {
      get_state_level_ww_quantiles(
        ww_draws = ww_draws
      )
    }
  }
  save_object(full_ww_quantiles)

  hosp_quantiles <- {
    if (is.null(full_hosp_quantiles)) {
      NULL
    } else {
      full_hosp_quantiles |>
        dplyr::filter(period != "calibration")
    }
  }
  save_object(hosp_quantiles)

  ww_quantiles <- {
    if (is.null(full_ww_quantiles)) {
      NULL
    } else {
      full_ww_quantiles |>
        dplyr::filter(period != "calibration")
    }
  }
  save_object(ww_quantiles)
  # Save forecasted quantiles locally as well as via
  # targets caching just for backup
  save_table(
    data_to_save = full_hosp_quantiles,
    type_of_output = ifelse(
      ww_model,
      "hosp_quantiles",
      "quantiles"
    ),
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = model,
    location = location
  )

  save_table(
    data_to_save = full_ww_quantiles,
    type_of_output = "ww_quantiles",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = model,
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
        model_type = model
      )
    }
  }

  hosp_draw_plot_savename <- ifelse(
    ww_model,
    "plot_hosp_draws",
    "plot_hosp_draws_hosp_model"
  )

  ggsave_plot(plot_hosp_draws,
    save_basename = hosp_draw_plot_savename
  )
  save_object(plot_hosp_draws)

  plot_hosp_t <- make_fig2_hosp_t(
    hosp_quantiles = full_hosp_quantiles,
    loc_to_plot = location,
    date_to_plot = forecast_date
  ) +
    ggtitle(glue::glue("{location} on {forecast_date}")) +
    theme_bw()

  ggsave_plot(plot_hosp_t)


  # Plots of R(t)
  draws <- wwinference::get_draws(wwinference_fit_obj, what = "all")

  plot_state_rt <- wwinference::get_plot_global_rt(
    draws$global_rt,
    forecast_date
  )
  ggsave_plot(plot_state_rt)

  if (ww_model) {
    plot_subpop_rt <- wwinference::get_plot_subpop_rt(
      draws$subpop_rt,
      forecast_date
    )
    ggsave_plot(plot_subpop_rt)
  }

  if (!is.null(ww_draws)) {
    plot_ww_draws <- get_plot_ww_data_comparison(
      ww_draws,
      location,
      model_type = model
    )

    ggsave_plot(plot_ww_draws)
  } else {
    plot_ww_draws <- NULL
  }
  save_object(plot_ww_draws)

  if (!is.null(full_ww_quantiles)) {
    plot_ww_t <- make_fig2_ct(
      full_ww_quantiles,
      loc_to_plot = location,
      date_to_plot = forecast_date,
      max_n_site_labs_to_show = length(unique(full_ww_quantiles$lab_site_index))
    ) +
      facet_wrap(~site_lab_name, scales = "free_y") +
      ggtitle(glue::glue("{location} on {forecast_date}")) +
      theme_bw()

    ggsave_plot(plot_ww_t)
  }


  ## Score hospital admissions forecasts----------------------------------
  hosp_scores <- get_full_scores(hosp_draws, scenario)
  save_object(hosp_scores)
  save_table(
    data_to_save = hosp_scores,
    type_of_output = "scores",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = model,
    location = location
  )
  hosp_scores_quantiles <- get_scores_from_quantiles(hosp_quantiles, scenario)
  save_object(hosp_scores_quantiles)
  save_table(
    data_to_save = hosp_scores_quantiles,
    type_of_output = "scores_quantiles",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = model,
    location = location
  )
}

#' Postprocess model for evaluation.
#'
#' Raw output is saved to disk in the `raw_output_dir` specified in the eval config
#' as serialized `.rds` files. Processed output is saved in a structured directory
#' format in the `output_dir` specified in the eval config. See the [save_table()],
#' [to_rds_with_suffix()], and [get_raw_output_suffix()] functions for more details.
#'
#' The bulk of the postprocessing for successful model fits is handled
#' by the [postprocess_successful_fit()] function, which is called within this
#' function provided the target model fit was indeed successful, but there are
#' some postprocessing tasks we wish to perform regardless of
#' whether the model fit was successful.
#'
#' @param config_index Index of eval_config to evaluate
#' @param eval_config_path Path to eval_config (created with `write_eval_config`)
#' @param params_path Path to params.toml
#' @param model model to postprocess. One of `"hosp"` and `"ww"`.
#' @param max_eval_data_days Maximum number of days of data to pull
#' when creating evaluation dataset. Default 365.
#' @return NULL, saving plots and tables to disk as side effects.
#' @export
eval_postprocess <- function(config_index,
                             eval_config_path,
                             params_path,
                             model,
                             max_eval_data_days = 365) {
  checkmate::assert_names(model, subset.of = c("ww", "hosp"))
  ww_model <- model == "ww"
  eval_config <- yaml::read_yaml(eval_config_path)
  output_dir <- eval_config$output_dir
  raw_output_dir <- eval_config$raw_output_dir
  params <- wwinference::get_params(params_path)
  location <- eval_config$location_ww[config_index]
  forecast_date <- eval_config$forecast_date_ww[config_index]
  scenario <- ifelse(ww_model,
    eval_config$scenario[config_index],
    "no_wastewater"
  )
  hosp_data_dir <- eval_config$hosp_data_dir
  ww_data_dir <- eval_config$ww_data_dir
  eval_date <- eval_config$eval_date
  fit_obj_name <- glue::glue("{model}_fit_obj")

  ww_data_mapping <- eval_config$ww_data_mapping

  raw_output_suffix <- get_raw_output_suffix(
    location,
    forecast_date,
    scenario
  )

  save_object <- purrr::partial(
    to_rds_with_suffix,
    output_dir = raw_output_dir,
    save_suffix = raw_output_suffix
  )

  load_object <- function(object_name) {
    return(readRDS(
      fs::path(raw_output_dir,
        glue::glue("{object_name}{raw_output_suffix}"),
        ext = "rds"
      )
    ))
  }

  wwinference::create_dir(output_dir)
  wwinference::create_dir(raw_output_dir)


  input_hosp_data <- load_object("input_hosp_data")
  last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)
  eval_hosp_data <- get_input_hosp_data(
    forecast_date_i = eval_date,
    location_i = location,
    hosp_data_dir = hosp_data_dir,
    calibration_time = max_eval_data_days
  ) |>
    dplyr::filter(.data$date >= !!min(input_hosp_data$date))
  save_object(eval_hosp_data)
  ## Format input hosp data in format the eval pipeline expects
  ## for backward compatibility
  input_hosp_data_wweval <- input_hosp_data |>
    dplyr::rename(
      daily_hosp_admits = "count",
      pop = "total_pop"
    )


  if (ww_model) {
    input_ww_data <- load_object("input_ww_data")

    eval_ww_data <- purrr::safely(get_input_ww_data)(
      forecast_date_i = eval_date,
      location_i = location,
      scenario_i = scenario,
      scenario_dir = scenario_dir,
      ww_data_dir = ww_data_dir,
      calibration_time = max_eval_data_days,
      last_hosp_data_date = eval_date,
      ww_data_mapping = ww_data_mapping
    )$result

    if (!is.null(eval_ww_data) && !is.null(input_ww_data)) {
      eval_ww_data <- eval_ww_data |>
        dplyr::filter(.data$date >= !!min(input_ww_data$date))
    }
    if (!is.null(input_ww_data)) {
      input_ww_data_wweval <- input_ww_data |>
        dplyr::mutate(
          ww = exp(.data$log_genome_copies_per_ml),
          lod_sewage = exp(.data$log_lod)
        ) |>
        dplyr::rename(
          ww_pop = "site_pop",
          below_LOD = "below_lod"
        )
      ww_data_flags <- get_ww_data_flags(
        input_ww_data_wweval,
        forecast_date
      )
    } else {
      input_ww_data_wweval <- NULL
      ww_data_flags <- tibble::tibble()
    }

    save_object(input_ww_data)
    save_object(eval_ww_data)
  } else {
    input_ww_data <- NULL
    eval_ww_data <- NULL
    input_ww_data_wweval <- NULL
  }

  fit_obj_wwinference <- load_object(fit_obj_name)
  fit_obj <- fit_obj_wwinference$fit$result


  # If model fit failed, dont produce any of the below outputs
  if (!is.null(fit_obj$error)) {
    errors <- fit_obj$error
    save_object(errors)
    save_table(
      data_to_save = errors,
      type_of_output = "errors",
      output_dir = output_dir,
      scenario = scenario,
      forecast_date = forecast_date,
      model_type = model,
      location = location
    )
  } else {
    process_successful_fit(
      wwinference_fit_obj = fit_obj_wwinference,
      stan_fit_obj = fit_obj,
      model = model,
      location = location,
      forecast_date = forecast_date,
      scenario = scenario,
      output_dir = output_dir,
      raw_output_dir = raw_output_dir,
      input_hosp_data_wweval = input_hosp_data_wweval,
      input_ww_data_wweval = input_ww_data_wweval,
      eval_hosp_data = eval_hosp_data,
      eval_ww_data = eval_ww_data
    )
  }

  ## Things to save even if model run fails
  ## save the flags alongside the input wastewater data and admissions data
  save_table(
    data_to_save = input_hosp_data_wweval,
    type_of_output = "input_hosp_data",
    output_dir = output_dir,
    scenario = scenario,
    forecast_date = forecast_date,
    model_type = model,
    location = location
  )

  if (ww_model) {
    save_table(
      data_to_save = ww_data_flags,
      type_of_output = "ww_data_flags",
      output_dir = output_dir,
      scenario = scenario,
      forecast_date = forecast_date,
      model_type = model,
      location = location
    )
    save_table(
      data_to_save = input_ww_data_wweval,
      type_of_output = "input_ww_data",
      output_dir = output_dir,
      scenario = scenario,
      forecast_date = forecast_date,
      model_type = model,
      location = location
    )
  }
}
