# Utils for simulation-based calibration analysis------------------------------
#' Get full parameter distribution
#' @description
#' This function takes in a fitted wastewater model and returns a long-format
#' tibble of the parameter values for all (or a subset of) draws.
#'
#' @param mcmc_fit CmdStanMCMC object
#' @param draw vector, which draws from the MCMC object do we want in the result?
#'  If NULL, returns all. This can be a very large data frame!
#'
#' @return a tibble containing the posterior distribution of each
#' parameter and additional information,
#' namely its name, its temporal context, and its spatial context.
#' @export
get_param_samples_long_df <- function(mcmc_fit,
                                      draw = NULL) {
  if (!"CmdStanMCMC" %in% class(mcmc_fit)) {
    stop("mcmc_fit must be of class `CmdStanMCMC`")
  }

  # Get information about the model's parameters
  param_df <- get_model_param_df(mcmc_fit) %>%
    filter(!generated_quantity)

  # @TODO allow passing in only one variable at a time or a subset
  #      use custom param_df to do so

  all_draws <- posterior::subset_draws(
    # list is supposedly more RAM-friendly format
    mcmc_fit$draws(format = "draws_list"),
    draw = draw
  )

  # Get in long format
  full_param_df <- lapply(param_df$param_name, function(p_name) {
    .get_1_param(
      param_df %>% filter(param_name == p_name),
      all_draws
    )
  })

  return(do.call(bind_rows, full_param_df))
}

#' Internal helper for get_param_samples_long_df
#' @description
#' This function makes a "long" data frame for a single parameter. This may be a
#' scalar-, vector-, or matrix-valued parameter.
#'
#'
#' @param param_info single-row data frame (or tibble), originating from get_model_param_df().
#' @param all_draws MCMC samples as a draws_array object
#'
#' @return data frame with parameter name, values, and (as applicable) information
#' about the spatiotemporal aspects of the parameter
#' @keywords internal
.get_1_param <- function(param_info, all_draws) {
  out_df <- NULL

  this_param <- param_info %>% pull(param_name)
  t_var <- param_info %>% pull(time_varying)
  ls_var <- param_info %>% pull(lab_or_site_varying)

  # spread according to param type: scalar, vector, matrix
  if (xor(t_var, ls_var)) {
    spread_on <- .get_param_spreader_strings(param_info)
    out_df <- all_draws %>%
      tidybayes::spread_draws((!!sym(this_param))[!!sym(spread_on)])
  } else if (t_var && ls_var) {
    spread_on <- .get_param_spreader_strings(param_info)
    out_df <- all_draws %>%
      tidybayes::spread_draws(
        (!!sym(this_param))[
          !!sym(spread_on["place"]),
          !!sym(spread_on["time"])
        ],
        regex = TRUE
      )
  } else {
    out_df <- all_draws %>%
      tidybayes::spread_draws(!!sym(this_param))
  }

  out_df <- out_df %>%
    mutate(
      name = this_param,
    ) %>%
    rename(
      value = !!sym(this_param),
      draw = `.draw`
    ) %>%
    select(-`.chain`, -`.iteration`)

  return(out_df)
}

#' Internal helper for .get_1_param
#' @description
#' Provides information required to spread() vector- or matrix-valued parameters.
#'
#' @param param_info single-row data frame (or tibble), originating from get_model_param_df().
#'
#' @return named vector,
#' @keywords internal
.get_param_spreader_strings <- function(param_info) {
  time_names <- c("day", "week", "day_of_week")
  names(time_names) <- rep("time", length(time_names))

  place_names <- c("ww_lab_site", "ww_lab", "ww_site")
  names(place_names) <- rep("place", length(place_names))

  place_bool <- param_info %>%
    select(c("per_lab_site", "per_lab", "per_site")) %>%
    as.logical()
  time_bool <- param_info %>%
    select(c("daily", "weekly", "cyclic")) %>%
    as.logical()
  # At the moment all cyclic parameters are also daily
  if (time_bool[3]) {
    time_bool <- c(FALSE, FALSE, TRUE)
  }
  res <- c(
    place_names[place_bool],
    time_names[time_bool]
  )
  return(res)
}


#' Get full parameter distribution
#' @description
#' This function takes in vectors of parameters, grouped by their indexing,
#' and returns a tidy dataframe with all the draws of all the parameters and
#' the corresponding indices
#'
#'
#' @param all_draws draws from the stan object
#' @param static_params character vector of parameters that are static and
#' don't need any indexing
#' @param weekly_params character vector of parameters that are indexed by week
#' @param daily_ww_params character vector of parameters that are indexed by
#'  day (t) and ww lab site
#' @param ww_lab_site_params character vector of parameters that are indexed by
#'  unique combo of the wastewater lab and site
#' @param day_of_week_params character vector of parameters that are indexed by
#'  day of week
#'
#' @return a dataframe containing the posterior distribution of each parameter
#' With indices corresponding to the parameters that have them
#' @export
#'
#' @examples
get_full_param_distrib <- function(all_draws,
                                   static_params,
                                   vector_params,
                                   matrix_params = NA) {
  # Get the static parameters
  for (i in seq_along(static_params)) {
    this_param <- static_params[i]
    this_param_df <- all_draws %>%
      spread_draws(!!sym(this_param)) %>%
      mutate(
        name = this_param,
        value = !!sym(this_param),
        index_rows = NA,
        index_cols = NA
      ) %>%
      select(
        name, value, `.draw`,
        index_rows, index_cols
      )
    if (i == 1) {
      param_df <- this_param_df
    } else {
      param_df <- rbind(param_df, this_param_df)
    }
  }

  # Get the vector parameters
  for (i in seq_along(vector_params)) {
    this_vector_param <- vector_params[i]
    this_vector_param_df <- all_draws %>%
      spread_draws((!!sym(this_vector_param))[!!sym("index_cols")],
        regex = TRUE
      ) %>%
      mutate(
        name = this_vector_param,
        value = !!sym(this_vector_param),
        index_rows = NA
      ) %>%
      select(
        name, value, `.draw`,
        index_rows, index_cols
      )
    if (i == 1) {
      vector_param_df <- this_vector_param_df
    } else {
      vector_param_df <- rbind(vector_param_df, this_vector_param_df)
    }
  }


  # Get the daily ww parameters
  if (!is.na(matrix_params)) {
    for (i in seq_along(matrix_params)) {
      this_matrix_param <- matrix_params[i]
      this_matrix_param_df <- all_draws %>%
        spread_draws(
          (!!sym(this_matrix_param))[
            !!sym("index_rows"),
            !!sym("index_cols")
          ],
          regex = TRUE
        ) %>%
        mutate(
          name = this_matrix_param,
          value = !!sym(this_matrix_param),
        ) %>%
        select(
          name, value, `.draw`,
          index_rows, index_cols
        )
      if (i == 1) {
        matrix_df <- this_matrix_param_df
      } else {
        matrix_df <- rbind(matrix_df, this_matrix_param_df)
      }
    }
  }



  if (!is.na(matrix_params)) {
    full_param_df <- rbind(
      param_df, vector_param_df, matrix_df
    )
  } else {
    full_param_df <- rbind(
      param_df, vector_param_df
    )
  }



  # Rename draw for ease of calling it
  full_param_df <- full_param_df %>%
    rename(draw = `.draw`) %>%
    left_join(
      full_param_df %>%
        group_by(index_rows, index_cols, name) %>%
        summarise(median = quantile(value, 0.5, na.rm = TRUE)),
      by = c("index_rows", "index_cols", "name")
    )

  return(full_param_df)
}

## Diagnostics------------------------------------------------------------------

#' Calculate low case count diagnostic flag
#'
#' The diagnostic flag is TRUE if either of the _last_ two weeks of the dataset
#' have fewer than an aggregate 10 hospital admissions per week.
#' This aggregation excludes the
#' count from confirmed outliers, which have been set to NA in the data.
#'
#' Adapted from https://github.com/cdcent/cfa-nnh-pipelines/blob/7f1c89d2eecfed89cc3f1be1771d93540ffd6eb4/NHSN/Rt/R/disease_process.R #nolint
#'
#' This function assumes that the `calib_data` input dataset has been
#' "completed": that any implicit missingness has been made explicit.
#'
#' @param calib_data A dataframe. It has `date` column of type `Date`,
#'   `daily_hosp_admits` column of type numeric and `period`
#'   column of type character.
#'
#' @return Returns TRUE if less than 10 hospital admissions in either of
#' the last two weeks
#' @export
get_low_case_count_diagnostic <- function(calib_data) {
  # Sorts dates in ascending order and get the last
  max_dates <- tail(sort(unique(
    calib_data$date[calib_data$period == "calibration"]
  )), 14)

  case_data_flag <- calib_data %>%
    ungroup() %>%
    # Pull out the last two weeks. This assumes that the df is "complete", i.e.
    # that all the nulls are made explicit.
    filter(date %in% max_dates) %>%
    # Are the dates in the first 7 days or the second 7 days?
    mutate(week_rank = dense_rank(date) <= 7) %>%
    group_by(week_rank) %>%
    # Threshold of 10 cases in both of the last two weeks. The `na.rm = TRUE`
    # ensures that missing data is treated as 0 for purposes of this diagnostic.
    summarize(
      n_weekly_cases_below_thresh = sum(daily_hosp_admits, na.rm = TRUE) < 10
    ) %>%
    ungroup() %>%
    pull(n_weekly_cases_below_thresh) %>%
    # If either is TRUE, diagnostic is TRUE
    any()

  return(case_data_flag)
}

#' Get some basic flags on the wastewater data, for internal use and for
#' reporting out in our metadata
#'
#' @param calib_data The training data object susbetted to before the forecast
#' date
#' @param delay_thres The maximum number of days of delay between the last
#' wastewater data point and the forecat date, before we would flag a state as
#' having insufficient wastewater data to inform a forecast. Default is 21
#' @param n_dps_thres The threshold number of data points within a single site
#' within a state before we would flag the state as having insufficient
#' wastewater data to inform a forecast. Default is 5
#' @param prop_below_lod_thres The threshold proportion of wastewater data
#' points that can be below the LOD. If greater than this proportion of points
#' are below the LOD, we flag the state as having insufficient wastewater data.
#' Default is 0.5
#' @param sd_thres The minimum standard deviation between wastewater data points
#' within a site. This is intended to catch when a site reports all the same
#' values. Default is 0.1
#' @return a dataframe with 5 True or False flags corresponding to wastewater
#' data presence and quality
#' @export
#'
#' @examples
get_wastewater_diagnostic <- function(calib_data,
                                      delay_thres = 21,
                                      n_dps_thres = 5,
                                      prop_below_lod_thres = 0.5,
                                      sd_thres = 0.1,
                                      mean_log_ww_value_thres = -4) {
  # Flag if all wastewater data is missing
  no_wastewater_data_flag <- all(is.na(calib_data$ww))

  if (isFALSE(no_wastewater_data_flag)) {
    # Flag if there isn't any wastewater data from the past 3 weeks
    delayed_wastewater_data_flag <- all(is.na(
      calib_data %>%
        filter(date >= forecast_date - days(delay_thres)) %>%
        pull(ww)
    ))

    # Flag if there are less than 5 data points
    insufficient_ww_data_flag <- (
      calib_data %>%
        filter(!is.na(ww)) %>%
        group_by(lab_wwtp_unique_id) %>%
        summarize(n_dps = n()) %>%
        pull(n_dps) %>%
        max()
    ) <= n_dps_thres

    # Flag if most datapoints are below the LOD
    most_below_lod <- (
      calib_data %>%
        filter(!is.na(ww)) %>%
        summarize(
          n_ww = n(),
          n_below_LOD = sum(below_LOD)
        ) %>%
        mutate(prop_below_LOD = n_below_LOD / n_ww) %>%
        pull(prop_below_LOD)
    ) > prop_below_lod_thres

    # Flag if most data points within a lab-site are flat
    most_flat <- (
      calib_data %>%
        group_by(lab_wwtp_unique_id) %>%
        summarise(sd_ww = sd(ww, na.rm = TRUE)) %>%
        filter(!is.na(sd_ww)) %>%
        ungroup() %>%
        summarise(mean_sd = mean(sd_ww)) %>%
        pull(mean_sd)
    ) <= sd_thres

    wastewater_values_too_low <- (
      calib_data %>%
        summarise(mean_log_ww = mean(log(ww), na.rm = TRUE)) %>%
        pull(mean_log_ww)
    ) <= mean_log_ww_value_thres
  } else {
    delayed_wastewater_data_flag <- NA
    insufficient_ww_data_flag <- NA
    most_below_lod <- NA
    most_flat <- NA
    wastewater_values_too_low <- NA
  }

  wastewater_diagnostic <- tibble(
    no_wastewaster_data_flag = no_wastewater_data_flag,
    delayed_wastewater_data_flag = delayed_wastewater_data_flag,
    insufficient_ww_data_flag = insufficient_ww_data_flag,
    most_below_lod = most_below_lod,
    wastewater_values_too_low = wastewater_values_too_low,
    most_flat = most_flat
  )
  return(wastewater_diagnostic)
}

#' Get a joint dataframe of data and model fit diagnostics
#'
#' @description
#' This returns a 10 row dataframe corresponding to 6 flags of the hospital
#' admissions or wastewater data and 4 rows corresponding to stan fit
#' diagnostics.
#'
#'
#' @param stan_fit_object The output of fit$sample() in cmdstanR
#' @param train_data The datarframe containing hospital admissions and
#' wastewater data for callibration of the model
#' @param location The location the model is being run on
#' @param model_type The type of model e.g. site-level observation error
#' @param forecast_date The date of the forecast
#' @param ...
#'
#' @return Returns a dataframe containing diagnostic summaries
#' @export
#'
#' @examples
get_diagnostics <- function(stan_fit_object, train_data, location,
                            model_type, forecast_date, ...) {
  calib_data <- train_data %>% filter(date <= forecast_date)
  low_case_count_diagnostic <- get_low_case_count_diagnostic(calib_data)

  if (!(model_type %in% c(
    "hospital admissions only", "state-level aggregated wastewater"
  ))) {
    wastewater_diagnostic <- get_wastewater_diagnostic(calib_data)
  } else {
    wastewater_diagnostic <- tibble(
      no_wastewaster_data_flag = TRUE,
      delayed_wastewater_data_flag = NA,
      insufficient_ww_data_flag = NA,
      most_below_lod = NA,
      wastewater_values_too_low = NA,
      most_flat = NA
    )
  }


  data_df <- tibble(
    diagnostic = c(
      "low_case_count_flag",
      "no_wastewater_data_flag",
      "delayed_wastewater_data_flag",
      "insufficient_ww_data_flag",
      "most_below_lod",
      "wastewater_values_too_low",
      "most_flat"
    ),
    value = c(
      low_case_count_diagnostic,
      wastewater_diagnostic$no_wastewaster_data_flag,
      wastewater_diagnostic$delayed_wastewater_data_flag,
      wastewater_diagnostic$insufficient_ww_data_flag,
      wastewater_diagnostic$most_below_lod,
      wastewater_diagnostic$wastewater_values_too_low,
      wastewater_diagnostic$most_flat
    ),
    "location" = location,
    "forecast_date" = forecast_date,
    "model_type" = model_type
  )

  diagnostics <- stan_fit_object$diagnostic_summary(quiet = TRUE)
  diagnostic_df <- tibble(
    diagnostic = c(
      "n_divergent",
      "n_max_treedepth",
      "mean_ebfmi",
      "p_high_rhat"
    ),
    value = c(
      sum(diagnostics$num_divergent),
      sum(diagnostics$num_max_treedepth),
      mean(diagnostics$ebfmi),
      as.numeric(mean(stan_fit_object$summary()[, "rhat"]$rhat > 1.05, na.rm = TRUE))
    ),
    "location" = location,
    "forecast_date" = forecast_date,
    "model_type" = model_type
  )

  return(rbind(data_df, diagnostic_df))
}

#' Load in and combine all the diagnostic tables
#'
#' @param df_of_filepaths dataframe containing filepath indicating where
#' model outputs are saved
#'
#' @return a combined list of all the diagnostics across locations and model
#' runs
#' @export
#'
#' @examples
read_diagnostics_df <- function(df_of_filepaths) {
  for (i in seq_len(nrow(df_of_filepaths))) {
    diagnostics_i <- read_csv(
      file.path(df_of_filepaths$diagnostics_file_path[i])
    )

    if (i == 1) {
      diagnostics_df <- diagnostics_i
    } else {
      diagnostics_df <- rbind(diagnostics_df, diagnostics_i)
    }
  }
  return(diagnostics_df)
}

#' Get the lists of states with different flags (both data and model)
#'
#' @param df long table containing diagnostic, value, location, forecast_date,
#' and model_type
#' @param ebmfi_tolerance tolerance for mean EBMFI, default of 0.2
#' @param divergences_tolerance tolerance for number of divergent transitions
#' assuming 2000 iterations, default 20
#' @param p_high_rhat_tolerance tolerance for probability of high p_hat,
#' default 0.05
#' @param n_max_tree_depth_tol tolerance for number of draws that
#' hit the maximum tree depth, default 20
#'
#' @return
#' @export
#'
#' @examples
get_summary_stats <- function(df,
                              ebmfi_tolerance = 0.2,
                              divergences_tolerance = 20,
                              p_high_rhat_tolerance = 0.05,
                              n_max_tree_depth_tol = 20) {
  states_w_no_ww_data <- df %>%
    filter(diagnostic == "no_wastewater_data_flag", value == 1) %>%
    pull(location)

  states_w_insufficient_ww_data <- df %>%
    filter(diagnostic == "insufficient_ww_data_flag", value == 1) %>%
    pull(location)

  states_w_delayed_ww_data <- df %>%
    filter(diagnostic == "delayed_wastewater_data_flag", value == 1) %>%
    pull(location)

  # states with any data below LOD, or flat
  states_below_lod_or_flat <- df %>%
    filter(
      diagnostic %in% c("most_below_lod", "most_flat"),
      value == 1
    ) %>%
    pull(location)

  # states with any wastewater values too low
  states_low_ww <- df %>%
    filter(
      diagnostic %in% c("wastewater_values_too_low"),
      value == 1
    ) %>%
    pull(location)

  states_to_flag_for_hub <- unique(c(
    states_w_insufficient_ww_data,
    states_w_delayed_ww_data
  ))

  states_w_low_hosp_admissions <- df %>%
    filter(diagnostic == "low_case_count_flag", value == 1) %>%
    pull(location)

  states_w_low_ebmfi <- df %>%
    filter(diagnostic == "mean_ebmfi", value <= ebmfi_tolerance) %>%
    pull(location)

  states_w_too_many_divergences <- df %>%
    filter(diagnostic == "n_divergent", value >= divergences_tolerance) %>%
    pull(location)

  states_w_high_rhat <- df %>%
    filter(diagnostic == "p_high_rhat", value >= p_high_rhat_tolerance) %>%
    pull(location)

  # states with high number of draws at maximum tree depth
  states_high_tree_depth <- df %>%
    filter(
      diagnostic == "n_max_treedepth",
      value >= n_max_tree_depth_tol
    ) %>%
    pull(location)

  stats <- list(
    states_w_no_ww_data = states_w_no_ww_data,
    states_w_insufficient_ww_data = states_w_insufficient_ww_data,
    states_w_delayed_ww_data = states_w_delayed_ww_data,
    states_below_lod_or_flat = states_below_lod_or_flat,
    states_low_ww = states_low_ww,
    states_to_flag_for_hub = states_to_flag_for_hub,
    states_w_low_hosp_admissions = states_w_low_hosp_admissions,
    states_w_low_ebmfi = states_w_low_ebmfi,
    states_w_too_many_divergences = states_w_too_many_divergences,
    states_w_high_rhat = states_w_high_rhat,
    states_high_tree_depth = states_high_tree_depth
  )
  return(stats)
}


## Extract and format the generated quantites-----------------------------------

#' @title Get generated quantities draws site level model
#' @description
#' This function takes the raw stan output from the site leve model and
#' arranges them in a tidy data format, with time and site_index as indexing
#' variables.
#'
#'
#' @param all_draws
#' @param train_data
#'
#' @return A long dataframe with model draws from pred_hosp, pred_ww, R(t) and
#' p_hosp(t), where all but pred_ww have one value per time point and draw. Bc
#' we need all the column names to be the same, site_index is left empty for those.
#' The resulting dataframe has the following columns:
#' name, site_index, t, value, draw
#'
#' @export
#'
#' @examples
get_gen_quants_draws <- function(all_draws,
                                 model_type) {
  hosp_draws <- all_draws %>%
    spread_draws(pred_hosp[t]) %>%
    # sample_draws(ndraws = n_draws) %>%
    rename(value = pred_hosp) %>%
    mutate(
      draw = `.draw`,
      name = "pred_hosp",
      lab_site_index = NA
    ) %>%
    select(name, lab_site_index, t, value, draw)

  ww_draws <- all_draws %>%
    spread_draws(pred_ww[lab_site_index, t]) %>%
    # sample_draws(ndraws = n_draws) %>%
    rename(value = pred_ww) %>%
    mutate(
      draw = `.draw`,
      name = "pred_ww",
      value = exp(value)
    ) %>%
    select(name, lab_site_index, t, value, draw)

  exp_state_ww_draws <- all_draws %>%
    spread_draws(exp_state_ww_conc[t]) %>%
    # sample_draws(ndraws = n_draws) %>%
    rename(value = exp_state_ww_conc) %>%
    mutate(
      draw = `.draw`,
      name = "exp_state_ww_conc",
      lab_site_index = NA
    ) %>%
    select(name, lab_site_index, t, value, draw)

  rt_draws <- all_draws %>%
    spread_draws(rt[t]) %>%
    #  sample_draws(ndraws = n_draws) %>%
    rename(value = rt) %>%
    mutate(
      draw = `.draw`,
      name = "R(t)",
      lab_site_index = NA
    ) %>%
    select(name, lab_site_index, t, value, draw)

  p_hosp_draws <- all_draws %>%
    spread_draws(p_hosp[t]) %>%
    #  sample_draws(ndraws = n_draws) %>%
    rename(value = p_hosp) %>%
    mutate(
      draw = `.draw`,
      name = "p_hosp",
      lab_site_index = NA
    ) %>%
    select(name, lab_site_index, t, value, draw)

  model_draws <- rbind(
    hosp_draws, ww_draws,
    exp_state_ww_draws, rt_draws,
    p_hosp_draws
  )
  model_draws <- model_draws %>%
    mutate(site_index = NA)

  if (model_type == "site-level infection dynamics") {
    site_level_rt <- all_draws %>%
      spread_draws(r_site_t[site_index, t]) %>%
      rename(value = r_site_t) %>%
      mutate(
        draw = `.draw`,
        name = "R_site_t",
        lab_site_index = NA
      ) %>%
      select(colnames(model_draws))

    model_draws <- rbind(model_draws, site_level_rt)
  }

  return(model_draws)
}

#' @title Get generated quantities draws
#' @description
#' This function takes the raw stan output from the aggregated model and
#' arranges them in a tidy data format, with time as an indexing variable.
#' @param all_draws
#' @param train_data
#'
#' @return A long dataframe with model draws from pred_hosp, pred_ww, R(t) and
#' p_hosp(t), where all have one value per time point and draw.
#' The resulting dataframe has the following columns:
#' name, t, value, draw
#' @export
#'
#' @examples
get_generated_quantities_draws <- function(all_draws, n_draws = 100) {
  # Dataframes with ndraws (long format)
  hosp_draws <- all_draws %>%
    spread_draws(pred_hosp[t]) %>%
    # sample_draws(ndraws = n_draws) %>%
    rename(value = pred_hosp) %>%
    mutate(
      draw = `.draw`,
      name = "pred_hosp"
    ) %>%
    select(name, t, value, draw)

  ww_draws <- all_draws %>%
    spread_draws(pred_conc[t]) %>%
    # sample_draws(ndraws = n_draws) %>%
    rename(value = pred_conc) %>%
    mutate(
      draw = `.draw`,
      name = "pred_ww",
      value = exp(value)
    ) %>%
    select(name, t, value, draw)

  exp_state_ww_draws <- all_draws %>%
    spread_draws(exp_state_ww_conc[t]) %>%
    # sample_draws(ndraws = n_draws) %>%
    rename(value = exp_state_ww_conc) %>%
    mutate(
      draw = `.draw`,
      name = "exp_state_ww_conc",
    ) %>%
    select(name, t, value, draw)

  rt_draws <- all_draws %>%
    spread_draws(rt[t]) %>%
    #  sample_draws(ndraws = n_draws) %>%
    rename(value = rt) %>%
    mutate(
      draw = `.draw`,
      name = "R(t)"
    ) %>%
    select(name, t, value, draw)

  p_hosp_draws <- all_draws %>%
    spread_draws(p_hosp[t]) %>%
    #  sample_draws(ndraws = n_draws) %>%
    rename(value = p_hosp) %>%
    mutate(
      draw = `.draw`,
      name = "p_hosp"
    ) %>%
    select(name, t, value, draw)

  model_draws <- rbind(
    hosp_draws, ww_draws, rt_draws,
    exp_state_ww_draws, p_hosp_draws
  )

  return(model_draws)
}


## Extract draws from posterior of static parameters----------------------------
#' @title Get parameters
#' @description
#' Returns a list of the static parameter in this model. Can be edited as needed
#'
#' @return
#' @export
#'
#' @examples
get_pars <- function() {
  pars <- c(
    "phi_h", "sigma_ww", "G", "eta_sd", "p_hosp_sd", "i0", "initial_growth",
    "viral_peak", "t_peak", "dur_shed", "autoreg_rt"
  )
  return(pars)
}

#' @title Get raw parameter draws
#' @description
#' This concatenates all the tidy draws dataframes of each of the parameters
#'
#' @param stan_output_draws
#' @param n_draws
#' @param ...
#'
#' @return a long tidy dataframe with columns names:
#' name, t, value, draw corresponding to the value of the draw of the posterior
#' of that parameter
#' @export
#'
#' @examples
get_raw_param_draws <- function(stan_output_draws, n_draws = 500, ...) {
  # Single parmeter psoterior draws with sumamry stats
  phi_h <- stan_output_draws %>%
    spread_draws(phi_h) %>%
    sample_draws(ndraws = n_draws) %>%
    mutate(draw = `.draw`) %>%
    mutate(
      name = "phi_h",
      t = NA
    ) %>%
    rename(value = phi_h) %>%
    select(name, t, value, draw)

  eta_sd <- stan_output_draws %>%
    spread_draws(eta_sd) %>%
    sample_draws(ndraws = n_draws) %>%
    mutate(draw = `.draw`) %>%
    mutate(
      name = "eta_sd",
      t = NA
    ) %>%
    rename(value = eta_sd) %>%
    select(name, t, value, draw)

  log10_g <- stan_output_draws %>%
    spread_draws(log10_g) %>%
    sample_draws(ndraws = n_draws) %>%
    mutate(draw = `.draw`) %>%
    mutate(
      name = "log10_g",
      t = NA
    ) %>%
    rename(value = log10_g) %>%
    select(name, t, value, draw)


  i0 <- stan_output_draws %>%
    spread_draws(i0) %>%
    sample_draws(ndraws = n_draws) %>%
    mutate(draw = `.draw`) %>%
    mutate(
      name = "i0",
      t = NA
    ) %>%
    rename(value = i0) %>%
    select(name, t, value, draw)

  p_hosp_sd <- stan_output_draws %>%
    spread_draws(p_hosp_w_sd) %>%
    sample_draws(ndraws = n_draws) %>%
    mutate(draw = `.draw`) %>%
    mutate(
      name = "p_hosp_sd",
      t = NA
    ) %>%
    rename(value = p_hosp_w_sd) %>%
    select(name, t, value, draw)

  initial_growth <- stan_output_draws %>%
    spread_draws(initial_growth) %>%
    sample_draws(ndraws = n_draws) %>%
    mutate(draw = `.draw`) %>%
    mutate(
      name = "initial_growth",
      t = NA
    ) %>%
    rename(value = initial_growth) %>%
    select(name, t, value, draw)

  infection_feedback <- stan_output_draws %>%
    spread_draws(infection_feedback) %>%
    sample_draws(ndraws = n_draws) %>%
    mutate(draw = `.draw`) %>%
    mutate(
      name = "infection_feedback",
      t = NA
    ) %>%
    rename(value = infection_feedback) %>%
    select(name, t, value, draw)

  autoreg_rt <- stan_output_draws %>%
    spread_draws(autoreg_rt) %>%
    sample_draws(ndraws = n_draws) %>%
    mutate(draw = `.draw`) %>%
    mutate(
      name = "autoreg_rt",
      t = NA
    ) %>%
    rename(value = autoreg_rt) %>%
    select(name, t, value, draw)


  posterior_params <- rbind(
    phi_h, eta_sd, log10_g, i0,
    infection_feedback,
    autoreg_rt, initial_growth, p_hosp_sd
  )

  return(posterior_params)
}

#' @title Get posterior parameter draws and summary stats
#' @description
#' Reads in model_draws, parameters, and training data and gets summary
#' stats on the model parameters, alongside the metadata associated with the model
#' run (comes from train data)
#'
#' @param stan_output_draws the wide format output directly from stan
#' @param train_data the data that went into stan (for a single forecast date,
#' location, model type, and data scenario)
#'
#' @return dataframe with the following parameters: phi_h, phi_w, eta_sd, G,
#' p_hosp, i0, initial_growth, dur_shed, viral_peak, t_peak, with n_draws from
#' the posterior alongside median and 50% and 95% credible intervals
#' @export
#'
#' @examples
get_parameter_draws <- function(model_draws, pars, train_data) {
  metadata_df <- get_metadata(train_data)
  forecast_date <- metadata_df$forecast_date
  location <- metadata_df$location
  last_hosp_data_date <- metadata_df$last_hosp_data_date
  pop <- metadata_df$pop
  hosp_reporting_delay <- metadata_df$hosp_reporting_delay
  include_ww <- metadata_df$include_ww

  posterior_params <- model_draws %>%
    filter(name %in% pars) %>%
    select(-t) %>%
    ungroup()

  posterior_params <- posterior_params %>%
    group_by(name) %>%
    mutate(
      median = quantile(value, 0.5, na.rm = TRUE),
      lb_50th = quantile(value, 0.25, na.rm = TRUE),
      ub_50th = quantile(value, 0.75, na.rm = TRUE),
      lb_95th = quantile(value, 0.025, na.rm = TRUE),
      ub_95th = quantile(value, 0.975, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      forecast_date = forecast_date,
      location = location,
      last_hosp_data_date = last_hosp_data_date,
      hosp_reporting_delay = hosp_reporting_delay,
      include_ww = include_ww
    )

  return(posterior_params)
}

# Prepare for submission ----------------------------------------------------

#' Get a consolidated dataframe with each location and its corresponding
#' file path for the necesary outputs.
#'
#' @param df_of_filepaths_inf_dyn the output filepaths resulting after running
#' the infection dynamics model targets, default is NA
#' @param df_of_filepaths_site_obs the output filepaths resulting after running
#' the site-level observation error model targets, default is NA
#' @param df_of_filepaths_hosp_only the output filepaths resulting after running
#' the hospital admissions only model targets, default is NA
#' @param df_of_filepaths_agg the output filepaths resulting after running the
#' national aggreated model target, default is NA
#' @param df_of_filepaths_varying_conc the output filepaths resulting after
#' running the time varying concentration model targets, default is NA
#' @param prod_model_type the model type we want to submit, default is
#' "site-level infection dynamics"
#' @param hosp_only_states the states that we want to manually
#' specify to use the hospital admissions only model instead of the wastewater
#' informed model
#'
#' @return a dataframe of length of the number of locations (will message if
#' not = 53) containing the model type and filepaths for the model outputs needed
#' for the hub submission
#' @export
#'
#' @examples
get_submission_filepath_df <- function(prod_model_type,
                                       hosp_only_states,
                                       df_of_filepaths_inf_dyn = NA,
                                       df_of_filepaths_site_obs = NA,
                                       df_of_filepaths_hosp_only = NA,
                                       df_of_filepaths_agg = NA,
                                       df_of_filepaths_varying_conc = NA) {
  if (prod_model_type == "site-level observation error") {
    stopifnot(
      "Filepaths for production model type not passed in" =
        !is.na(df_of_filepaths_site_obs)
    )
    df_of_filepaths <- rbind(
      df_of_filepaths_site_obs,
      df_of_filepaths_agg
    )
  } else if (prod_model_type == "site-level infection dynamics") {
    stopifnot(
      "Filepaths for production model type not passed in" =
        !is.na(df_of_filepaths_inf_dyn)
    )
    df_of_filepaths <- rbind(
      df_of_filepaths_inf_dyn,
      df_of_filepaths_agg
    )
  } else if (prod_model_type == "site-level time-varying concentration") {
    stopifnot(
      "Filepaths for production model type not passed in" =
        !is.na(df_of_filepaths_varying_conc)
    )
    df_of_filepaths <- rbind(
      df_of_filepaths_varying_conc,
      df_of_filepaths_agg
    )
  } else {
    message("Production model type not specified properly")
  }

  if (!is.null(hosp_only_states)) {
    # Replace the full file path of the wastewater model with the
    # hospital admissions only model
    inds_ho <- df_of_filepaths$location %in% c(hosp_only_states)

    df_of_filepaths$model_draws_file_path[inds_ho] <-
      df_of_filepaths_hosp_only$model_draws_file_path[inds_ho]
    df_of_filepaths$quantiles_file_path[inds_ho] <-
      df_of_filepaths_hosp_only$quantiles_file_path[inds_ho]
    df_of_filepaths$future_hosp_draws_file_path[inds_ho] <-
      df_of_filepaths_hosp_only$future_hosp_draws_file_path[inds_ho]
    df_of_filepaths$parameters_file_path[inds_ho] <-
      df_of_filepaths_hosp_only$parameters_file_path[inds_ho]
    df_of_filepaths$diagnostics_file_path[inds_ho] <-
      df_of_filepaths_hosp_only$diagnostics_file_path[inds_ho]
    # Replace model type
    df_of_filepaths$model_type[inds_ho] <- "hospital admissions only"
  }

  if (any(duplicated(df_of_filepaths$location))) {
    stop("Multiple model outputs for a single location")
  }

  if (length(df_of_filepaths$location) < 53) {
    message("Dataframe of filepaths only generating for a subset of locations")
  } else {
    message("All 53 states and territories in dataframe for hub submission")
  }
  return(df_of_filepaths)
}

#' Hardcoded figure filepath for figures per the read me
#'
#' @param output_file_path
#' @param forecast_date
#' @param date_run
#'
#' @return
#' @export
#'
#' @examples
get_figure_file_path <- function(
    output_file_path,
    forecast_date,
    date_run) {
  fp <- file.path(
    output_file_path, "figures",
    glue::glue("{forecast_date}-run-on-{date_run}")
  )
  return(fp)
}
#' Get the hardcoded file path for pdfs per the read me
#'
#' @param output_file_path
#' @param forecast_date
#' @param date_run
#'
#' @return
#' @export
#'
#' @examples
get_pdf_file_path <- function(output_file_path,
                              forecast_date,
                              date_run) {
  fp <- file.path(
    output_file_path, "cleaned",
    glue::glue("{forecast_date}-run-on-{date_run}")
  )
  return(fp)
}

#' Get the relative path to directory to save the forecasts within the repo
#'
#' @param forecast_date date of forecast
#'
#' @return
#' @export
#'
#' @examples
get_relative_forecast_dir <- function(forecast_date) {
  fp <- fs::path(
    "forecasts",
    glue::glue("{forecast_date}")
  )

  return(fp)
}

#' Get the hardcoded submission file path
#'
#' @param output_file_path
#' @param forecast_date
#' @param date_run
#' @param prod_run
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_submission_file_path <- function(output_file_path,
                                     forecast_date,
                                     date_run) {
  fp <- file.path(
    output_file_path, "cleaned",
    glue::glue("{forecast_date}-run-on-{date_run}")
  )

  return(fp)
}

#' Load in future hosp draws and combine into a single dataframe
#'
#' @param df_of_filepaths filepaths where the future hosp draws live
#' @param forecast_submission_filepath where we want the csv to be saved
#' @param submitting_model_name name of the model that we will set as the
#' folder name
#' @param write_files whether or not to write to the forecast submission
#' file path
#' @return df of hub submission for all locations
#' @export
#'
#' @examples
get_df_for_hub_submission <- function(df_of_filepaths,
                                      submitting_model_name,
                                      submission_file_path,
                                      repo_file_path,
                                      prod_run,
                                      write_files = TRUE,
                                      ...) {
  for (i in seq_len(nrow(df_of_filepaths))) {
    model_draws_i <- arrow::read_parquet(
      file = df_of_filepaths$future_hosp_draws_file_path[i]
    )

    quant_summary_i <- format_hosp_draws_hub_sub(model_draws_i)

    if (i == 1) {
      quant_summary_full <- quant_summary_i
    } else {
      quant_summary_full <- rbind(quant_summary_full, quant_summary_i)
    }
  }

  model_types <- quant_summary_full %>%
    pull(model_type) %>%
    unique()
  message("More than one model type being passed in")
  n_locs <- quant_summary_full %>%
    pull(location) %>%
    unique() %>%
    length()
  message("Number of locations in dataset passed in: ", n_locs)
  forecast_date <- quant_summary_full %>%
    pull(forecast_date) %>%
    unique()

  # remove model type
  quant_summary <- quant_summary_full %>%
    select(-model_type) %>%
    mutate(quantile = round(quantile, digits = 4))

  if (isTRUE(write_files)) {
    if (isTRUE(prod_run)) {
      full_file_path <- file.path(
        submission_file_path,
        "submitted_forecasts",
        submitting_model_name
      )
    } else {
      full_file_path <- file.path(
        submission_file_path,
        "test_forecasts",
        submitting_model_name
      )
    }

    create_dir(full_file_path)
    create_dir(repo_file_path)

    write_csv(quant_summary, file.path(
      full_file_path,
      glue::glue("{forecast_date}-{submitting_model_name}.csv")
    ))
    message("Writing forecast Hub files to csv")
    write.table(quant_summary_full,
      file =
        file.path(
          repo_file_path,
          glue::glue("{forecast_date}.tsv")
        ),
      row.names = FALSE
    )
    message("Writing forecast files to repo location")
  }

  return(quant_summary_full)
}



#' Prepare posterior draws of hospital admissions for the COVID-19 forecast hub
#'
#' @param hosp_draws dataframe containing the posterior draws from
#' all time points of the hospital admissions for a single model and location
#' @return a dataframe formatted for COVID-19 forecast hub submission
#' @export
#'
#' @examples
format_hosp_draws_hub_sub <- function(hosp_draws) {
  model_type <- hosp_draws %>%
    pull(model_type) %>%
    unique()
  forecast_date <- hosp_draws %>%
    pull(forecast_date) %>%
    unique()

  stopifnot("More than one model output passed in" = length(model_type) == 1)
  stopifnot("More than one forecast date passed in" = length(forecast_date) == 1)

  quant_summary <- hosp_draws %>%
    trajectories_to_quantiles(
      timepoint_cols = "date",
      id_cols = "location",
      value_col = "value"
    ) %>%
    mutate(
      location = loc_abbr_to_flusight_code(location),
      model_type = model_type,
      forecast_date = forecast_date
    ) %>%
    filter(date >= forecast_date + days(1)) %>%
    rename(
      target_end_date = date
    ) %>%
    mutate(days_ahead = as.numeric(target_end_date - forecast_date)) %>%
    mutate(
      target = glue::glue("{days_ahead} day ahead inc hosp"),
      type = "quantile"
    ) %>%
    rename(
      value = quantile_value,
      quantile = quantile_level
    ) %>%
    select(
      target, location, forecast_date, target_end_date,
      quantile, value, type, model_type
    )

  return(quant_summary)
}




#' @title Get all quantiles
#' @description
#' Takes in the model draws and summarizes over them to get quantiles that
#' correspond to the quantiles reported in the COVID-19 Forecast Hub that
#' are used for generating WIS scores.
#'
#' @param model_draws
#'
#' @return a long dataframe of all the quantiled values of the model variables
#' columns names include all the things we had in the draws_df + metadata:
#' name, t, value, draw, quantile,
#' include_ww, forecast_date, hosp_reporting_delay,
#' location, data_type, date, ww, daily_hosp_admits,
#' daily_hosp_admits_for_eval, pop, obs_data, period, damp_type
#' @export
#'
#' @examples
get_all_quantiles <- function(model_draws) {
  forecasts <- trajectories_to_quantiles(
    model_draws,
    timepoint_cols = "t",
    value_col = "value",
    id_cols = c("location", "name")
  ) %>%
    rename(
      quantile = quantile_level,
      value = quantile_value
    )

  # Get the truth data, but also all the other variables that we need to keep
  # track of in the grouping!
  obs_data <- model_draws %>%
    select(-draw, -value) %>%
    distinct()

  inf_and_forecasts_w_obs <- forecasts %>%
    as.data.frame() %>%
    left_join(obs_data, by = c("t", "location", "name")) %>%
    mutate(
      data_type = "quantile",
      draw = NA
    ) %>%
    ungroup() %>%
    select(c(colnames(model_draws), quantile))

  return(inf_and_forecasts_w_obs)
}

# Scores from quantiles -------------------------------------------------------
#' @title Get scores
#' @description
#' Reads in the quantiles and summarizes scores by the grouping variables specified
#'
#' @param quantiles
#' @param output_file_path
#' @param grouping_vars
#' @param write_files
#'
#' @return a dataframe of average scores over the grouping variables indicated
#' @export
#'
#' @examples
get_scores <- function(quantiles, output_file_path,
                       scores_file_path = file.path("summaries", "scores"),
                       grouping_vars = c(
                         "hosp_reporting_delay",
                         "location_name", "period",
                         "forecast_date", "name", "model"
                       ),
                       write_files = TRUE) {
  # Add model column
  if (str_contains(colnames(quantiles), "include_ww")) {
    quantiles <- quantiles %>% mutate(
      model = ifelse(include_ww == 1, "WW + hosp", "hosp only")
    )
  } else {
    # Assume if column isn't present, we are running the WW + hosp model!
    quantiles <- quantiles %>% mutate(
      model = "WW + hosp"
    )
  }

  log_quantiles <- quantiles %>% mutate(
    true_value = log(true_value),
    prediction = log(prediction)
  )

  log_wis_scores <- log_quantiles %>%
    scoringutils::score() %>%
    scoringutils::summarise_scores(
      by = c({{ grouping_vars }})
    ) %>%
    ungroup() %>%
    rename(
      log_interval_score = interval_score,
      log_dispersion = dispersion,
      log_underprediction = underprediction,
      log_overprediction = overprediction
    ) %>%
    select(-coverage_deviation, -bias, -ae_median)

  scores_summary <- quantiles %>%
    scoringutils::score() %>%
    scoringutils::add_coverage(
      ranges = c(50, 90),
      by = c({{ grouping_vars }})
    ) %>%
    scoringutils::summarize_scores(by = c({{ grouping_vars }})) %>%
    ungroup()

  baseline_score <- scores_summary %>%
    filter(model == "hosp only") %>%
    select(location_name, period, name, forecast_date, interval_score) %>%
    group_by(location_name, period, name, forecast_date) %>%
    mutate(baseline_score = interval_score) %>%
    ungroup() %>%
    select(-interval_score)

  scores_summary <- scores_summary %>%
    left_join(baseline_score) %>%
    ungroup() %>%
    mutate(relative_WIS = interval_score / baseline_score) %>%
    left_join(log_wis_scores, by = c({{ grouping_vars }}))

  if (isTRUE(write_files)) {
    create_dir(file.path(output_file_path, scores_file_path))

    arrow::write_parquet(
      x = scores_summary,
      sink = file.path(
        output_file_path, scores_file_path,
        "scores_summary.parquet"
      )
    )
  }

  return(scores_summary)
}

#' @title Summarize scores
#' @description
#' Takes the scores grouped by location, period, model, data type (name),
#' and hosp_reporting delay and averages over them. Used to get summaries to
#' compare across models and scenarios over time
#'
#'
#' @param scores
#'
#' @return wide dataframe of average scores across time
#' @export
#'
#' @examples
summarize_scores <- function(scores) {
  scores_sum <- scores %>%
    group_by(location_name, period, model, name, hosp_reporting_delay) %>%
    summarise(
      mean_coverage_90 = mean(coverage_90),
      mean_ae_median = mean(ae_median),
      mean_WIS = mean(interval_score),
      mean_relative_WIS = mean(relative_WIS),
      mean_log_WIS = mean(log_interval_score),
      mean_abs_coverage_deviation = mean(abs(coverage_deviation))
    ) %>%
    filter(name == "pred_hosp", period != "calibration")

  return(scores_sum)
}


#' Pivot the quantiles output for easy plotting
#'
#' @param quantiles  a long dataframe of quantiles
#'
#' @return a wide dataframe of quantiles with just the quantiles we want for plotting
#' @export
#'
#' @examples
pivot_data_for_plotting <- function(quantiles) {
  quantiles_wide <- quantiles %>%
    dplyr::mutate(quantile = round(quantile, 3)) %>%
    dplyr::filter(
      name %in% c("R(t)", "pred_hosp", "exp_state_ww_conc"),
      quantile %in% c(
        0.025, 0.25, 0.5,
        0.750, 0.975
      )
    ) %>%
    dplyr::select(date, location, name, value, quantile) %>%
    tidyr::pivot_wider(
      id_cols = c(location, name, date),
      names_from = quantile, values_from = value,
      names_prefix = "quantity_"
    ) %>%
    dplyr::left_join(
      quantiles %>%
        dplyr::select(
          date,
          location,
          daily_hosp_admits,
          forecast_date,
          daily_hosp_admits_for_eval,
          period,
          model_type
        ) %>%
        dplyr::distinct(),
      by = c("location", "date")
    )

  return(quantiles_wide)
}

#' Quantile the site level expected observed WW
#'
#' @param draws the tidy bayes output with stacked draws of all generated quantites
#'
#' @return a wide format dataframe with just the median, 50th, and 95th quantiles of
#'          the expected WW concentation
#' @export
#'
#' @examples
get_quantiles_on_site_level_ww <- function(draws) {
  ww_quantiles <- draws %>%
    filter(name == "pred_ww") %>%
    group_by(lab_site_index, date) %>%
    summarise(
      median_ww = quantile(log(value), 0.5, na.rm = TRUE),
      lb_25th = quantile(log(value), 0.25, na.rm = TRUE),
      ub_75th = quantile(log(value), 0.75, na.rm = TRUE),
      lb_025th = quantile(log(value), 0.025, na.rm = TRUE),
      ub_975th = quantile(log(value), 0.975, na.rm = TRUE)
    ) %>%
    left_join(
      draws %>%
        select(
          lab_site_index, date,
          site, lab, forecast_date,
          location, ww, model_type,
          below_LOD, flag_as_ww_outlier
        ) %>%
        distinct(),
      by = c("lab_site_index", "date")
    ) %>%
    mutate(
      log_conc = log(ww),
      site_lab = ifelse(
        !is.na(lab),
        paste0("Site: ", site, " Lab: ", lab),
        paste0("Site: ", site, " Lab: not specified")
      ),
      include_ww = 1
    ) %>%
    ungroup()

  return(ww_quantiles)
}

#' Save a single pdf of plots
#'
#' Saves pdf containing the concatenated figures
#'
#' @param list_of_plots the output from targets of a list of ggplot objects
#' @param pdf_file_path where we want to save the pdf within the output file path
#' @param type_of_output what the pdf is a plot of
#' @param n_row number of rows of figures on a page
#' @param n_col number of columns of figures on a page
#' @param output_file_path where to save the output
#' @param forecast_date date of forecast
#' @param model_type which model we are running
#' @param ...
#'
#' @return NULL
#' @export
#'
#' @examples
save_to_pdf <- function(list_of_plots,
                        type_of_output,
                        pdf_file_path,
                        forecast_date,
                        model_type,
                        n_row = 3,
                        n_col = 2,
                        ...) {
  model_file_name <- case_when(
    model_type == "state-level aggregated wastewater" ~ "state_agg_WW_hosp",
    model_type == "hospital admissions only" ~ "hosp_only",
    model_type == "site-level observation error" ~ "site_level_obs_error",
    model_type == "site-level infection dynamics" ~ "site_level_inf_dyn"
  )

  full_file_path <- file.path(pdf_file_path)
  create_dir(full_file_path)

  ggsave(
    filename = file.path(
      full_file_path,
      paste0(type_of_output, ".pdf")
    ),
    plot = marrangeGrob(list_of_plots, nrow = n_row, ncol = n_col),
    width = 8.5, height = 11
  )

  return(NULL)
}
