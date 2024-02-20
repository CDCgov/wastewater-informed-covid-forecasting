# These functions are the worker functions that take in the training data in
# a format specific for the model operation, format it for stan, run the model,
# and format the outputs into a single dataframe containing quantiles and draws
# of generated quantities and parameters

#' This code was adapted from code written
#' (under an MIT license) as part of the `epinowcast`
#' package (https://github.com/epinowcast/epinowcast)

# Compile a stan model and include stan function files
#' ww_model
#' @description
#' This function compiles the stan model, and is written to include the 'stan'
#' folder. Within each stan file, to include the functions, use #include
#' functions/{your_function_file}.stan
#'
#'
#' @param model
#' @param include
#' @param compile
#' @param threads
#' @param target_dir
#' @param stanc_options
#' @param cpp_options
#' @param verbose
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ww_model <- function(model,
                     include = system.file("stan",
                       package = "cfaforecastrenewalww"
                     ),
                     compile = TRUE, threads = FALSE,
                     target_dir = tempdir(), stanc_options = list(),
                     cpp_options = list(), verbose = TRUE, ...) {
  if (verbose) {
    message(glue::glue("Using model {model}."))
    message(sprintf("include is %s.", toString(include)))
  }

  if (compile) {
    monitor <- suppressMessages
    if (verbose) {
      monitor <- function(x) {
        return(x)
      }
    }
    cpp_options$stan_threads <- threads
    model <- monitor(cmdstanr::cmdstan_model(
      model,
      include_paths = include,
      stanc_options = stanc_options,
      cpp_options = cpp_options,
      ...
    ))
  }
  return(model)
}



#' Get dataframe of filepaths
#'
#' @param output_dir
#' @param forecast_date
#' @param location
#' @param model_type
#'
#' @return
#' @export
#'
#' @examples
get_df_of_filepaths <- function(output_dir,
                                forecast_date,
                                location, model_type) {
  # Specify the filepaths for each output type
  model_draws_file_path <- file.path(
    output_dir, "raw", location, model_type,
    glue::glue("draws.parquet")
  )
  quantiles_file_path <- file.path(
    output_dir, "raw", location, model_type,
    glue::glue("quantiles.parquet")
  )
  parameters_file_path <- file.path(
    output_dir, "raw", location, model_type,
    glue::glue("parameters.parquet")
  )
  future_hosp_draws_file_path <- file.path(
    output_dir, "raw", location, model_type,
    glue::glue("future_hosp_draws.parquet")
  )
  diagnostics_file_path <- file.path(
    output_dir, "raw", location, model_type,
    glue::glue("diagnostics.csv")
  )
  model_flags_file_path <- file.path(
    output_dir, "raw", location, model_type,
    glue::glue("model_flags.csv")
  )

  df <- data.frame(
    forecast_date, location, model_type,
    model_draws_file_path, quantiles_file_path,
    future_hosp_draws_file_path,
    parameters_file_path,
    diagnostics_file_path,
    model_flags_file_path
  )
  return(df)
}



# Fit the model using aggregated WW data ---------------------------------------

#' @title Fit site level model
#' @description Takes in the training data, which is a long dataframe with all the
#' site level WW observations plus the observed hospitalizations.
#' Fit the stan model to a single slice of data
#'
#' @param train_data for a single location
#' @param params of the model, these should be the same across all runs
#' @param forecast_date
#' @param forecast_time
#' @param include_hosp
#' @param compute_likelihood
#' @param damp_type
#' @param n_draws
#' @param n_chains
#' @param iter_sampling
#' @param iter_warmup
#' @param n_parallel_chains
#' @param adapt_delta
#' @param max_treedepth
#' @param model_file compiled stan model (an upstream target)
#' @param model_file_path
#' @param write_files
#'
#' @return a dataframe with 100 model draws for all time points for WW
#'  concentraiton, hospitalizaitons, hosp per 100k, and the matched data
#' @export
#'
#' @examples
fit_aggregated_model <- function(train_data, params,
                                 model_file,
                                 forecast_date,
                                 run_id,
                                 date_run,
                                 forecast_time,
                                 model_type,
                                 generation_interval,
                                 inf_to_hosp,
                                 infection_feedback_pmf,
                                 include_hosp,
                                 compute_likelihood,
                                 n_draws, n_chains,
                                 iter_sampling,
                                 iter_warmup,
                                 n_parallel_chains,
                                 adapt_delta,
                                 max_treedepth,
                                 seed,
                                 output_dir,
                                 write_files = TRUE,
                                 ...) {
  # This will act on training data for a single location

  # Need to get all the components we need for the stan model
  stan_data <- get_stan_data(
    train_data, params,
    forecast_date, forecast_time,
    include_hosp,
    compute_likelihood,
    generation_interval,
    inf_to_hosp,
    infection_feedback_pmf
  )
  metadata_df <- get_metadata(train_data)
  forecast_date <- metadata_df$forecast_date
  location <- metadata_df$location
  last_hosp_data_date <- metadata_df$last_hosp_data_date
  pop <- metadata_df$pop
  hosp_reporting_delay <- metadata_df$hosp_reporting_delay
  include_ww <- metadata_df$include_ww
  min_date <- min(train_data$date)
  max_date <- ymd(forecast_date + days(forecast_time))
  dates <- seq(
    from = min_date,
    to = max_date,
    by = "days"
  )
  message("Forecast date: ", forecast_date)

  model_type <- ifelse(include_ww == 1, "state-level aggregated wastewater",
    "hospital admissions only"
  )
  df_of_filepaths <- get_df_of_filepaths(
    output_dir,
    forecast_date,
    location, model_type
  )

  if (all(is.na(train_data$ww)) && include_ww == 1) {
    message(
      unique(train_data$location),
      " does not contain any WW data, run hospital admissions only model"
    )

    # Set model type to look for the hospital admissions only model
    model_type <- "hospital admissions only"

    df <- get_df_of_filepaths(
      output_dir,
      forecast_date,
      location, forecast_date
    )
  } else { # fit the model

    t <- seq(from = 1, to = length(dates), by = 1)

    date_df <- data.frame(date = dates, t = t)

    if (file.exists(df_of_filepaths$model_draws_file_path)) {
      # Just pass the filepath with the metadata
      df <- df_of_filepaths
    } else {
      init_fun <- function() {
        state_agg_inits(train_data, params, stan_data)
      }

      fit_dynamic_rt <- model_file$sample(
        data = stan_data,
        seed = seed,
        init = init_fun,
        iter_sampling = iter_sampling,
        iter_warmup = iter_warmup,
        chains = n_chains,
        parallel_chains = n_parallel_chains,
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth
      )

      # get model draws for all parameters
      all_draws <- fit_dynamic_rt$draws()

      # Grabs just predicted hospitalizations, WW concentration, and hosp per 100k
      gen_quants_draws <- get_generated_quantities_draws(all_draws)



      # First make the dataframe with the generated quantities
      gen_quants_draws <- gen_quants_draws %>%
        mutate(
          include_ww = include_ww,
          forecast_date = forecast_date,
          hosp_reporting_delay = hosp_reporting_delay,
          location = location,
          data_type = "draw"
        ) %>%
        filter(name %in% c(
          "pred_hosp", "pred_ww", "R(t)",
          "p_hosp", "exp_state_ww_conc"
        )) %>%
        left_join(date_df, by = "t") %>%
        left_join(train_data %>% select(
          t, date, ww, daily_hosp_admits,
          daily_hosp_admits_for_eval, pop
        ), by = c("date", "t"))


      hosp_per_100k <- gen_quants_draws %>%
        filter(name == "pred_hosp") %>% # grab the generated quantities
        mutate(
          name = "pred_hosp_per_100k",
          value = 1e5 * value / pop
        )

      gen_quants_draws <- rbind(gen_quants_draws, hosp_per_100k)


      # Make a column for matched observed data
      gen_quants_draws <- gen_quants_draws %>%
        mutate(
          obs_data = case_when(
            name == "pred_hosp" ~ daily_hosp_admits_for_eval,
            name == "pred_hosp_per_100k" ~ 1e5 * daily_hosp_admits_for_eval / pop,
            name == "pred_ww" ~ ww,
            TRUE ~ NA
          ),
          # Column for period
          period = case_when(
            date <= last_hosp_data_date ~ "calibration",
            (date > last_hosp_data_date & date <= forecast_date) ~ "nowcast",
            date > forecast_date ~ "forecast"
          )
        ) %>%
        ungroup()

      # Get the quantiles on hospitalizations and WW concentration
      quantiles <- get_all_quantiles(gen_quants_draws %>%
        filter(name %in% c(
          "pred_hosp", "pred_ww",
          "R(t)", "exp_state_ww_conc"
        ))) %>%
        mutate(model_type = ifelse(
          include_ww == 1,
          "state-level aggregated wastewater",
          "hospital admissions only"
        ))

      # Get a subset of the draws
      gen_quants_draws <- gen_quants_draws %>%
        select(
          name, t, value, draw, include_ww, forecast_date,
          hosp_reporting_delay,
          location, date, ww, daily_hosp_admits, daily_hosp_admits_for_eval,
          pop, obs_data, period
        ) %>%
        mutate(
          model_type = ifelse(include_ww == 1, "state-level aggregated wastewater",
            "hospital admissions only"
          )
        ) %>%
        filter(draw %in% c(sample(1:max(draw), n_draws)))

      # Make one for just future hospital admissions draws
      future_hosp_draws <- gen_quants_draws %>%
        filter(name == "pred_hosp", date >= forecast_date + days(1))



      # Then make the one for the parameter draws
      parameter_draws <- get_raw_param_draws(all_draws)
      parameter_draws <- parameter_draws %>%
        mutate(
          data_type = "draw",
          forecast_date = forecast_date,
          include_ww = include_ww,
          location = location,
          hosp_reporting_delay = hosp_reporting_delay,
          pop = pop,
          model_type = ifelse(include_ww == 1, "state-level aggregated wastewater",
            "hospital admissions only"
          )
        )

      diagnostics <- get_diagnostics(
        fit_dynamic_rt, train_data, location,
        model_type, forecast_date
      )
      model_run_diagnostics <- get_model_run_diagnostics(
        fit_dynamic_rt, n_chains
      )

      # Make a 1 row dataframe that contains the metadata combined with the
      # filepath where the dataframes of draws, quantiles, and parquets are saved
      df <- df_of_filepaths

      if (isTRUE(write_files)) {
        # Need to create new folder
        create_dir(file.path(
          output_dir, "raw", location, model_type
        ))
        create_dir(file.path(
          output_dir, "raw", location, model_type,
          "stan_objects"
        ))

        arrow::write_parquet(
          x = gen_quants_draws,
          sink = df$model_draws_file_path
        )
        arrow::write_parquet(
          x = quantiles,
          sink = df$quantiles_file_path
        )
        arrow::write_parquet(
          x = parameter_draws,
          sink = df$parameters_file_path
        )
        arrow::write_parquet(
          x = future_hosp_draws,
          sink = df$future_hosp_draws_file_path
        )
        readr::write_csv(
          diagnostics, df$diagnostics_file_path
        )
        if (nrow(model_run_diagnostics) > 0) {
          readr::write_csv(
            model_run_diagnostics, df$model_flags_file_path
          )
        }


        fit_dynamic_rt$save_output_files(
          dir = file.path(
            output_dir, "raw", location, model_type,
            "stan_objects"
          )
        )
      }
    } # end else for if filepath exists
  } # end else for if wastewater data

  return(df)
}


#' Fit the stan model to a single slice of data
#'
#' @param train_data for a single location
#' @param params of the model, these should be the same across all runs
#' @param config_vars tells the model whether or not to include WW,
#'                    include hosp, compute likelihood, and what damp type.
#' @param model_file compiled stan model (an upstream target)
#'
#' @return a dataframe with 100 model draws for all time points for WW
#'  concentraiton, hospitalizaitons, hosp per 100k, and the matched data
#' @export
#'
#' @examples
fit_site_level_model <- function(train_data,
                                 params,
                                 model_file,
                                 forecast_date,
                                 run_id,
                                 date_run,
                                 forecast_time,
                                 generation_interval,
                                 inf_to_hosp,
                                 infection_feedback_pmf,
                                 model_type,
                                 output_dir,
                                 adapt_delta,
                                 max_treedepth,
                                 seed,
                                 include_hosp = 1,
                                 compute_likelihood = 1,
                                 n_draws = 100,
                                 n_chains = 4,
                                 iter_sampling = 500,
                                 iter_warmup = 250,
                                 n_parallel_chains = 4,
                                 write_files = TRUE,
                                 output_full_df = FALSE,
                                 ...) {
  # Get the single model run variables and make them into global variables
  metadata_df <- get_metadata(train_data)
  forecast_date <- metadata_df$forecast_date
  location <- metadata_df$location
  last_hosp_data_date <- metadata_df$last_hosp_data_date
  pop <- metadata_df$pop
  hosp_reporting_delay <- metadata_df$hosp_reporting_delay
  include_ww <- metadata_df$include_ww
  min_date <- min(train_data$date)
  max_date <- ymd(forecast_date + days(forecast_time))
  dates <- seq(
    from = min_date,
    to = max_date,
    by = "days"
  )
  t <- seq(from = 1, to = length(dates), by = 1)

  date_df <- data.frame(date = dates, t = t)

  # Check that there is any wastewater.
  # If there's not, include_ww =0, model_type = hospital_admissions_only
  if (all(is.na(train_data$ww))) {
    message(
      unique(train_data$location),
      " does not contain any WW data, run hospital admissions only model"
    )

    # Set model type to look for the hospital admissions only model
    model_type <- "hospital admissions only"

    df_of_filepaths <- get_df_of_filepaths(
      output_dir,
      forecast_date,
      location, model_type
    )
    # Still make the dataframe to be returned, but point it to the hospital
    # admissions only output
    df <- df_of_filepaths
  } else { # Otherwise, do everything else

    df_of_filepaths <- get_df_of_filepaths(
      output_dir,
      forecast_date,
      location, model_type
    )

    if (file.exists(df_of_filepaths$model_draws_file_path)) {
      # Just pass the filepath with the metadata
      df <- df_of_filepaths
      message("Model results already exist, returning path to model results")
    } else {
      # Flag WW outliers
      train_data <- flag_ww_outliers(train_data)


      # Need to get all the components we need for the stan model
      stan_data <- get_stan_data_site_level_model(
        train_data, params,
        forecast_date, forecast_time,
        model_type,
        generation_interval,
        inf_to_hosp,
        infection_feedback_pmf,
        include_hosp,
        compute_likelihood
      )

      if (model_type == "site-level time-varying concentration") {
        init_fun <- function() {
          time_varying_conc_inits(train_data, params, stan_data)
        }
      } else if (model_type == "site-level observation error") {
        init_fun <- function() {
          site_level_obs_inits(train_data, params, stan_data)
        }
      } else if (model_type == "site-level infection dynamics") {
        init_fun <- function() {
          site_level_inf_inits(train_data, params, stan_data)
        }
      } else {
        message("Model type not specified properly")
      }

      fit_dynamic_rt <- model_file$sample(
        data = stan_data,
        seed = seed,
        init = init_fun,
        iter_sampling = iter_sampling,
        iter_warmup = iter_warmup,
        chains = n_chains,
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth,
        parallel_chains = n_parallel_chains
      )
      print("Model ran")

      # get model draws for all parameters
      all_draws <- fit_dynamic_rt$draws()

      # Grabs just predicted hospitalizations, WW concentration,
      # and hosp per 100k
      gen_quants_draws <- get_gen_quants_draws(
        all_draws, model_type
      )

      # First make the dataframe with the generated quantities
      gen_quants_draws_non_ww <- gen_quants_draws %>%
        filter(name != "pred_ww") %>%
        mutate(
          include_ww = include_ww,
          forecast_date = forecast_date,
          hosp_reporting_delay = hosp_reporting_delay,
          location = location,
          data_type = "draw"
        ) %>%
        filter(name %in% c(
          "pred_hosp", "R(t)", "p_hosp",
          "exp_state_ww_conc"
        )) %>%
        left_join(date_df, by = "t") %>%
        left_join(
          train_data %>%
            select(
              t, date, daily_hosp_admits,
              daily_hosp_admits_for_eval, pop
            ) %>%
            distinct(),
          by = c("date", "t")
        ) %>%
        mutate(
          ww = NA,
          site = NA,
          site_index = NA,
          lab = NA,
          lab_wwtp_unique_id = NA,
          below_LOD = NA,
          flag_as_ww_outlier = NA,
          lod_sewage = NA,
          ww_pop = NA
        )

      # This should be at the lab site level
      gen_quants_draws_w_ww <- gen_quants_draws %>%
        filter(name == "pred_ww") %>%
        select(-site_index) %>%
        mutate(
          include_ww = include_ww,
          forecast_date = forecast_date,
          hosp_reporting_delay = hosp_reporting_delay,
          location = location,
          data_type = "draw"
        ) %>%
        left_join(date_df, by = "t") %>%
        ungroup() %>%
        left_join(
          train_data %>%
            select(
              lab_site_index, lab_wwtp_unique_id, site_index,
              lab, site
            ) %>%
            distinct(),
          by = c("lab_site_index")
        ) %>%
        left_join(
          train_data %>%
            select(
              date, daily_hosp_admits,
              daily_hosp_admits_for_eval, pop
            ) %>%
            distinct(),
          by = c("date")
        ) %>%
        left_join(
          train_data %>%
            ungroup() %>%
            select(
              lab_site_index, date, ww,
              below_LOD, flag_as_ww_outlier, lod_sewage, ww_pop
            ) %>%
            filter(!is.na(ww)) %>%
            unique(),
          by = c("date", "lab_site_index")
        ) %>%
        select(colnames(gen_quants_draws_non_ww))

      if (model_type != "site-level infection dynamics") {
        gen_quants_draws_w_data <- rbind(
          gen_quants_draws_non_ww,
          gen_quants_draws_w_ww
        )
      } else {
        gen_quants_draws_ww_site <- gen_quants_draws %>%
          filter(name == "R_site_t") %>%
          select(-lab_site_index) %>%
          mutate(
            include_ww = include_ww,
            forecast_date = forecast_date,
            hosp_reporting_delay = hosp_reporting_delay,
            location = location,
            data_type = "draw"
          ) %>%
          left_join(date_df, by = "t") %>%
          ungroup() %>%
          left_join(
            train_data %>%
              select(site_index, site, ww_pop) %>%
              distinct(),
            by = c("site_index")
          ) %>%
          mutate(
            lab_site_index = NA,
            lab_wwtp_unique_id = NA,
            lab = NA,
            ww = NA,
            below_LOD = NA,
            flag_as_ww_outlier = NA,
            lod_sewage = NA
          ) %>%
          left_join(
            train_data %>%
              select(date, daily_hosp_admits, daily_hosp_admits_for_eval, pop) %>%
              distinct(),
            by = c("date")
          ) %>%
          select(colnames(gen_quants_draws_non_ww))

        gen_quants_draws_w_data <- rbind(
          gen_quants_draws_non_ww,
          gen_quants_draws_w_ww,
          gen_quants_draws_ww_site
        )
      }

      hosp_per_100k <- gen_quants_draws_w_data %>%
        filter(name == "pred_hosp") %>% # grab the generated quantities
        mutate(
          name = "pred_hosp_per_100k",
          value = 1e5 * value / pop
        )

      gen_quants_draws_w_data <- rbind(gen_quants_draws_w_data, hosp_per_100k)


      # Make a column for matched observed data
      gen_quants_draws_w_data <- gen_quants_draws_w_data %>%
        mutate(
          obs_data = case_when(
            name == "pred_hosp" ~ daily_hosp_admits_for_eval,
            name == "pred_hosp_per_100k" ~ 1e5 * daily_hosp_admits_for_eval / pop,
            name == "pred_ww" ~ ww,
            TRUE ~ NA
          )
        ) %>%
        mutate( # Column for period
          period = case_when(
            date <= last_hosp_data_date ~ "calibration",
            (date > last_hosp_data_date & date <= forecast_date) ~ "nowcast",
            date > forecast_date ~ "forecast"
          )
        ) %>%
        ungroup()

      model_type_to_save <- ifelse(include_ww == 0, "hospital admissions only",
        model_type
      )

      gen_quants_draws_w_data <- gen_quants_draws_w_data %>%
        ungroup() %>%
        mutate(model_type = model_type_to_save)

      quantiles <- get_all_quantiles(gen_quants_draws_w_data %>%
        filter(name %in% c(
          "pred_hosp", "exp_state_ww_conc",
          "R(t)"
        )))

      gen_quants_draws_w_data <- gen_quants_draws_w_data %>%
        filter(draw %in% c(sample(1:max(draw), n_draws)))


      parameter_draws <- get_raw_param_draws(all_draws)
      parameter_draws <- parameter_draws %>%
        mutate(
          data_type = "draw",
          forecast_date = forecast_date,
          include_ww = include_ww,
          location = location,
          hosp_reporting_delay = hosp_reporting_delay,
          pop = pop,
          model_type = model_type
        )

      # Make one for just future hospital admissions draws
      future_hosp_draws <- gen_quants_draws_w_data %>%
        filter(name == "pred_hosp", date >= forecast_date + days(1))

      if (isTRUE(output_full_df)) {
        df <- list(
          gen_quants_draws_w_data,
          quantiles,
          parameter_draws
        )
      }

      # Get model run and data diagnostics
      diagnostics <- get_diagnostics(
        fit_dynamic_rt, train_data, location,
        model_type_to_save, forecast_date
      )
      model_run_diagnostics <- get_model_run_diagnostics(
        fit_dynamic_rt, n_chains
      )

      # Make a 1 row dataframe that contains the metadata combined with the
      # filepath where the dataframes of draws, quantiles, and parquets are saved
      df <- df_of_filepaths

      if (isTRUE(write_files)) {
        # Need to create each of these new folders
        create_dir(file.path(
          output_dir, "raw", location, model_type
        ))
        create_dir(file.path(
          output_dir, "raw", location, model_type,
          "stan_objects"
        ))

        arrow::write_parquet(
          x = gen_quants_draws_w_data,
          sink = df$model_draws_file_path
        )
        arrow::write_parquet(
          x = quantiles,
          sink = df$quantiles_file_path
        )
        arrow::write_parquet(
          x = parameter_draws,
          sink = df$parameters_file_path
        )
        arrow::write_parquet(
          x = future_hosp_draws,
          sink = df$future_hosp_draws_file_path
        )
        readr::write_csv(
          diagnostics, df$diagnostics_file_path
        )
        if (nrow(model_run_diagnostics) > 0) {
          readr::write_csv(
            model_run_diagnostics, df$model_flags_file_path
          )
        }

        fit_dynamic_rt$save_output_files(
          dir = file.path(
            output_dir, "raw", location, model_type,
            "stan_objects"
          )
        )
      }
    }
  } # end else if filepath exists
  return(df)
}
