# These functions are the worker functions that take in the training data in
# a format specific for the model operation, format it for stan, run the model,
# and format the outputs into a single dataframe containing quantiles and draws
# of generated quantities and parameters

#' This code was adapted from code written
#' (under an MIT license) as part of the `epinowcast`
#' package (https://github.com/epinowcast/epinowcast)

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

#' @title Fit aggregated model
#'
#' @description
#' Wrapper function to fit the "aggregated" model, which takes in a single
#' obervation of hospital admissions and wastewater data per time point
#'
#'
#' @param train_data dataframe containing a single locations observed hospital
#' admissions and observed wastewater concentrations, with no more than one
#' wastewater data stream for each time point
#' @param params disease-specific parameters, including hyperparameters for priors.
#' @param model_file the compiled stan model
#' @param forecast_date date of the forecast
#' @param forecast_time duration of the forecast period
#' @param model_type name of the model being fit. Options are:
#' `"state-level aggregated wastewater"`
#' @param generation_interval a discretized (in days) normalized pmf indexed
#' starting at 1 of the probability of each time from initial infection to
#' secondary transmission
#' @param inf_to_hosp a discretized (in days) normalized pmf indexed starting at
#' 0 of the probability of time from initial infection to hospital admissions
#' @param infection_feedback_pmf a discretized (in days) normalized pmf
#' that dictates the distribution of delays from incident infection to incidence
#' feedback
#' @param include_hosp whether or not to include hospital admissions data in
#' the likelihood to include
#' @param compute_likelihood whether or not to include any data in the
#' likelihood, if set to 0 returns prior predictions
#' @param n_draws number of draws to save in the draws.parquet
#' @param n_chains number of independent MCMC chains to run
#' @param iter_sampling number of iterations to save in MCMC sampling
#' @param iter_warmup number of iterations to discard in MCMC sampling
#' @param n_parallel_chains number of chains to run in parallel, default = 4
#' @param adapt_delta target proposal acceptance probability for the No-U-Turn Sampler.
#' sampler
#' @param max_treedepth max value in exponents of 2 of what the binary tree size
#' in the NUTS algorithm should have
#' @param seed random seed for the sampler
#' @param output_dir location to save the model outputs
#' @param write_files whether or not to save the outputs, default = 1 to save
#' @param ... Additional named arguments (ignored) to allow [do.call()] on
#' lists with additional entries
#'
#' @return a dataframe with model draws for all time points for WW
#'  concentraiton, hospitalizaitons, hosp per 100k, and the matched data
#' @export
#'
#' @examples
fit_aggregated_model <- function(train_data, params,
                                 model_file,
                                 forecast_date,
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

      print(paste0("Model type: ", model_type))

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
      gen_quants_draws <- get_generated_quantities_draws(all_draws, model_type)


      # Time-varying parameters depend on model type
      if (model_type == "state-level aggregated wastewater") {
        time_varying_pars <- c(
          "pred_hosp", "pred_ww", "R(t)",
          "p_hosp", "exp_state_ww_conc"
        )
      } else {
        time_varying_pars <- c(
          "pred_hosp", "pred_ww", "R(t)",
          "exp_state_ww_conc"
        )
      }


      # First make the dataframe with the generated quantities
      gen_quants_draws <- gen_quants_draws %>%
        mutate(
          include_ww = include_ww,
          forecast_date = forecast_date,
          hosp_reporting_delay = hosp_reporting_delay,
          location = location,
          data_type = "draw"
        ) %>%
        filter(name %in% time_varying_pars) %>%
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
        mutate(model_type = model_type)

      # Get a subset of the draws
      gen_quants_draws <- gen_quants_draws %>%
        select(
          name, t, value, draw, include_ww, forecast_date,
          hosp_reporting_delay,
          location, date, ww, daily_hosp_admits, daily_hosp_admits_for_eval,
          pop, obs_data, period
        ) %>%
        mutate(
          model_type = model_type
        ) %>%
        filter(draw %in% c(sample(1:max(draw), n_draws)))

      # Make one for just future hospital admissions draws
      future_hosp_draws <- gen_quants_draws %>%
        filter(name == "pred_hosp", date >= forecast_date + days(1))



      # Then make the one for the parameter draws
      parameter_draws <- get_raw_param_draws(all_draws, model_type)
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

      diagnostics <- get_diagnostics(
        fit_dynamic_rt, train_data, location,
        model_type, forecast_date
      )
      model_run_diagnostics <- get_model_run_diagnostics(
        fit_dynamic_rt
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


#' @title Fit site-level model
#' @description
#' Wrapper function to take a location's training data and fit to the
#' site-level model
#'
#' @param train_data dataframe containing the hospital admissions on each day,
#' the wastewater concentrations in each site and lab, and other metadata needed
#' to pass to stan to run the model
#' @param params disease-specific parameters, including hyperparameters for priors.
#' @param model_file the compiled stan model
#' @param forecast_date date of the forecast
#' @param forecast_time duration of the forecast period
#' @param generation_interval a discretized (in days) normalized pmf indexed
#' starting at 1 of the probability of each time from initial infection to
#' secondary transmission
#' @param inf_to_hosp a discretized (in days) normalized pmf indexed starting at
#' 0 of the probability of time from initial infection to hospital admissions
#' @param infection_feedback_pmf a discretized (in days) normalized pmf
#' that dictates the distribution of delays from incident infection to incidence
#' feedback
#' @param model_type name of the model being fit. Options are
#' `"site-level infection dynamics"`, `"site-level observation error"`,
#' `"site-level time-varying concentration"`
#' @param output_dir location to save the model outputs
#' @param adapt_delta dapt_delta target proposal acceptance probability for the No-U-Turn Sampler.
#' @param max_treedepth max value in exponents of 2 of what the binary tree size
#' in the NUTS algorithm should have
#' @param seed random seed for the sampler
#' @param include_hosp whether or not to include hospital admissions data in
#' the likelihood (default = 1) to include
#' @param compute_likelihood whether or not to include any data in the
#' likelihood (default = 1), if set to 0 returns prior predictions
#' @param n_draws number of draws to save in the draws.parquet, default = 100
#' @param n_chains number of independent MCMC chains to run, default = 4
#' @param iter_sampling number of iterations to save in MCMC sampling,
#' default = 500
#' @param iter_warmup number of iterations to discard in MCMC sampling,
#' default = 250
#' @param n_parallel_chains number of chains to run in parallel, default = 4
#' @param write_files whether or not to save the outputs, default = 1 to save
#' @param output_full_df whether or not to have targets return the full
#' dataframe of draws, default = FALSE to just return a dataframe of filepaths
#' @param ... Additional named arguments (ignored) to allow [do.call()] on
#' lists with additional entries
#'
#' @return a dataframe with model draws for all time points for WW
#'  concentraiton, hospitalizaitons, hosp per 100k, and the matched data
#' @export
#' @examples
fit_site_level_model <- function(train_data,
                                 params,
                                 model_file,
                                 forecast_date,
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
          subpop_index = NA,
          lab = NA,
          lab_wwtp_unique_id = NA,
          subpop = NA,
          below_LOD = NA,
          flag_as_ww_outlier = NA,
          lod_sewage = NA,
          ww_pop = NA,
          subpop_size = NA
        )

      # From train_data, identify whether we need to fit an additional subpop
      pop_ww <- train_data %>%
        select(site_index, ww_pop) %>%
        filter(!is.na(site_index)) %>%
        group_by(site_index) %>%
        summarise(pop_avg = mean(ww_pop)) %>%
        arrange(site_index, "desc") %>%
        pull(pop_avg)
      pop <- train_data %>%
        select(pop) %>%
        unique() %>%
        pull(pop)

      add_auxiliary_site <- ifelse(pop >= sum(pop_ww), TRUE, FALSE)

      # From the input data, which labs align with which sites and
      # which population sizes
      site_map_raw <- train_data %>%
        select(
          lab_site_index, lab_wwtp_unique_id, site_index,
          lab, site, ww_pop
        ) %>%
        distinct() %>%
        filter(!is.na(site_index))

      # Create a new variable called subpop, link it to site indexes
      site_subpop_map <- site_map_raw %>%
        mutate(subpop = paste0("Site: ", site)) %>%
        select(site_index, subpop, ww_pop) %>%
        rename(
          subpop_index = site_index,
          subpop_size = ww_pop
        )
      # Link subpops to the rest of the site and lab metadata
      site_map <- site_map_raw %>%
        left_join(site_subpop_map, by = c("site_index" = "subpop_index")) %>%
        mutate(subpop_index = site_index)

      # If we add an auxiliary site, this means we need to track an additional
      # subpopulation whose population is the difference between the state pop
      # and the sum of the wastewater site populations
      if (isTRUE(add_auxiliary_site)) {
        extra_subpop <- data.frame(
          subpop_index = max(site_map$site_index) + 1,
          subpop = "Pop not on wastewater",
          subpop_size = pop - sum(pop_ww)
        )

        subpop_map <- rbind(site_subpop_map, extra_subpop)
      } else {
        subpop_map <- site_subpop_map
      }




      # This should be at the lab site level, but you want to map to
      # sites and subpops
      gen_quants_draws_w_ww <- gen_quants_draws %>%
        filter(name == "pred_ww") %>%
        select(-subpop_index) %>%
        mutate(
          include_ww = include_ww,
          forecast_date = forecast_date,
          hosp_reporting_delay = hosp_reporting_delay,
          location = location,
          data_type = "draw"
        ) %>%
        left_join(date_df, by = "t") %>%
        ungroup() %>%
        left_join(site_map,
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
              below_LOD, flag_as_ww_outlier, lod_sewage
            ) %>%
            filter(!is.na(ww)) %>%
            unique(),
          by = c("date", "lab_site_index")
        ) %>%
        mutate(
          subpop_index = site_index
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
          left_join(subpop_map,
            by = c("subpop_index")
          ) %>%
          left_join(site_map %>%
            select(
              subpop_index,
              site_index, site, ww_pop
            ) %>%
            distinct(), by = "subpop_index") %>%
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


      parameter_draws <- get_raw_param_draws(all_draws, model_type)
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
        fit_dynamic_rt
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
