#' Fit model for evaluation
#'
#' @param forecast_date As-of date for the forecast.
#' @param location Location to forecast.
#' @param model Model to fit. One of `"ww"` and `"hosp"`
#' @param scenario Wastewater data availability scenario to analyze.
#' @param hosp_data_dir Path to a directory containing vintaged hospital
#' admissions data in date-stamped .csv files.
#' @param ww_data_dir Path to a directory containing vintaged wastewater
#' data in date-stamped .csv files.
#' @param ww_data_mapping String associating forecast dates to wastewater
#' vintage dates. Passed to [date_of_ww_data()].
#' @param scenario_dir Path to a directory containing .csv files that
#' define wastewater data availability scenarios.
#' @param calibration_time Days of prior admissions and wastewater
#' data to which to fit the model relative to the `forecast_date`.
#' @param forecast_horizon Days forward to forecast relative to the
#' `forecast_date`.
#' @param params_path Path to a params.toml file that gives values for
#' prior hyperparameters.
#' @param raw_output_dir Path to a directory in which to save
#' raw output as serialized `.rds` files.
#' @param seed Seed for Stan's pseudorandom number generator.
#' @param iter_sampling Number of samples to draw per MCMC chain.
#' @param n_chains Number of MCMC chains to run.
#' @param adapt_delta Target acceptance probability for the No-U-Turn
#' sampler adaptation phase.
#' @param max_treedepth Maximum tree depth for the No-U-Turn sampler.
#' @param table_of_exclusions Table of outlying hospital admissions
#' datapoints to exclude. If `NULL`, exclude nothing. Default `NULL`.
#' @return NULL, saving the pre-processed data and model fitting
#' output to disk as side effects.
#' @export
eval_fit <- function(
        forecast_date,
        location,
        model,
        scenario,
        hosp_data_dir,
        ww_data_dir,
        ww_data_mapping,
        scenario_dir,
        calibration_time,
        forecast_horizon,
        params_path,
        raw_output_dir,
        seed,
        iter_sampling,
        n_chains,
        adapt_delta,
        max_treedepth,
        table_of_exclusions = NULL
) {
        checkmate::assert_names(model, subset.of = c("ww", "hosp"))
        ww_model <- model == "ww"

        params <- wwinference::get_params(params_path)
        raw_output_suffix <- get_raw_output_suffix(
                location,
                forecast_date,
                scenario
        )
        table_of_exclusions <- tibble::as_tibble(table_of_exclusions)

        save_object <- purrr::partial(
                to_rds_with_suffix,
                output_dir = raw_output_dir,
                save_suffix = raw_output_suffix
        )

        wwinference::create_dir(raw_output_dir)

        raw_input_hosp_data <- get_input_hosp_data(
                forecast_date_i = forecast_date,
                location_i = location,
                hosp_data_dir = hosp_data_dir,
                calibration_time = calibration_time
        )

        input_hosp_data <- exclude_hosp_outliers(
                raw_input_hosp_data = raw_input_hosp_data,
                forecast_date = forecast_date,
                table_of_exclusions = table_of_exclusions
        )
        save_object(input_hosp_data)

        last_hosp_data_date <- get_last_hosp_data_date(input_hosp_data)

        if (ww_model) {
                ww_data_pull <- purrr::safely(get_input_ww_data)(
                        forecast_date_i = forecast_date,
                        location_i = location,
                        scenario_i = scenario,
                        scenario_dir = scenario_dir,
                        ww_data_dir = ww_data_dir,
                        calibration_time = calibration_time,
                        last_hosp_data_date = last_hosp_data_date,
                        ww_data_mapping = ww_data_mapping
                )

                input_ww_data <- ww_data_pull$result
                save_object(input_ww_data)
                include_ww <- TRUE
        } else {
                input_ww_data <- NULL
                include_ww <- FALSE
        }

        model_spec <- wwinference::get_model_spec(
                generation_interval = wwinference::default_covid_gi,
                inf_to_count_delay = wwinference::default_covid_inf_to_hosp,
                infection_feedback_pmf = wwinference::default_covid_gi,
                params = params,
                include_ww = include_ww
        )

        fit_opts <- list(
                seed = as.integer(seed),
                iter_sampling = as.integer(iter_sampling),
                adapt_delta = as.numeric(adapt_delta),
                chains = as.integer(n_chains),
                max_treedepth = as.integer(max_treedepth)
        )

        do_fit <- !is.null(input_hosp_data) &&
                (!include_ww || !is.null(input_ww_data))

        fit_obj <- NULL

        if (do_fit) {
                message("Fitting model...")
                fit_fn <- purrr::safely(wwinference::wwinference)
                fit_out <- fit_fn(
                        ww_data = input_ww_data,
                        count_data = input_hosp_data,
                        forecast_date = forecast_date,
                        calibration_time = as.integer(calibration_time),
                        forecast_horizon = as.integer(forecast_horizon),
                        model_spec = model_spec,
                        fit_opts = fit_opts
                )
                fit_obj <- fit_out$result
                err_msg <- fit_out$error
                if (!is.null(err_msg)) {
                        message(err_msg)
                }
        } else {
                message("Missing needed data. Skipping fit")
                err_msg <- ifelse(
                        include_ww,
                        "missing ww data",
                        "fitting error or non-wastewater data error"
                )
        }

        ## If wwinference job fails, replace
        ## with missing data error for postprocessing to proceed
        ## without failure
        if (is.null(fit_obj)) {
                fit_obj <- list(fit = list(result = list(error = err_msg)))
        }

        save_object(fit_obj, save_basename = glue::glue("{model}_fit_obj"))
}
