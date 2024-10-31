# Quick estimate of posterior parameters

benchmark_config <- yaml::read_yaml(file.path(
  "input", "config",
  "eval", "benchmark_config.yaml"
))

vars <- c("eta_sd", "inf_feedback")
eta_sd_draws <- tibble::tibble()
inf_feedback_draws <- tibble::tibble()

for (i in seq_along(benchmark_config$forecast_date_hosp)) {
  this_location <- benchmark_config$location_hosp[i]
  this_forecast_date <- benchmark_config$forecast_date_hosp[i]
  this_scenario <- "no_wastewater"
  for (j in seq_along(vars)) {
    fp_var <- wweval::get_filepath(benchmark_config$output_dir,
      scenario = this_scenario,
      forecast_date = this_forecast_date,
      model_type = "hosp",
      location = this_location,
      output_type = vars[j],
      file_extension = "tsv"
    )

    these_var_draws <- readr::read_tsv(fp_var)
    var_draws <- these_var_draws |>
      dplyr::mutate(
        location = this_location,
        forecast_date = this_forecast_date
      )
    if (vars[j] == "eta_sd") {
      eta_sd_draws <- dplyr::bind_rows(eta_sd_draws, var_draws)
    }
    if (vars[j] == "inf_feedback") {
      inf_feedback_draws <- dplyr::bind_rows(
        inf_feedback_draws,
        var_draws
      )
    }
  } # end loop over vars
} # end loop over forecast date-locations

# Get empirical mean, sd, logmean, and logsd------------------------------
## eta_sd---------------------------------------------------------------
mean_eta_sd <- mean(eta_sd_draws$eta_sd)
sd_eta_sd <- sd(eta_sd_draws$eta_sd)

message("Empirical mean of RW step size across 5 locations: ", mean_eta_sd)
message("Empirical sd of RW step size across 5 locations: ", sd_eta_sd)

## inf_feedback----------------------------------------------------------
logmean_inf_feedback <- mean(log(inf_feedback_draws$infection_feedback))
logsd_inf_feedback <- sd(log(inf_feedback_draws$infection_feedback))


message(
  "Empirical logmean of infection feedback across 5 locations: ",
  logmean_inf_feedback
)
message(
  "Empirical logsd of infection feedback across 5 locations: ",
  logsd_inf_feedback
)

posterior_params <- list(
  mean_eta_sd = mean_eta_sd,
  sd_eta_sd = sd_eta_sd,
  logmean_inf_feedback = logmean_inf_feedback,
  logsd_inf_feedback = logsd_inf_feedback
)
yaml::write_yaml(posterior_params, "output/posterior_params.yaml")
