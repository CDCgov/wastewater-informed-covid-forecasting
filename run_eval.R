#' Run an evaluation fitting or postprocessing job.

library(argparser)
options(mc.cores = 4)

runner_functions <- c(
  "fit" = wweval::eval_fit,
  "postprocess" = wweval::eval_postprocess
)


parsed <- arg_parser("Run eval pipeline for one forecast problem") |>
  add_argument(
    "--forecast-date",
    help = "As-of date for the forecast."
  ) |>
  add_argument(
    "--eval-date",
    help = "As-of date for the evaluation data to use."
  ) |>
  add_argument(
    "--location",
    help = "Location to forecast."
  ) |>
  add_argument(
    "--model",
    help = "Model to fit or postprocess. One of `ww` and `hosp`"
  ) |>
  add_argument(
    "--scenario",
    help = "Wastewater data avaiability scenario to analyze."
  ) |>
  add_argument(
    "--hosp-data-dir",
    help = paste0(
      "Path to a directory containing vintaged hospital ",
      "admissions data in date-stamped .csv files."
    )
  ) |>
  add_argument(
    "--ww-data-dir",
    help = paste0(
      "Path to a directory containing vintaged wastewater ",
      "data in date-stamped .csv files."
    )
  ) |>
  add_argument(
    "--ww-data-mapping",
    help = paste0(
      "String associating forecast dates to wastewater ",
      "vintage dates."
    )
  ) |>
  add_argument(
    "--scenario-dir",
    help = paste0(
      "Path to a directory containing .csv files that ",
      "define wastewater data availability scenarios."
    )
  ) |>
  add_argument(
    "--calibration-time",
    help = paste0(
      "Days of prior admissions and wastewater data to ",
      "which to fit the model relative to the ",
      "forecast_date."
    )
  ) |>
  add_argument(
    "--forecast-horizon",
    help = "Days forward to forecast relative to the forecast_date."
  ) |>
  add_argument(
    "--params-path",
    help = paste0(
      "Path to params.toml file that gives values ",
      "for prior hyperparameters."
    )
  ) |>
  add_argument(
    "--output-dir",
    help = paste0(
      "Path to a directory in which to save ",
      "processed output."
    )
  ) |>
  add_argument(
    "--raw-output-dir",
    help = paste0(
      "Path to a directory in which to save ",
      "raw output as serialized .rds files."
    )
  ) |>
  add_argument(
    "--seed",
    help = "Seed for Stan's pseudorandom number generator."
  ) |>
  add_argument(
    "--iter-sampling",
    help = "Number of samples to draw per MCMC chain."
  ) |>
  add_argument(
    "--n-chains",
    help = "Number of MCMC chains to run."
  ) |>
  add_argument(
    "--adapt-delta",
    help = paste0(
      "Target acceptance probability for the No-U-Turn ",
      "sampler adaptation phase."
    )
  ) |>
  add_argument(
    "--max-treedepth",
    help = "Maximum tree depth for the No-U-Turn sampler."
  ) |>
  add_argument(
    "--scoring-offset",
    help = "Offset to use when scoring on transformed scales."
  ) |>
  add_argument(
    "--task-type",
    help = "Task to run. One of 'fit' and 'postprocess'"
  ) |>
  parse_args()

checkmate::assert_names(parsed$model, subset.of = c("ww", "hosp"))
checkmate::assert_names(parsed$task_type, subset.of = names(runner_functions))

job_runner_function <- runner_functions[[parsed$task_type]]

message(glue::glue(
  "Starting a {parsed$task_type} task for location {parsed$location} ",
  "and forecast date {parsed$forecast_date} using the ",
  "{parsed$model} model"
))


filtered_args <- parsed[names(parsed) %in% formalArgs(job_runner_function)]

do.call(job_runner_function, filtered_args)
