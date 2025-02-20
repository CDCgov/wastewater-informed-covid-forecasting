#' Run an evaluation fitting or postprocessing job.

library(argparser)
options(mc.cores = 4)

runner_functions <- c(
  "fit_hosp" = wweval::eval_fit_hosp,
  "fit_ww" = wweval::eval_fit_ww,
  "postprocess_hosp" = wweval::eval_post_process_hosp,
  "postprocess_ww" = wweval::eval_post_process_ww
)


parsed <- arg_parser("Run eval pipeline for one config") |>
  add_argument(
    "config_index",
    help = "Which entry in eval_config to use",
    type = "integer"
  ) |>
  add_argument(
    "eval_config_path",
    help = "path to eval_config.yaml"
  ) |>
  add_argument(
    "params_path",
    help = "path to params.toml"
  ) |>
  add_argument(
    "model",
    help = "Model to fit (either 'ww' or 'hosp')"
  ) |>
  add_argument(
    "job",
    help = "Job type to perform (either 'fit' or 'postprocess')"
  ) |>
  parse_args()

checkmate::assert_names(parsed$model,
  subset.of = c("ww", "hosp")
)
checkmate::assert_names(parsed$job,
  subset.of = c("fit", "postprocess")
)

run_name <- glue::glue("{parsed$job}_{parsed$model}")

checkmate::assert_names(run_name,
  subset.of = names(runner_functions)
)

job_runner_function <- runner_functions[[run_name]]

message(glue::glue(
  "Starging a {run_name} job for index ",
  "{parsed$config_index} in config ",
  "{parsed$eval_config_path} with parameters from ",
  "{parsed$params_path}"
))


config_index <- job_runner_function(
  config_index = parsed$config_index,
  eval_config_path = parsed$eval_config_path,
  params_path = parsed$params_path
)
