#' Run an evaluation fitting or postprocessing job.

library(argparser)
options(mc.cores = 4)

runner_functions <- c(
  "fit" = wweval::eval_fit,
  "postprocess" = wweval::eval_postprocess
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
  subset.of = names(runner_functions)
)

job_runner_function <- runner_functions[[parsed$model]]

message(glue::glue(
  "Starting a {parsed$job} job with the ",
  "{parsed$model} model for index ",
  "{parsed$config_index} in config ",
  "{parsed$eval_config_path} with parameters from ",
  "{parsed$params_path}"
))


config_index <- job_runner_function(
  config_index = parsed$config_index,
  eval_config_path = parsed$eval_config_path,
  params_path = parsed$params_path,
  model = parsed$model
)
