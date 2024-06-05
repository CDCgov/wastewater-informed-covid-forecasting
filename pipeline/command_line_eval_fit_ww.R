library(argparser, quietly = TRUE)
library(cfaforecastrenewalww)
library(wweval)
library(ggplot2)
## some functions from plots.R complain about aes()
## function not existing if we don't load ggplot2

options(mc.cores = 4)

# Command Line Version ------------------------------------------------------------------------
parsed_args <- arg_parser("Run eval pipeline for one config") |>
  add_argument("config_index", help = "index of entry in eval_config to use", type = "integer") |>
  add_argument("eval_config_path", help = "path to eval_config.yaml") |>
  add_argument("params_path", help = "path to params.toml") |>
  parse_args()

eval_fit_ww(
  config_index = parsed_args$config_index,
  eval_config_path = parsed_args$eval_config_path,
  params_path = parsed_args$params_path
)
