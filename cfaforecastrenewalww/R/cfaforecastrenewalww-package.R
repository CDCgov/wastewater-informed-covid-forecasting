#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import ggplot2
#' @import lubridate
#' @import here
#' @import cmdstanr
#' @import yaml
#' @import tidybayes
#' @import zoo
#' @import boot
## usethis namespace: end
NULL

#' @importFrom arrow read_parquet read_ipc_stream write_parquet
#' @importFrom jsonlite fromJSON
#' @importFrom posterior subset_draws summarize_draws as_draws
#' @importFrom grDevices boxplot.stats
#' @importFrom stats dgamma dlnorm dnbinom dweibull ecdf mad median qlogis quantile
#'  rnbinom rnorm sd time rlnorm
#' @importFrom utils tail write.table
#' @importFrom scoringutils score summarize_scores add_coverage
#' @importFrom tidyr unnest
#' @importFrom readr read_csv write_csv
#' @importFrom glue glue
NULL

# This is currently causing conflicts with dplyr.
# For now it is enough to add it in DESCRIPTION.
# @import data.table
