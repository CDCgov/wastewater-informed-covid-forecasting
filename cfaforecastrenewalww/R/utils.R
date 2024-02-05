#' @title Normalize vector to a simplex
#'
#' @param vector numeric vector
#'
#' @return vector whos sum adds to 1
#' @export
#' @examples
#' to_simplex(c(1, 1, 1))
#' @noRd
to_simplex <- function(vector) {
  return(vector / sum(vector))
}


#' @title Convert to logmean in lognorm distribution
#' @description see arithmetic moments here
#' https://en.wikipedia.org/wiki/Log-normal_distribution
#' @param mean
#' @param sd
#'
#' @return corresponding mean of the lognormal distribution
#' @export
#'
#' @examples
convert_to_logmean <- function(mean, sd) {
  logmean <- log(mean^2 / sqrt(sd^2 + mean^2))
  return(logmean)
}


#' @title Convert to logsd in lognormal distribution
#' @description@description see arithmetic moments here
#' https://en.wikipedia.org/wiki/Log-normal_distribution
#'
#' @param mean
#' @param sd
#'
#' @return corresponding stdev of the lognormal distribution
#' @export
#'
#' @examples
convert_to_logsd <- function(mean, sd) {
  logsd <- sqrt(log(1 + (sd^2 / mean^2)))
  return(logsd)
}


#' @title Suppress output and messages for code.
#' @description Used in the pipeline.
#' @return The result of running the code.
#' @param code Code to run quietly.
#' @examples
#' library(cmdstanr)
#' compile_model("stan/model.stan")
#' quiet(fit_model("stan/model.stan", simulate_data_discrete()))
#' out
#' @noRd
quiet <- function(code) {
  sink(nullfile())
  on.exit(sink())
  suppressMessages(code)
}


#' @title Get the quantiles being used
#'
#' @param grouped_quantiles
#'
#' @return
#' @export
#'
#' @examples
get_quantiles <- function(grouped_quantiles) {
  list_of_quantiles <- grouped_quantiles %>%
    select(quantile) %>%
    unique() %>%
    pull()
  return(list_of_quantiles)
}

#' @title Get metadata from a single group training data
#' @description From a single model run, get the relevant metadata
#' @param train_data single group training data
#' (single location, model type, forecast date)
#'
#' @return single row dataframe containing values for forecast date, pop,
#'  include_ww, hosp_reporting delay, and location
#' @export
#'
#' @examples
get_metadata <- function(train_data) {
  last_hosp_data_date <- max(train_data$date[!is.na(
    train_data$daily_hosp_admits
  )])

  metadata_df <- train_data %>%
    ungroup() %>%
    select(
      forecast_date,
      pop,
      include_ww,
      hosp_reporting_delay,
      location
    ) %>%
    distinct() %>%
    mutate(last_hosp_data_date = last_hosp_data_date)

  stopifnot(
    "Trying to access metadata from more than one model run" =
      nrow(metadata_df) == 1
  )

  return(metadata_df)
}

#' @title Create a new directory if one doesn't exist
#' @description
#' Function to create a directory for the specified output file path if needed.
#' dir_create won't throw a warning if its already made though!
#'
#'
#' @param output_file_path file path that may or may not need to be created
#'
#' @export
#'
#' @examples
create_dir <- function(output_file_path) {
  if (!file.exists(output_file_path)) {
    fs::dir_create(output_file_path, recurse = TRUE, mode = "0777")
    Sys.chmod(output_file_path, mode = "0777", use_umask = FALSE)
  }
}

#' @title Get index matrix
#' @description Get the matrix needed to convert a vetor from weekly to daily
#' @param n_days number of days we will expand to
#' @param n_weeks number of weeks those days correspond to
#'
#' @return a n_day x n_week matrix for multiplying by weekly estimated
#' value to conver it to daily
#' @export
#'
#' @examples
get_ind_m <- function(n_days, n_weeks) {
  ind_m <- matrix(nrow = n_days, ncol = n_weeks)
  for (i in 1:n_days) {
    for (j in 1:n_weeks) {
      if (((i - 1) %/% 7) + 1 == j) {
        ind_m[i, j] <- 1
      } else {
        ind_m[i, j] <- 0
      }
    }
  }

  return(ind_m)
}

#' @title Get index matrix
#' @description Get the matrix needed to convert a vetor from weekly to daily
#' @param n_days number of days we will expand to
#' @param n_weeks number of weeks those days correspond to
#'
#' @return a n_day x n_week matrix for multiplying by weekly estimated
#' value to conver it to daily
#' @export
#'
#' @examples
get_ind_m_cum_sum <- function(n_days, n_weeks) {
  ind_m <- matrix(nrow = n_days, ncol = n_weeks)
  for (i in 1:n_days) {
    for (j in 1:n_weeks) {
      if (((i - 1) %/% 7) + 1 >= j) {
        ind_m[i, j] <- 1
      } else {
        ind_m[i, j] <- 0
      }
    }
  }

  return(ind_m)
}

#' Add probability mass functions
#'
#' This function allows the addition of probability mass functions (PMFs) to
#' produce a new PMF. This is useful for example in the context of reporting
#' delays where the PMF of the sum of two Poisson distributions is the
#' convolution of the PMFs.
#'
#' This code was adapted from code written
#' (under an MIT license) as part of the `epinowcast`
#' package (https://github.com/epinowcast/epinowcast)
#'
#' @param pmfs A list of vectors describing the probability mass functions to
#'
#' @return A vector describing the probability mass function of the sum of the
#'
#' @export
#' @examples
#' # Sample and analytical PMFs for two Poisson distributions
#' x <- rpois(10000, 5)
#' xpmf <- dpois(0:20, 5)
#' y <- rpois(10000, 7)
#' ypmf <- dpois(0:20, 7)
#' # Add sampled Poisson distributions up to get combined distribution
#' z <- x + y
#' # Analytical convolution of PMFs
#' conv_pmf <- add_pmfs(list(xpmf, ypmf))
#' conv_cdf <- cumsum(conv_pmf)
#' # Empirical convolution of PMFs
#' cdf <- ecdf(z)(0:42)
#' # Compare sampled and analytical CDFs
#' plot(conv_cdf)
#' lines(cdf, col = "black")
add_pmfs <- function(pmfs) {
  d <- length(pmfs)
  if (d == 1) {
    return(pmfs[[1]])
  }
  if (!is.list(pmfs)) {
    return(pmfs)
  }
  # P(Z = z) = sum_over_x(P(X = x) * P(Y = z - x)) # nolint
  return(
    Reduce(x = pmfs, f = function(conv, pmf) {
      lc <- length(conv)
      wd <- seq_len(lc) - 1
      proc <- numeric(lc + length(pmf))
      for (j in seq_along(pmf)) {
        proc[j + wd] <- proc[j + wd] + pmf[j] * conv
      }
      return(proc)
    })
  )
}

#' Get a random string of length 8 containing 3 letters, 4 digits, 1 letter
#'
#' @param forecast_date
#' @param date_run
#' @param ww_data_path
#'
#' @return the random string with uppercase letters
#' @export
#'
#' @examples
get_random_string <- function(forecast_date, date_run, ww_data_path) {
  time_hash <- rlang::hash(Sys.time())
  random_string <- as.character(substr(time_hash, 1, 5))

  return(random_string)
}

#' Check whether a required package is installed
#' @param pkg_name Character scalar. The name of the package to check.
#' @return If the package is not available, it returns an error.
#' @noRd
check_package_is_installed <- function(pkg_name) {
  if (!requireNamespace(pkg_name)) {
    stop(
      glue::glue("The R package `{pkg_name}` is not available. "),
      glue::glue("Use `install.packages(\"{pkg_name}\")`.")
    )
  }
}

# From datasets::state.abb
us_states_abb <- c(
  "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL",
  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
  "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
  "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
  "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)
