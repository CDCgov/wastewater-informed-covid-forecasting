#' Get the generation interval from a gamma distribution
#'
#' @param mean_gi
#' @param stdev_gi
#' @param gt_max
#'
#' @return a normalized generation interval assuming gamma distribution
#' @export
#'
#' @examples
get_generation_interval_gamma <- function(mean_gi, stdev_gi, gt_max) {
  a <- (mean_gi^2) / (stdev_gi^2)
  b <- mean_gi / (stdev_gi^2)

  generation_interval <- dgamma(seq(0, gt_max - 1, 1), shape = a, scale = b)
  # make sure to normalize so it always sums to 1
  generation_interval <- generation_interval / sum(generation_interval)
  return(generation_interval)
}

#' Get the generation interval from a lognormal distribution
#'
#' @param mu
#' @param sigma
#' @param gt_max
#'
#' @return a normalized generation interval assuming a lognormal distribution
#' @export
#'
#' @examples
get_generation_interval_lnorm <- function(mu = 0.92877,
                                          sigma = 0.526,
                                          gt_max = 15) {
  generation_interval <- dlnorm(seq(0, gt_max - 1, 1),
    meanlog = mu,
    sdlog = sigma
  )
  # make sure to normalize so it always sums to 1
  generation_interval <- generation_interval / sum(generation_interval)

  return(generation_interval)
}

#' Drop the first element of a simplex
#'
#' When this vector corresponds to the generation interval distribution, we
#' want to drop this first bin. The renewal equation assumes that same-day
#' infection and onward transmission does not occur, and we assume
#' everything is 1 indexed not 0 indeced. We need to
#' manually drop the first element from the PMF vector.
#'
#' @param x A numeric vector, sums to 1. Corresponds to a discretized PDF or PMF
#'   (usually the GI distribution).
#'
#' @return A numeric vector, sums to 1.
#' @export
drop_first_and_renormalize <- function(x) {
  # Check input sums to 1
  stopifnot(abs(sum(x) - 1) < 1e-8)
  # Drop and renormalize
  y <- x[2:length(x)] / sum(x[2:length(x)])
  vec_outside_tol <- abs(sum(y) - 1L) > 1e-10
  # Normalize until within tolerance
  while (vec_outside_tol) {
    y <- y / sum(y)
    vec_outside_tol <- abs(sum(y) - 1L) > 1e-10
  }
  return(y)
}

#' Simulate daily double censored PMF. From {epinowcast}:
#' https://package.epinowcast.org/dev/reference/simulate_double_censored_pmf.html
#'
#' This function simulates the probability mass function of a  daily
#' double-censored process. The process involves two distributions: a primary
#' distribution which represents the censoring process for the primary event
#' and another distribution (which is offset by the primary).
#'
#' Based off of:
#' https://www.medrxiv.org/content/10.1101/2024.01.12.24301247v1
#'
#' @param max Maximum value for the computed CDF. If not specified, the maximum
#' value is the maximum simulated delay.
#' @param fun_primary Primary distribution function (default is \code{runif}).
#' @param fun_dist Distribution function to be added to the primary (default is
#' \code{rlnorm}).
#' @param n Number of simulations (default is 1e6).
#' @param primary_args List of additional arguments to be passed to the primary
#' distribution function.
#' @param dist_args List of additional arguments to be passed to the
#' distribution function.
#' @param ... Additional arguments to be passed to the distribution function.
#' This is an alternative to `dist_args`.
#'
#' @return A numeric vector representing the PMF.
#' @export
#' @family modelmodulehelpers
#' @examples
#' simulate_double_censored_pmf(10, meanlog = 0, sdlog = 1)
simulate_double_censored_pmf <- function(
    max, fun_primary = stats::runif, primary_args = list(),
    fun_dist = stats::rlnorm,
    dist_args = list(...), n = 1e6, ...) {
  primary <- do.call(fun_primary, c(list(n), primary_args))
  secondary <- primary + do.call(fun_dist, c(list(n), dist_args))
  delay <- floor(secondary) - floor(primary)
  if (missing(max)) {
    max <- base::max(delay)
  }
  cdf <- ecdf(delay)(0:max)
  pmf <- c(cdf[1], diff(cdf))
  vec_outside_tol <- abs(sum(pmf) - 1L) > 1e-10
  while (vec_outside_tol) {
    pmf <- pmf / sum(pmf)
    vec_outside_tol <- abs(sum(pmf) - 1L) > 1e-10
  }
  return(pmf)
}


#' @title Make reporting delay pmf
#' @description
#' Convolve the incubation period pmf with the symptom to hospital admission pmf
#' and normalize
#'
#' @param incubation_period_pmf
#' @param hospital_onset_delay_pmf
#'
#' @return convolution of incubation period and sympton onset to hospital
#' admission pmf
#' @export
#'
#' @examples
make_reporting_delay_pmf <- function(incubation_period_pmf, hospital_onset_delay_pmf) {
  pmfs <- list(
    "incubation_period" = incubation_period_pmf,
    "hosp_onset_delay" = hospital_onset_delay_pmf
  )

  infection_to_hosp_delay_pmf <- add_pmfs(pmfs) %>%
    (\(x) x / sum(x))()
  return(infection_to_hosp_delay_pmf)
}


#' @title Make incubation period pmf
#' @description This uses NNH's logic and workflow to make a pmf corresponding to
#' the incubation period for COVID after Omicron used in Park et al 2023
#' (which we also use for the
#' lognormal generation interval). These estimates are from early Omicron.
#'
#' @return pmf of incubation period
#' @export
#'
#' @examples
make_incubation_period_pmf <- function(backward_scale = 3.60,
                                       backward_shape = 1.50,
                                       r = 0.15) {
  # From: Park, Sang Woo, et al. "Inferring the differences in incubation-period
  # and generation-interval distributions of the Delta and Omicron variants of
  # SARS-CoV-2." Proceedings of the National Academy of Sciences 120.22 (2023):
  # e2221887120.

  # "However, when we account for growth-rate differences and reestimate the
  # forward incubation periods, we find that both variants have similar
  # incubation-period distributions with a mean of 4.1 d (95% CI: 3.8 to 4.4 d)
  # for the Delta variant and 4.2 d (95% CI: 3.6 to 4.9 d) for the Omicron
  # variant Fig. 3B)."

  # https://github.com/parksw3/omicron-generation/blob/master/scripts/calculate_incubation_mle.R
  # https://github.com/parksw3/omicron-generation/blob/master/rdacache/calculate_incubation_mle.rda
  # Fits a Weibull to the data

  # Relies on fundamental assumption about epidemic growth rate.

  # Lognormal mean and sd to check
  mu <- 1.245956
  sigma <- 0.5872772

  # exp(mu + 0.5 * sigma**2) = 4.131 \appox 4.2
  # Close but not but exactly the 4.2 reported. I checked by cloning the repo
  # and rerunning the code, which produces the same slightly different results.
  # Perhaps a misreading of 4.13 as 4.18?
  # The estimate for non-sgtf (Delta) matches the reported result:
  # exp(1.26321 + 0.5 *  0.5378194**2) = 4.087 \approx 4.1

  # "Since incubation-period data are not provided, we are not able to fit Eq. 2
  # directly; instead, we take the backward incubation-period distributions
  # bI(x) estimated by ref. 4, which was originally assumed to follow a Weibull
  # distribution, and apply Eq. 2. In particular, ref. 4 estimated the scale and
  # shape parameters of the Weibull distribution to be 4.93 (95% CI: 4.51 to
  # 5.37) and 1.83 (95% CI: 1.59 to 2.08), respectively, for the Delta cases,
  # and 3.60 (95% CI: 3.23 to 3.98) and 1.50 (95% CI: 1.32 to 1.70),
  # respectively, for Omicron cases."

  # This is what's plotted in Fig. 2B
  # https://github.com/parksw3/omicron-generation/blob/d36d4568bfd3b3d389b30282758b9c322cfe2b9f/figure/figure_incubation.R#L23C1-L23C1 # nolint
  corrected_sgtf <- tibble(
    time = seq(0, 23, by = 1), # 23 seems to get most of the distribution mass
    density0 = dweibull(time, shape = backward_shape, scale = backward_scale) * exp(r * time)
  )

  # Check:
  # sum(corrected_sgtf$time * corrected_sgtf$density0)/sum(corrected_sgtf$density0) =
  #  4.134 \approx exp(mu + 0.5 * sigma**2)

  inc_period_pmf <- corrected_sgtf$density0 / sum(corrected_sgtf$density0)
  return(inc_period_pmf)
}


#' @title Make hospital onset delay pmf
#' @description Uses the parameter estimates from cfa-parameter-estimates,
#' which is based on Danache et al linelist data from symptom onset to hospital
#' admission. See below:
#' https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0261428
#'
#' @param neg_binom_mu
#' @param neg_binom_size
#'
#' @return pmf of distribution from symptom onset to hospital admission
#' @export
#'
#' @examples
make_hospital_onset_delay_pmf <- function(neg_binom_mu = 6.98665, neg_binom_size = 2.490848) {
  density <- dnbinom(x = seq(0, 30, 1), mu = neg_binom_mu, size = neg_binom_size)
  hosp_onset_delay_pmf <- density / sum(density)

  return(hosp_onset_delay_pmf)
}
