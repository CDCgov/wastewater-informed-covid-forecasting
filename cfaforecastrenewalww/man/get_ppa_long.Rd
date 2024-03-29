% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posterior_predictive_analysis.R
\name{get_ppa_long}
\alias{get_ppa_long}
\title{Reformats output of posterior_predictive_analysis() into something more easily usable}
\usage{
get_ppa_long(x, keep_generated = FALSE)
}
\arguments{
\item{x}{output of posterior_predictive_analysis()}

\item{keep_generated}{boolean indicating whether to keep generated quantities or not}
}
\value{
long-format tibble which is per-parameter, per-replicate-analysis
}
\description{
Reformats output of posterior_predictive_analysis() into something more easily usable
}
