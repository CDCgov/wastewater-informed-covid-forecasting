% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_model_outputs.R
\name{get_wastewater_diagnostic}
\alias{get_wastewater_diagnostic}
\title{Get some basic flags on the wastewater data, for internal use and for
reporting out in our metadata}
\usage{
get_wastewater_diagnostic(
  calib_data,
  delay_thres = 21,
  n_dps_thres = 5,
  prop_below_lod_thres = 0.5,
  sd_thres = 0.1,
  mean_log_ww_value_thres = -4
)
}
\arguments{
\item{calib_data}{The training data object susbetted to before the forecast
date}

\item{delay_thres}{The maximum number of days of delay between the last
wastewater data point and the forecat date, before we would flag a state as
having insufficient wastewater data to inform a forecast. Default is 21}

\item{n_dps_thres}{The threshold number of data points within a single site
within a state before we would flag the state as having insufficient
wastewater data to inform a forecast. Default is 5}

\item{prop_below_lod_thres}{The threshold proportion of wastewater data
points that can be below the LOD. If greater than this proportion of points
are below the LOD, we flag the state as having insufficient wastewater data.
Default is 0.5}

\item{sd_thres}{The minimum standard deviation between wastewater data points
within a site. This is intended to catch when a site reports all the same
values. Default is 0.1}
}
\value{
a dataframe with 5 True or False flags corresponding to wastewater
data presence and quality
}
\description{
Get some basic flags on the wastewater data, for internal use and for
reporting out in our metadata
}
