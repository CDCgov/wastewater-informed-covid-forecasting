% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pre_processing.R
\name{get_hosp_reporting_delay}
\alias{get_hosp_reporting_delay}
\title{Get the hospital reporting delay}
\usage{
get_hosp_reporting_delay(forecast_date)
}
\arguments{
\item{forecast_date}{The date the forecast is made}
}
\value{
hosp_reporting delay The delay from forecast date to last hospital
admission date
}
\description{
This is purely based on our knowledge of the NHSN data reporting, as we
pre-specify before loading the most recent data..
}
