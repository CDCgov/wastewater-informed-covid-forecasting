% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{get_plot_draws}
\alias{get_plot_draws}
\title{Plot of draws for a single forecast date, with optional groupings}
\usage{
get_plot_draws(
  df,
  y,
  figure_file_path,
  log_scale = FALSE,
  grouping_var = NA,
  from_full_df = FALSE,
  days_pre_forecast_date_plot = 90,
  write_files = TRUE,
  show_calibration_data = FALSE,
  show_median = FALSE
)
}
\arguments{
\item{df}{dataframe either containing filepaths to load in model draws or
dataframe of model draws with data. Expected columns are:
name, t, value, draw, include_ww, forecast_date, hosp_reporting_delay,
location, date, daily_hosp_admits, daily_hosp_admits_for_eval, obs_data,
period, model_type}

\item{y}{outcome variable for the y-axis of plots, options are 'pred_hosp'
and 'pred_ww'}

\item{figure_file_path}{directory to save the figure}

\item{log_scale}{whether or not to transform the y-axis to log scale,
default is FALSE}

\item{grouping_var}{what to group by, default is NA, options are 'model_type'
and 'location'}

\item{from_full_df}{if TRUE, df is a draws dataframe, if FALSE, df is a data
frame of filepaths}

\item{days_pre_forecast_date_plot}{how many days to show the calibration data
default is 90}

\item{write_files}{whether or not to write files to the path, default is TRUE}

\item{show_calibration_data}{whether or not to show the data the model was
calibrated to, which might be different from the eval data. Default is false}

\item{show_median}{whether or not to show the median across all draws, default
is false}
}
\value{
plot object
}
\description{
Plot of draws for a single forecast date, with optional groupings
}
