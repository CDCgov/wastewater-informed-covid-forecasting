% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{get_rt_boxplot_across_states}
\alias{get_rt_boxplot_across_states}
\title{Get R(t) box plot across states}
\usage{
get_rt_boxplot_across_states(
  df_of_filepaths,
  figure_file_path,
  write_files = TRUE
)
}
\arguments{
\item{df_of_filepaths}{dataframe containing the filepaths needed to load
in the R(t) trajectories for each state}

\item{figure_file_path}{directory to save the figure}

\item{write_files}{if TRUE then write files to specified location, if FALSE
then don't}
}
\value{
plot object of R(t) as a horizontal box plot for each state
}
\description{
Get R(t) box plot across states
}
