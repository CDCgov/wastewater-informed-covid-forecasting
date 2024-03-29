% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_output_metadata.R
\name{get_location_notes_table}
\alias{get_location_notes_table}
\title{Get a table that records notes on data characteristics
and model choices for individual locations,
e.g. the absence of recent wastewater observations or
the decision to publish the hospital admissions only
model}
\usage{
get_location_notes_table(
  full_diagnostics_df,
  hosp_only_states,
  exclude_states,
  output_dir = NULL,
  prod_run = FALSE
)
}
\arguments{
\item{full_diagnostics_df}{Data frame of data diagnostics
by locations.}

\item{hosp_only_states}{States for which we chose
to use the hospitalization only model, as a vector
with states coded identically to how they are in
the `location` column of `full_diagnostics_df`}

\item{exclude_states}{States for which we chose to
not submit a forecast for this week, as a vector with states
coded identically to how they are in the `location` columnd
of `full_diagnostics_df`}

\item{output_dir}{Path to a directory in which to save
the metadata table. Only used if `prod_run = TRUE`.
Default `NULL`.}

\item{prod_run}{Is this a run for publication /
production? If so, the output will be saved in an
appropriate location as a .tsv. Boolean, default `FALSE`}
}
\value{
The table of information as a [tibble::tibble()].
}
\description{
Get a table that records notes on data characteristics
and model choices for individual locations,
e.g. the absence of recent wastewater observations or
the decision to publish the hospital admissions only
model
}
