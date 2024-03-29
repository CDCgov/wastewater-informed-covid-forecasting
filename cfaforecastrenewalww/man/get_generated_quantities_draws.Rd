% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_model_outputs.R
\name{get_generated_quantities_draws}
\alias{get_generated_quantities_draws}
\title{Get generated quantities draws}
\usage{
get_generated_quantities_draws(all_draws, model_type, n_draws = 100)
}
\arguments{
\item{train_data}{}
}
\value{
A long dataframe with model draws from pred_hosp, pred_ww, R(t) and
p_hosp(t), where all have one value per time point and draw.
The resulting dataframe has the following columns:
name, t, value, draw
}
\description{
This function takes the raw stan output from the aggregated model and
arranges them in a tidy data format, with time as an indexing variable.
}
