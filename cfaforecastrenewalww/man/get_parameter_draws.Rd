% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_model_outputs.R
\name{get_parameter_draws}
\alias{get_parameter_draws}
\title{Get posterior parameter draws and summary stats}
\usage{
get_parameter_draws(model_draws, pars, train_data)
}
\arguments{
\item{train_data}{the data that went into stan (for a single forecast date,
location, model type, and data scenario)}

\item{stan_output_draws}{the wide format output directly from stan}
}
\value{
dataframe with the following parameters: phi_h, phi_w, eta_sd, G,
p_hosp, i0, initial_growth, dur_shed, viral_peak, t_peak, with n_draws from
the posterior alongside median and 50% and 95% credible intervals
}
\description{
Reads in model_draws, parameters, and training data and gets summary
stats on the model parameters, alongside the metadata associated with the model
run (comes from train data)
}
