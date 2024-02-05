testthat_stan_include <- function() {
  system.file(
    "stan",
    package = "cfaforecastrenewalww"
  )
}

model_file_path <- system.file(
  "stan", "renewal_ww_hosp_site_level_inf_dynamics.stan",
  package = "cfaforecastrenewalww"
)

cat("\nsetup.R is compiling the stan model in preparation for testing.\n")

# precompiled site-level infeciton dynamics model
compiled_site_inf_model <- cmdstanr::cmdstan_model(
  model_file_path,
  force_recompile = TRUE,
  compile_standalone = TRUE,
  include = testthat_stan_include()
)
compiled_site_inf_model$expose_functions()
