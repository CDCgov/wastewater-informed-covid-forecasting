testthat_stan_include <- function() {
  system.file(
    "stan",
    package = "cfaforecastrenewalww"
  )
}

model_file_path_id <- system.file(
  "stan", "renewal_ww_hosp_site_level_inf_dynamics.stan",
  package = "cfaforecastrenewalww"
)

cat("\nsetup.R is compiling the stan model in preparation for testing.\n")

# precompiled site-level infection dynamics model
compiled_site_inf_model <- cmdstanr::cmdstan_model(
  model_file_path_id,
  force_recompile = TRUE,
  compile_standalone = TRUE,
  include = testthat_stan_include()
)

# Make sure hosp-only model is compiled if it isn't yet
model_file_path_ho <- system.file(
  "stan", "renewal_ww_hosp.stan",
  package = "cfaforecastrenewalww"
)

compiled_hosp_only_model <- ww_model(model_file_path_ho)
