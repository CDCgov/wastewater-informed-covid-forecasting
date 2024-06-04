set.seed(1)
toy_data_and_params <- cfaforecastrenewalww::generate_simulated_data()
example_df <- toy_data_and_params$example_df
param_df <- toy_data_and_params$param_df

usethis::use_data(param_df, overwrite = TRUE)
usethis::use_data(example_df, overwrite = TRUE)
