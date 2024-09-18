stan_program <- "
data {
  int<lower=0> n;
  vector[n] y;
}
parameters {
  real mu;
}
model {
  y ~ normal(mu, 1);
}
"

stanfile <- cmdstanr::write_stan_file(stan_program)
stanmodel <- cmdstanr::cmdstan_model(stanfile)


test_that(paste0(
  "sample_model gives an error if passed a valid ",
  "CmdStanModel but not the needed data to sample from it"
), {
  test <- quiet(suppressWarnings(wweval::sample_model(
    standata = list(),
    stan_model_path = stanfile,
    stan_models_dir = NULL,
    init_lists = NULL
  )))

  expect_equal(
    test$error[1],
    "Fitting failed. Unable to print."
  )
})

test_that(paste0(
  "wweval::sample_model works if passed a valid model"
), {
  test_no_error <- quiet(wweval::sample_model(
    list(
      n = 5,
      y = c(2.52, 2.1, 1.7, 1.2, 1.8)
    ),
    stanfile,
    NULL,
    NULL
  ))
  expect_equal(
    test_no_error$error[1],
    NULL
  )
  expect_equal(
    names(test_no_error),
    c("draws", "diagnostics", "summary_diagnostics")
  )
})
