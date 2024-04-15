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
  "sample_model errors if given something other ",
  "than a CmdStanModel object as `compiled_model`"
), {
  expect_error(
    sample_model(
      list(),
      c(1, 2, 3),
      NULL
    ),
    "must be a cmdstanr::CmdStanModel object"
  )
})

test_that(paste0(
  "sample_model gives a different error if passed a valid ",
  "CmdStanModel but not the needed data to sample from it"
), {
  expect_error(
    quiet(suppressWarnings(sample_model(
      list(),
      stanmodel,
      NULL
    ))),
    "No chains finished successfully"
  )
})

test_that(paste0(
  "sample_model works if passed a valid model"
), {
  expect_no_error(
    quiet(sample_model(
      list(
        n = 5,
        y = c(2.52, 2.1, 1.7, 1.2, 1.8)
      ),
      stanmodel,
      NULL
    ))
  )
})
