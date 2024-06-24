//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as the sum over individual iid gammas of
// pop components, where the individual components are gamma distributed
// with mean mu and coefficient of variation cv

// This example will use the normal approximation for the sum of iid gammas

// The input data is a vector 'y' of length 'N'.

functions {
  vector gamma_sum_approx(real cv, vector N, vector noise_noncentered) {
  // sqrt(N) * cv is the standard deviation of the sum of Gamma distributions
  return N + noise_noncentered .* sqrt(N) * cv;
}

}
data {
  int<lower=0> N;
  vector[N] y;
  vector[N] pop_vector;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'cv', the mean and coefficient of variatin
// of the individual components, and `zeta_raw`, the N(0,1) vector use in the
// non-centered parameterization of the normal approximation.
parameters {
  real mu;                        // mean of individual component
  real<lower=0> cv;               // coefficent of variation in individual data
}


// The model to be estimated.
model {
  mu ~ normal(2, 1);
  cv ~ normal( 1, 1);

  y ~ gamma(pop_vector./cv^2, 1 / (mu * (cv^2)));

}
