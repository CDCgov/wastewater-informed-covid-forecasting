/**
  * Assembles a biased (GMRF-like) random walk out of its non-centered, non-scaled components.
  * The bias is that (x[i] - x[i - 1]) is positively correlated with (x[i - 1] - x[i - 2]).
  * @param x0 The initial value, or intercept.
  * @param ar The autocorrelation coefficient.
  * @param sd The sd for the white noise/IID normal terms.
  * @param z vector of IID Normal(0,1) variables, the white noise/increment terms.
  * @return A vector, the values of the biased random walk.
  */
vector biased_rw(real x0, real ar, real sd, vector z) {
  int n = num_elements(z) + 1;
  vector[n] x;

  x[1] = x0;
  x[2] = x[1] + sd * z[1];

  for (i in 3:n) {
    x[i] = x[i - 1] + ar * (x[i - 1] - x[i - 2]) + sd * z[i - 1];
  }

  return x;
}
