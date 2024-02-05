/**
  * Assembles an AR(1) process out of its non-centered, non-scaled components.
  * @param mu vector of the mean value that the process preserves to
  * @param ac The autocorrelation coefficient.
  * @param sd The sd for the white noise/IID normal terms.
  * @param z vector of IID Normal(0,1) variables, the white noise/increment terms.
  * @return A vector, the values of the mean-preserving AR(1) process:
  * x[1] = x0,
  * x[i > 1] = mu + tvd[[i], where
  * tvd[i] = ac * tvd[i - 1] + sd* z[i]
  * Formally x(t) = mu(t) + delta(t) where
  * delta(t) = psi*delta(t-1) + eta(t)
  */
vector ar1(vector mu, real ac, real sd, vector z) {
  int n = num_elements(z);
  vector[n] x;
  vector[n] tvd;

  tvd[1] = sd * z[1];
  x[1] = mu[1] + tvd[1];

  for (i in 2:n) {
    tvd[i] = ac * tvd[i - 1] + sd * z[i];
    x[i] = mu[i] + tvd[i];
  }

  return x;
}
