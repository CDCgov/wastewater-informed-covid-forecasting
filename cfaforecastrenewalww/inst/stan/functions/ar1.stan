/**
  * Assembles an AR(1) process out of its non-centered, non-scaled components.
  * @param mu vector of the mean value that the process preserves to
  * @param ac The autocorrelation coefficient.
  * @param sd The sd for the white noise/IID normal terms.
  * @param z vector of IID Normal(0,1) variables, the white noise/increment terms.
  * @param is_stat int serving as logical, should process be initialized at
  * stationary variance (1) or not (0)? Not valid for |ac| >= 1.
  * @return A vector, the values of the mean-preserving AR(1) process:
  * x[1] = x0,
  * x[i > 1] = mu + tvd[[i], where
  * tvd[i] = ac * tvd[i - 1] + sd* z[i]
  * Formally x(t) = mu(t) + delta(t) where
  * delta(t) = psi*delta(t-1) + eta(t)
  */
vector ar1(vector mu, real ac, real sd, vector z, int is_stat) {
  int n = num_elements(z);
  vector[n] eta;
  vector[n] x;
  vector[n] tvd;

  eta = sd * z;
  if(is_stat) {
    real adj;
    if (ac >= 1.0) {
      reject("AR(1) process is not stationary if ac >= 1.");
    }
    adj = 1.0 / sqrt(1.0 - ac^2);
    eta[1] = adj * eta[1];
  }

  tvd[1] = eta[1];
  for (t in 2:n) {
    tvd[t] = ac * tvd[t - 1] + eta[t];
  }

  x = mu + tvd;
  return x;
}
