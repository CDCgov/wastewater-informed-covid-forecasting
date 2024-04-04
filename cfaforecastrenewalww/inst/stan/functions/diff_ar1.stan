/**
  * Assembles a differenced AR(1) process out of its non-centered, non-scaled
  * components.
  * @param x0 The initial value, or intercept.
  * @param ar The autocorrelation coefficient.
  * @param sd The standard deviation for the white noise/IID normal terms.
  * @param z Vector of IID Normal(0,1) variables, the white noise/increment terms.
  *
  * @return A vector representing the values of the differenced AR(1) process.
  */
vector diff_ar1(real x0, real ar, real sd, vector z) {
  int n = num_elements(z) + 1;
  vector[n] diffs;

  diffs[1] = x0;
  diffs[2:n] = ar1(rep_vector(0.0, n-1), ar, sd, z, 0);
  return cumulative_sum(diffs);
}
