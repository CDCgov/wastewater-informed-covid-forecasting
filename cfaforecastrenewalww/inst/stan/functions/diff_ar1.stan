/**
  * Assembles a differenced AR(1) process out of its non-centered, non-scaled
  * components.
  * @param x0 The initial value, or intercept.
  * @param ar The autocorrelation coefficient.
  * @param sd The standard deviation for the white noise/IID normal terms.
  * @param z Vector of IID Normal(0,1) variables, the white noise/increment terms.
  * @param is_stat int serving as logical. Passed as the `is_stat`
  * argument to function [ar1()]. Should the underlying AR(1) process on
  * the first differences be initialized at stationary variance (1)
  * or not (0)? Not valid for |ar| >= 1.
  *
  * @return A vector representing the values of the differenced
  * AR(1) process.
  */
vector diff_ar1(real x0, real ar, real sd, vector z, int is_stat) {
  int n = num_elements(z) + 1;
  vector[n] diffs;

  diffs[1] = x0;
  diffs[2:n] = ar1(rep_vector(0.0, n-1), ar, sd, z, is_stat);
  return cumulative_sum(diffs);
}
