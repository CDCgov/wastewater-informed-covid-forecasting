// This code was adapted from code written
// (under an MIT license) as part of the `EpiSewer`
// package (https://github.com/adrian-lison/EpiSewer)
// Non-centered paramaterization of a normal approximation for the
// sum of N i.i.d. Gamma distributed RVs with mean 1 and a specified cv
vector gamma_sum_approx(real cv, vector N, vector noise_noncentered) {
  // sqrt(N) * cv is the standard deviation of the sum of Gamma distributions
  return N + noise_noncentered .* sqrt(N) * cv;
}
