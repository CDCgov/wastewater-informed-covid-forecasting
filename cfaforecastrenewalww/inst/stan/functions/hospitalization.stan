/**
  * Assembles daily hospitalization probability vector from a weekly random walk.
  * Uses non-centered, non-scaled differences, initial value, and SD for the walk.
  * Multiplies by matrix to transform to daily.
  *
  * @param p_hosp_m matrix to handle conversion from weekly to daily
  * @param p_hosp_int intercept/first value, on unconstrained scale
  * @param p_hosp_w_sd, scaling factor for/SD of random walk
  * @param p_hosp_w weekly deviations of the random walk, on unconstrained scale
  * @return A vector, daily probabilities of hospitalization
  */
vector assemble_p_hosp(matrix p_hosp_m, real p_hosp_int, real p_hosp_w_sd, vector p_hosp_w) {
  vector[dims(p_hosp_m)[1]] p_hosp;
  p_hosp = p_hosp_m * append_row(p_hosp_int, p_hosp_w_sd * p_hosp_w);
  p_hosp = inv_logit(p_hosp);
  return p_hosp;
}
