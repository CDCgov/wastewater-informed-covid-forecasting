/**
  * Assembles daily hospitalization probability vector from a weekly random walk.
  * Uses non-centered, non-scaled differences, initial value, and SD for the walk.
  * Multiplies by matrix to transform to daily.
  *
  * @param p_hosp_m matrix to handle conversion from weekly to daily
  * @param p_hosp_int intercept/first value, on unconstrained scale
  * @param p_hosp_w_sd, scaling factor for/SD of random walk
  * @param autoreg_p_hosp, autoreg parameter for IHR
  * @param p_hosp_w weekly deviations of the random walk, on unconstrained scale
  * @param tot_weeks, number of total weeks in calibration and forecast period
  * @param is_stat, whether or not AR(1) process starts at stationarity
  * @return A vector, daily probabilities of hospitalization
  */
vector assemble_p_hosp(matrix p_hosp_m, real p_hosp_mean, real p_hosp_w_sd,
                      real autoreg_p_hosp,
                      vector p_hosp_w, int tot_weeks, int is_stat) {
  vector[dims(p_hosp_m)[1]] p_hosp;
  vector[tot_weeks] p_hosp_in_weeks;

  p_hosp_in_weeks = ar1(rep_vector(p_hosp_mean, tot_weeks),
                    autoreg_p_hosp,
                    p_hosp_w_sd,
                    p_hosp_w,
                    is_stat);
  p_hosp = p_hosp_m * p_hosp_in_weeks;
  p_hosp = inv_logit(p_hosp);
  return p_hosp;
}
