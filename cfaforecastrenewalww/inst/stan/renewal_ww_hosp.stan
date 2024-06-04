functions {
#include functions/ar1.stan
#include functions/diff_ar1.stan
#include functions/convolve.stan
#include functions/infections.stan
#include functions/hospitalization.stan
#include functions/observation_model.stan
#include functions/utils.stan
}
// end functions

// The fixed input data
data {
  int<lower=1> gt_max;
  int<lower=1> hosp_delay_max;
  vector<lower=0,upper=1>[hosp_delay_max] inf_to_hosp;
  int<lower=0> dur_inf; // duration people are infectious (number of days)
  real<lower=0> mwpd; // mL of WW produced per person per day
  int<lower=1> if_l; // length of infection feedback pmf
  vector[if_l] infection_feedback_pmf; // infection feedback pmf
  int<lower=0> ot; // maximum time index for the hospital admissions (max number of days we could have observations)
  int<lower=0> oht; // number of days that we have hospital admissions observations
  int<lower=0> owt; // number of days of observed WW (should be roughly ot/7)
  int<lower=0> uot; // unobserved time before we observe hospital admissions/ WW
  int<lower=0> ht; // horizon time (nowcast + forecast time)
  int<lower=0> n_weeks; // number of weeks for weekly random walk on R(t)
  matrix<lower=0>[ot + ht, n_weeks] ind_m; // matrix needed to transform R(t) from weekly to daily
  int<lower=0> tot_weeks; // number of weeks for the weekly random walk on IHR (includes unobserved time)
  matrix<lower=0>[uot + ot + ht, tot_weeks] p_hosp_m ; // matrix needed to convert p_hosp RW from weekly to daily
  vector<lower=0,upper=1>[gt_max] generation_interval; // generation interval distribution
  real<lower=1e-20> n; // population size
  array[owt] int<lower=1,upper=ot + ht> ww_sampled_times; // the days on which WW is sampled relative
                                   // to the days with which hospital admissions observed
  array[oht] int<lower=1, upper=ot> hosp_times; // the days on which hospital admissions are observed
  array[oht] int<lower=0> hosp; // observed hospital admissions
  array[ot + ht] int<lower=1,upper=7> day_of_week; // integer vector with 1-7 corresponding to the weekday
  vector[owt] log_conc; // log(genome copies/mL)
  int<lower=0,upper=1> compute_likelihood; // 1= use data to compute likelihood
  int<lower=0,upper=1> include_ww; // 1= include wastewater data in likelihood calculation
  int<lower=0,upper=1> include_hosp; // 1 = fit to hosp, 0 = only fit wastewater model

  // Priors
  vector[6] viral_shedding_pars; // tpeak, viral peak, shedding duration mean and sd
  real<lower=0> autoreg_rt_a;
  real<lower=0> autoreg_rt_b;
  real<lower=0> autoreg_p_hosp_a;
  real<lower=0> autoreg_p_hosp_b;
  real inv_sqrt_phi_prior_mean;
  real<lower=0> inv_sqrt_phi_prior_sd;
  real r_prior_mean;
  real<lower=0> r_prior_sd;
  real log10_g_prior_mean;
  real<lower=0> log10_g_prior_sd;
  real<lower=0> i0_over_n_prior_a;
  real<lower=0> i0_over_n_prior_b;
  real wday_effect_prior_mean;
  real<lower=0> wday_effect_prior_sd;
  real initial_growth_prior_mean;
  real<lower=0> initial_growth_prior_sd;
  real sigma_ww_prior_mean;
  real<lower=0> eta_sd_sd;
  real p_hosp_prior_mean;
  real<lower=0> p_hosp_sd_logit;
  real<lower=0> p_hosp_w_sd_sd;
  real inf_feedback_prior_logmean;
  real<lower=0> inf_feedback_prior_logsd;
}

transformed data {
  // viral shedding parameters
  real t_peak_mean = viral_shedding_pars[1];
  real<lower=0> t_peak_sd = viral_shedding_pars[2];
  real viral_peak_mean = viral_shedding_pars[3];
  real<lower=0> viral_peak_sd = viral_shedding_pars[4];
  real dur_shed_mean = viral_shedding_pars[5];
  real<lower=0> dur_shed_sd = viral_shedding_pars[6];
  // natural scale -> lognormal parameters
  // https://en.wikipedia.org/wiki/Log-normal_distribution
  real r_logmean = convert_to_logmean(r_prior_mean, r_prior_sd);
  real<lower=0> r_logsd = convert_to_logsd(r_prior_mean, r_prior_sd);
  // reversed generation interval
  vector<lower=0,upper=1>[gt_max] gt_rev_pmf = reverse(generation_interval);
  vector<lower=0,upper=1>[if_l] infection_feedback_rev_pmf = reverse(infection_feedback_pmf);
}

// The parameters accepted by the model.
parameters {
  vector[n_weeks-1] w; // weekly random walk
  real<lower=0> eta_sd; // step size of random walk
  real<lower = 0, upper=1> autoreg_rt;// coefficient on AR process in R(t)
  array [(include_ww==1) ? 1 : 0] real<lower=0, upper=1> autoreg_p_hosp;
  real<upper=log(10)> log_r; // baseline reproduction number estimate (log)
  real<lower=0,upper=1> i0_over_n; // Per capita incident infections
  // on day -uot before first observation day
  real<lower=-1, upper=1> initial_growth; // initial growth from I0 to first observed time
  real<lower=1/sqrt(5000)> inv_sqrt_phi_h;
  real<lower=0> sigma_ww;
  real p_hosp_mean; // Estimated IHR
  vector[(include_ww==1) ? tot_weeks : 0] p_hosp_w;
  array [(include_ww==1) ? 1 : 0] real <lower=0> p_hosp_w_sd;
  real<lower=0> t_peak; // time to viral load peak in shedding
  real viral_peak; // log10 peak viral load shed /mL
  real<lower=0> dur_shed; // duration of detectable viral shedding
  real log10_g; // log10 of number of genomes per infected individual
  simplex[7] hosp_wday_effect; // day of week reporting effect, sums to 1
  real<lower=0> infection_feedback; // infection feedback

}

transformed parameters {
  vector[ot + uot + ht] new_i; // daily incident infections /n
  vector [(include_ww ==1) ? ot + uot + ht: 1] p_hosp; // probability of hospitalization
  vector[(include_ww == 1) ? tot_weeks-1: 0] p_hosp_in_weeks; // the weekly vector of probability of hospital admissions
  vector[ot + uot + ht] model_hosp; // model estimated hospital admissions
  vector[oht] exp_obs_hosp;  //  expected observed hospital admissions
  vector[ot] exp_obs_hosp_no_wday_effect; // expected observed hospital admissions before weekday effect
  vector[gt_max] s; // viral kinetics trajectory (normalized)
  vector[ot + uot + ht] model_log_v; // model estimated log viral genomes shed per person
  vector[ot+ht] model_log_v_ot; // model estimated log viral genomes shed per person during ot
  vector[owt] exp_obs_log_v; // model estimated log viral genomes shed per person at ww sampled times only
  vector[ot + uot + ht] model_net_i; // number of net infected individuals shedding on each day (sum of individuals in dift stages of infection)
  real<lower=0> phi_h = inv_square(inv_sqrt_phi_h); // previouslt inv_square(inv_sqrt_phi_h)
  vector<lower=0>[ot + ht] unadj_r; // R(t)
  vector<lower=0>[ot + ht] rt; // R(t)
  real<lower=0> i0 = i0_over_n * n; // Absolute initial incident infections
  vector[n_weeks] log_rt_weeks; // log R(t) in weeks for autocorrelated RW


  // AR + RW implementation:
  log_rt_weeks = diff_ar1(log_r, autoreg_rt, eta_sd, w, 0);
  unadj_r = ind_m * log_rt_weeks;
  unadj_r = exp(unadj_r);

  // Expected daily number of new infections (per capita), using EpiNow2 assumptions re pre and post observation time
  // Using pop = 1 so that damping is normalized to per capita
  (new_i, rt) = generate_infections(
    unadj_r,
    uot,
    gt_rev_pmf,
    log(i0_over_n),
    initial_growth,
    ht,
    infection_feedback,
    infection_feedback_rev_pmf);

  // Expected hospitalizations:
  // generates all hospitalizations, across unobserved time, observed time, and forecast time
  if(include_ww==1){
    p_hosp = assemble_p_hosp(p_hosp_m, p_hosp_mean, p_hosp_w_sd[1],
                           autoreg_p_hosp[1], p_hosp_w, tot_weeks, 1);
    model_hosp = convolve_dot_product(p_hosp .* new_i, reverse(inf_to_hosp),
                                    ot + uot + ht);
  }else{
    p_hosp[1] = inv_logit(p_hosp_mean);
    // generates all hospitalizations, across unobserved time, observed time, and forecast time
    model_hosp = convolve_dot_product(p_hosp[1] * new_i, reverse(inf_to_hosp),
                                    ot + uot + ht);
  }



  // just get the expected observed hospitalizations
  exp_obs_hosp_no_wday_effect = model_hosp[uot + 1 : uot + ot];
  // apply the weekday effect so these are distributed with fewer admits on Sat & Sun
  // multiply by n because data must be integer so need this to be in actual numbers not proportions
  exp_obs_hosp = n * day_of_week_effect(exp_obs_hosp_no_wday_effect[hosp_times],
                                        day_of_week[hosp_times], hosp_wday_effect);

  // Expected shed viral genomes:
  // Shedding kinetics trajectory
  s = get_vl_trajectory(t_peak, viral_peak, dur_shed, gt_max);

  // This should also be a convolution of incident infections and shedding kinetics pmf times avg total virus shed
  model_net_i = convolve_dot_product(new_i, reverse(s), uot + ot + ht); // net number of infected individuals
  // log number of viral genomes shed on a given day = net infected individuals * amount shed per individual
  model_log_v = log(10)*log10_g + log(model_net_i + 1e-8); // adding for numerical stability
  // genome copies/mL = genome copies/(person * mL of WW per person day)
  model_log_v_ot = model_log_v[(uot + 1) : (uot + ot + ht)] - log(mwpd);
  exp_obs_log_v = model_log_v_ot[ww_sampled_times];
}

// Prior and sampling distribution
model {
  // priors
  vector[7] effect_mean = rep_vector(wday_effect_prior_mean, 7);
  w ~ std_normal();
  eta_sd ~ normal(0, eta_sd_sd);
  autoreg_rt ~ beta(autoreg_rt_a, autoreg_rt_b);
  autoreg_p_hosp ~ beta(autoreg_p_hosp_a, autoreg_p_hosp_b);
  log_r ~ normal(r_logmean, r_logsd);
  i0_over_n ~ beta(i0_over_n_prior_a, i0_over_n_prior_b);
  initial_growth ~ normal(initial_growth_prior_mean, initial_growth_prior_sd);
  inv_sqrt_phi_h ~ normal(inv_sqrt_phi_prior_mean, inv_sqrt_phi_prior_sd);
  sigma_ww ~ normal(0, sigma_ww_prior_mean);
  log10_g ~ normal(log10_g_prior_mean, log10_g_prior_sd);
  hosp_wday_effect ~ normal(effect_mean, wday_effect_prior_sd);
  p_hosp_mean ~ normal(logit(p_hosp_prior_mean), p_hosp_sd_logit);
  p_hosp_w ~ std_normal();
  p_hosp_w_sd ~ normal(0, p_hosp_w_sd_sd);
  t_peak ~ normal(t_peak_mean, t_peak_sd);
  viral_peak ~ normal(viral_peak_mean, viral_peak_sd);
  dur_shed ~ normal(dur_shed_mean, dur_shed_sd);
  infection_feedback ~ lognormal(inf_feedback_prior_logmean, inf_feedback_prior_logsd);

  // Compute log likelihood
  if (compute_likelihood == 1) {
    if (include_ww == 1) {
      log_conc ~ normal(exp_obs_log_v, sigma_ww);
    }

    if (include_hosp == 1) {
      hosp ~ neg_binomial_2(exp_obs_hosp, phi_h);
    }
  } // end if for computing log likelihood
}

generated quantities {
  array[ot + ht] real pred_hosp;
  array[ot + ht] real pred_new_i;
  array[ot + ht] real pred_conc;
  vector[ot + ht] exp_state_ww_conc = exp(model_log_v_ot); // state mean wastewater concentration
  real<lower=0> g = pow(10, log10_g);

  pred_hosp = neg_binomial_2_rng(n * day_of_week_effect(model_hosp[uot + 1 :
                                                        uot + ot + ht],
                                                        day_of_week,
                                                        hosp_wday_effect),
                                 phi_h);
  pred_new_i = neg_binomial_2_rng(n * new_i[uot + 1 : uot + ot + ht], phi_h);

  pred_conc = normal_rng(model_log_v_ot, sigma_ww);
}
