// This code was adapted from code written
// (under an MIT license) as part of the `EpiNow2`
// package (https://github.com/epiforecasts/EpiNow2)
// calculate infectiousness (weighted sum of the generation interval and incident infections)
real update_infectiousness(vector infections, vector gt_rev_pmf,
                           int seeding_time, int index) {
  int gt_max = num_elements(gt_rev_pmf);
  // work out where to start the convolution of past infections with the
  // generation time distribution: (current_time - maximal generation time) if
  // that is >= 1, otherwise 1 (how far back to add infectiousness from)
  int inf_start = max(1, index + seeding_time - gt_max);
  // work out where to end the convolution: (current_time - 1)
  int inf_end = index + seeding_time - 1;
  // number of indices of the generation time to sum over (inf_end - inf_start + 1)
  // Either go all the way back or just go to where we start seeing infections
  int pmf_accessed = min(gt_max, index + seeding_time - 1);
  // calculate the elements of the convolution
  real new_inf = dot_product(infections[inf_start : inf_end],
                             tail(gt_rev_pmf, pmf_accessed)
                             // Last elements of reversed generation interval = first elemenets of generation interval!
                             );
  return new_inf;
}

/**
  * Computes the number of infections over time by updating the reproduction
  * number based on the effective infectiousness from previous days and
  * feedback from incidence data. Adapted from the EpiNow2 package.
  *
  * @param obs_r A vector of length `ot` representing the observed reproduction
  * numbers over a certain period.
  *
  * @param uot An integer representing the number of unobserved time steps
  * prior to the first observed value in `obs_r`.
  *
  * @param gt_rev_pmf A vector representing the reversed generation time
  * probability mass function.
  *
  * @param initial_infections An array of real numbers representing the initial
  * number of infections (in log scale) at the start of the unobserved period.
  *
  * @param initial_growth A real number representing the initial
  * growth rate of infections (in log scale) over the unobserved period.
  *
  * @param ht An integer representing the time horizon for the historical
  * tracking of infections.
  *
  * @param infection_feedback An optional array of real numbers providing
  * feedback to adjust the reproduction number based on recent incidence data.
  *
  * @param infection_feedback_pmf_rev A vector representing the
  * reversed probability mass function for the infection feedback.
  *
  * @return A tuple containing a vector of length `t` representing the number
  * of infections for each time step, combining both unobserved and observed
  * periods and a vector the same length as the input obs_r representing the
  * effective reproduction number at each time step.
  * @author Sam Abbott
  */
tuple(vector, vector) generate_infections(vector obs_r, int uot, vector gt_rev_pmf,
  real initial_infections, real initial_growth,
  int ht,  real infection_feedback, vector infection_feedback_pmf_rev
) {
  // time indices and storage
  int at = num_elements(obs_r); // all time with R(t), including horizon time
  int t = at + uot; // uot + ot + Ht
  vector[at] rt = obs_r;
  vector[t] infections = rep_vector(0, t);
  real infectiousness;
  real infection_feedback_weighting;
  tuple(vector[t], vector[at]) output;

  // Initialise infections using daily growth
  infections[1] = initial_infections;
  if (uot > 1) {
    vector[uot-1] growth = rep_vector(initial_growth, uot-1);
    infections[2:uot] = initial_infections + cumulative_sum(growth);
  }
  infections[1:uot] = exp(infections[1:uot]);
  // iteratively update infections
  for (s in 1:at) {
    infectiousness = update_infectiousness(infections, gt_rev_pmf, uot, s);
      infection_feedback_weighting = update_infectiousness(
        infections, infection_feedback_pmf_rev, uot, s
      );
      rt[s] = exp(log(rt[s]) - infection_feedback .* infection_feedback_weighting);
    infections[s + uot] = rt[s] * infectiousness;
  }

  // Assign tuple output
  output.1 = infections;
  output.2 = rt;
  return(output);
}
