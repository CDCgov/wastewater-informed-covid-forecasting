// This code was adapted from code written
// (under an MIT license) as part of the `EpiNow2`
// package (https://github.com/epiforecasts/EpiNow2)
vector day_of_week_effect(vector reports, array[] int day_of_week,
                          vector effect) {
  int t = num_elements(reports);
  int wl = num_elements(effect);
  // scale day of week effect
  vector[wl] scaled_effect = wl * effect;
  vector[t] scaled_reports;
  for (s in 1:t) {
    // add reporting effects (adjust for simplex scale)
    scaled_reports[s] = reports[s] * scaled_effect[day_of_week[s]];
  }
  return scaled_reports;
}

vector get_vl_trajectory(real tpeak, real viral_peak,
                         real duration_shedding, int n) {
  vector[n] s;
  real growth = viral_peak / tpeak;
  real wane = viral_peak / (duration_shedding - tpeak);

  for (t in 1 : n) {
    if (t <= tpeak) {
      s[t] = pow(10, growth * t);
    } else {
      s[t] = viral_peak + wane * tpeak - wane * t;
      if (s[t] < 0) {
        s[t] = 0;
      }
      s[t] = pow(10, s[t]);
    }
  }
  s = s / sum(s);
  return s;
}
