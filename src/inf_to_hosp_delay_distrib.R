# script to generate infection to hospital admissions delay distribution

inc <- cfaforecastrenewalww::make_incubation_period_pmf(
  backward_scale = 3.60, backward_shape = 1.5,
  r = 0.15
)
sym_to_hosp <- cfaforecastrenewalww::make_hospital_onset_delay_pmf(
  neg_binom_mu = 6.98665,
  neg_binom_size = 2.490848
)

probability_mass <- cfaforecastrenewalww::make_reporting_delay_pmf(
  inc, sym_to_hosp
)

inf_to_hosp <- data.frame(
  timepoint = seq(from = 0, to = length(probability_mass) - 1),
  probability_mass = probability_mass
)



cfaforecastrenewalww::create_dir(file.path("repo_data", "saved_pmfs"))
write.csv(inf_to_hosp, file.path(
  "repo_data", "saved_pmfs",
  glue::glue("inf_to_hosp.csv")
),
row.names = FALSE
)
