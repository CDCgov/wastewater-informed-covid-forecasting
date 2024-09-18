# Script to save the generation interval to be passed in

probability_mass <- cfaforecastrenewalww::simulate_double_censored_pmf(
  max = 15, meanlog = 0.92877, sdlog = 0.526, fun_dist = rlnorm, n = 5e6
) |> cfaforecastrenewalww::drop_first_element_and_renormalize()
# Drop the first bin from the discretized pmf. This bin has the (jittered)
# density in (0, 1]. The renewal equation assumes that there is no same-day
# transmission, so the GI must be left-truncated at 1. Perform this
# truncation by removing the first indexed point and renormalizing.

generation_interval <- data.frame(
  timepoint =
    seq(
      from = 1,
      to = length(probability_mass)
    ),
  probability_mass = probability_mass
)


wwinference::create_dir(file.path("repo_data", "saved_pmfs"))
write.csv(generation_interval, file.path(
  "repo_data", "saved_pmfs",
  glue::glue("generation_interval.csv")
),
row.names = FALSE
)
