library(brms)

h_dat <- wwinference::hosp_data |>
  dplyr::transmute(
    time = as.numeric(
      .data$date - as.Date("2023-11-01")
    ) +
      1L,
    hosp = .data$daily_hosp_admits
  ) |>
  dplyr::filter(.data$time > 0)


h_fit <- brms::brm(
  formula = hosp ~ time,
  data = h_dat,
  family = brms::negbinomial(),
  prior = c(
    brms::prior(
      "normal(0, 0.06)",
      class = "b",
      coef = "time"
    ),
    brms::prior("normal(0, 1)", class = "shape")
  ),
  seed = 123,
  backend = "cmdstanr"
)

dat <- wwinference::ww_data |>
  dplyr::mutate(
    time = as.numeric(
      .data$date - as.Date("2023-10-10")
    ) +
      1L,
    below_lod = .data$log_genome_copies_per_ml <= .data$log_lod,
    conc = ifelse(
      .data$below_lod,
      .data$log_lod,
      .data$log_genome_copies_per_ml
    ),
    cens = ifelse(.data$below_lod, -1, 0),
    lab_site_index = interaction(.data$site, .data$lab)
  ) |>
  dplyr::filter(.data$time > 0, .data$time < 20) |>
  dplyr::select(
    "time",
    "lab_site_index",
    "conc",
    "cens"
  )


ww_formula <- brms::bf(
  conc | cens(cens) ~ time + (time || lab_site_index),
  sigma ~ (1 || lab_site_index)
)
fit <- brms::brm(
  formula = ww_formula,
  data = dat,
  family = brms::brmsfamily(
    "gaussian",
    link = "identity",
    link_sigma = "log"
  ),
  seed = 123,
  backend = "cmdstanr"
)
