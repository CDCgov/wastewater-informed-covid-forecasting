devtools::load_all('wweval')
targets::tar_load(params)
targets::tar_load(parameter_draws)


prior_posterior_plot <- plot_prior_posterior(
  parameter_draws,
  params
)

cowplot::save_plot(
  "output/prior_posterior.jpg",
  prior_posterior_plot,
  base_width = 15,
  base_aspect_ratio = 2
)
