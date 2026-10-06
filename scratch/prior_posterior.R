devtools::load_all('wweval')
targets::tar_load(params)
targets::tar_load(parameter_draws)

to_plot <- dplyr::bind_rows(
  list(
    "display_name" = "log10 genomes shed per\ninfected individual",
    "symbol" = "$\\log_{10}(G)$",
    "stan_name" = "log10_g",
    "prior_pdf" = distributional::dist_normal(
      mean = params$log10_g_prior_mean,
      sd = params$log10_g_prior_sd
    ),
    "x_transform" = "identity",
    "xlim" = list(NULL)
  ),
  list(
    "display_name" = "infection feedback strength",
    "symbol" = "$\\gamma$",
    "stan_name" = "infection_feedback",
    "prior_pdf" = distributional::dist_lognormal(
      mu = params$infection_feedback_prior_logmean,
      sigma = params$infection_feedback_prior_logsd
    ),
    "x_transform" = "log10",
    "xlim" = list(NULL)
  ),
  list(
    "display_name" = "Observation noise s.d.\nvariability by site",
    "symbol" = "$\\sigma_{\\log \\sigma_{c}}$",
    "stan_name" = "sd_log_sigma_ww_site",
    "prior_pdf" = distributional::dist_normal(
      mean = params$sd_log_sigma_ww_site_prior_mode,
      sd = params$sd_log_sigma_ww_site_prior_sd
    ) |>
      distributional::dist_truncated(lower = 0),
    "x_transform" = "identity",
    "xlim" = list(NULL)
  ),
  list(
    "display_name" = "s.d. of log site-lab\nmultipliers",
    "symbol" = "$\\sigma_m$",
    "stan_name" = "ww_site_mod_sd",
    "prior_pdf" = distributional::dist_normal(
      mean = 0,
      sd = params$ww_site_mod_sd_sd
    ) |>
      distributional::dist_truncated(lower = 0),
    "x_transform" = "identity",
    "xlim" = list(c(0, 1.5))
  ),
  list(
    "display_name" = "s.d. of the AR(1) process\non the R(t) differences",
    "symbol" = "$\\sigma_r$",
    "stan_name" = "eta_sd",
    "prior_pdf" = distributional::dist_normal(
      mean = params$eta_sd_mean,
      sd = params$eta_sd_sd
    ) |>
      distributional::dist_truncated(lower = 0),
    "x_transform" = "identity",
    "xlim" = list(c(0, 0.09))
  )
) |>
  dplyr::distinct() |>
  dplyr::rename(.variable = "stan_name")

prior_posterior_plot <- plot_prior_posterior(
  parameter_draws |> dplyr::filter(model_type == "ww"),
  to_plot
)

cowplot::save_plot(
  "output/prior_posterior.jpg",
  prior_posterior_plot,
  base_width = 15,
  base_aspect_ratio = 2
)
