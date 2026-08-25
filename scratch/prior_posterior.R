devtools::load_all('wweval')

params <- RcppTOML::parseTOML("input/params.toml") |>
  purrr::list_flatten(name_spec = "{inner}")

loader <- get_object_loader(
  "WA",
  "2023-10-16",
  "status_quo",
  "output/eval_latest/raw_output"
)

example_fit <- loader("ww_fit_obj")

to_plot <- list(
  list(
    "name" = "log10 genomes shed per infected individual",
    "symbol" = "$\\log_{10}(G)$",
    "stan_name" = "log10_g",
    "prior_pdf" = distributional::dist_normal(
      mean = params$log10_g_prior_mean,
      sd = params$log10_g_prior_sd
    )
  ),
  list(
    "name" = "infection feedback strength",
    "symbol" = "$\\gamma$",
    "stan_name" = "infection_feedback",
    "prior_pdf" = distributional::dist_lognormal(
      mu = params$infection_feedback_prior_logmean,
      sigma = params$infection_feedback_prior_logsd
    )
  ),
  list(
    name = "Observation noise sd variability by site",
    symbol = "$\\sigma_{\\log \\sigma_{c}}$",
    stan_name = "sd_log_sigma_ww_site",
    prior_pdf = distributional::dist_normal(
      dnorm,
      mean = params$sd_log_sigma_ww_site_prior_mode,
      sd = params$sd_log_sigma_ww_site_prior_sd
    )
  ),
  list(
    "name" = "sd of log site-lab multipliers",
    "symbol" = "$\\sigma_m$",
    "stan_name" = "ww_site_mod_sd",
    prior_pdf = distributional::dist_normal(
      mean = 0,
      sd = params$ww_site_mod_sd_sd
    )
  ),
  list(
    "name" = "s.d. of the AR(1) process on the R(t) differences",
    "symbol" = "$\\sigma_r$",
    "stan_name" = "eta_sd",
    prior_pdf = distributional::dist_normal(
      mean = params$eta_sd_mean,
      sd = params$eta_sd_sd
    )
  )
)

needed_params <- purrr::map_chr(to_plot, \(x) x$stan_name)

draws <- tidybayes::spread_draws(
  example_fit$fit$result,
  log10_g,
  infection_feedback,
  sd_log_sigma_ww_site,
  ww_site_mod_sd,
  eta_sd
)


plot <- ggplot() +
  ggdist::stat_slab(
    mapping = aes(
      xdist = distributional::dist_normal(
        mean = params$log10_g_prior_mean,
        sd = params$log10_g_prior_sd
      )
    ),
    color = "black",
    alpha = 0.25,
    linetype = "dashed",
    fill = "black"
  ) +
  ggdist::stat_slabinterval(
    mapping = aes(x = .data$log10_g),
    data = draws,
    density = "histogram",
    fill = "darkblue"
  ) +
  get_plot_theme()

draws <- tidybayes::spread_draws(
  example_fit$fit$result,
  log10_g,
  infection_feedback,
  sd_log_sigma_ww_site,
  ww_site_mod_sd,
  eta_sd
)

plot <- ggplot() +
  ggdist::stat_slab(
    mapping = aes(
      xdist = distributional::dist_lognormal(
        mu = params$infection_feedback_prior_logmean,
        sigma = params$infection_feedback_prior_logsd
      )
    ),
    color = "black",
    alpha = 0.25,
    linetype = "dashed",
    fill = "black"
  ) +
  ggdist::stat_slabinterval(
    mapping = aes(x = .data$infection_feedback),
    data = draws,
    density = "histogram",
    fill = "darkblue"
  ) +
  get_plot_theme()


ggsave("~/example_plot_inf_feedback.png", plot)

plot_prior_posterior <- function(draws_wide, param_name, dist) {
  p <- ggplot() +
    ggdist::stat_slab(
      mapping = aes(xdist = dist),
      color = "black",
      alpha = 0.25,
      linetype = "dashed",
      fill = "black"
    ) +
    ggdist::stat_slabinterval(
      mapping = aes(x = .data[[param_name]]),
      data = draws,
      density = "histogram",
      fill = "darkblue"
    ) +
    get_plot_theme()

  return(p)
}

plots <- purrr::map(to_plot, \(x) {
  plot_prior_posterior(draws, x$stan_name, x$prior_pdf)
})
