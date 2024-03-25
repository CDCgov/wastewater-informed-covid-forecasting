# Plots

#' Plot the WW and hospitalization data together
#'
#' @param comb dataframe for a single location to plot
#' @param figure_file_path higher level directory where data from this pipeline
#'  will be saved
#' @param days_to_show_calib_data number of days before last hospital admissions
#' day to put on plot, default is 90
#' @param write_files TRUE if write to file location
#' @return a plot for a single location the scaled WW versus hospital admissions
#' @export
#'
#' @examples
plot_combined_data <- function(comb, figure_file_path,
                               days_to_show_calib_data = 90,
                               write_files = TRUE, ...) {
  selected_location <- comb %>%
    pull(location) %>%
    unique()
  if (length(selected_location) != 1) {
    print("Training data consists of more than one location")
  }

  forecast_date <- comb %>%
    pull(forecast_date) %>%
    unique()

  include_ww <- comb %>%
    pull(include_ww) %>%
    unique()

  last_hosp_data_date <- max(comb$date[!is.na(comb$daily_hosp_admits)])
  last_ww_data_date <- max(
    comb$date[comb$period != "forecast" & !is.na(comb$ww)]
  )
  coeff <- (
    median(comb$daily_hosp_admits_for_eval, na.rm = TRUE) /
      median(comb$ww, na.rm = TRUE)
  )
  if (is.na(coeff)) {
    coeff <- 1
  }


  p <- comb %>%
    filter(date >= last_hosp_data_date - days(days_to_show_calib_data)) %>%
    ggplot() +
    geom_bar(
      aes(x = date, y = daily_hosp_admits_for_eval, fill = period),
      stat = "identity", position = position_dodge(), alpha = 0.1
    ) +
    geom_point(
      data = comb %>% filter(
        period != "forecast",
        date >= last_hosp_data_date - days(days_to_show_calib_data)
      ),
      aes(x = date, y = ww * coeff), color = "black", fill = "black",
      shape = 24
    ) +
    geom_point(
      data = comb %>% filter(period == "forecast"),
      aes(x = date, y = ww * coeff), color = "black", fill = "white",
      shape = 24, stroke = 1
    ) +
    geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
    geom_vline(aes(xintercept = last_hosp_data_date - days(11)),
      color = "purple4",
      linetype = "dashed"
    ) +
    geom_vline(aes(xintercept = last_ww_data_date - days(5)),
      color = "darkgreen",
      linetype = "dashed"
    ) +
    scale_y_continuous(
      # Features of the first axis
      name = "Daily hospital admissions",

      # Add a second axis and specify its features
      sec.axis = sec_axis(
        trans = ~ . / coeff,
        name = "Genome copies per mL"
      )
    ) +
    xlab("") +
    ylab("Hospital admissions") +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 10, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 9,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    ggtitle(glue::glue(
      "Hospital admissions vs WW concentration in ",
      "{selected_location} as of {forecast_date}"
    ))
  p

  model_type <- ifelse(include_ww == 1,
    "state-level aggregated wastewater",
    "hospital admissions only"
  )

  if (isTRUE(write_files)) {
    # Helper function that checks if the path already exists and creates it
    # if not
    full_file_path <- file.path(
      figure_file_path, selected_location
    )
    create_dir(full_file_path)

    ggsave(
      file.path(
        full_file_path,
        "observed_hosp_and_agg_WW.png"
      ),
      plot = p,
      width = 8,
      height = 5,
      bg = "white"
    )
  }



  return(p)
}


#' Plot of draws for a single forecast date, with optional groupings
#'
#' @param df dataframe either containing filepaths to load in model draws or
#' dataframe of model draws with data. Expected columns are:
#' name, t, value, draw, include_ww, forecast_date, hosp_reporting_delay,
#' location, date, daily_hosp_admits, daily_hosp_admits_for_eval, obs_data,
#' period, model_type
#' @param y outcome variable for the y-axis of plots, options are 'pred_hosp'
#' and 'pred_ww'
#' @param figure_file_path directory to save the figure
#' @param log_scale whether or not to transform the y-axis to log scale,
#' default is FALSE
#' @param grouping_var what to group by, default is NA, options are 'model_type'
#' and 'location'
#' @param from_full_df if TRUE, df is a draws dataframe, if FALSE, df is a data
#' frame of filepaths
#' @param days_pre_forecast_date_plot how many days to show the calibration data
#' default is 90
#' @param write_files whether or not to write files to the path, default is TRUE
#' @param show_calibration_data whether or not to show the data the model was
#' calibrated to, which might be different from the eval data. Default is false
#' @param show_median whether or not to show the median across all draws, default
#' is false
#'
#' @return plot object
#' @export
#'
#' @examples
get_plot_draws <- function(df, y,
                           figure_file_path,
                           log_scale = FALSE,
                           grouping_var = NA,
                           from_full_df = FALSE,
                           days_pre_forecast_date_plot = 90,
                           write_files = TRUE,
                           show_calibration_data = FALSE,
                           show_median = FALSE) {
  # Need to be able to load in multiple model runs and combine
  if (isFALSE(from_full_df)) { # then load in the data
    for (i in seq_len(nrow(df))) {
      model_draws_i <- arrow::read_parquet(
        file =
          df$model_draws_file_path[i]
      )
      model_draws_i <- model_draws_i %>%
        filter(name == {{ y }}) %>%
        select(
          name, t, value, draw, include_ww, forecast_date, hosp_reporting_delay,
          location, date, daily_hosp_admits, daily_hosp_admits_for_eval, obs_data,
          period, model_type
        )

      if (i == 1) {
        model_draws <- model_draws_i
      } else {
        model_draws <- rbind(model_draws, model_draws_i)
      }
    }
  } else {
    model_draws <- df %>%
      filter(name == {{ y }}) %>%
      select(
        name, t, value, draw, include_ww, forecast_date, hosp_reporting_delay,
        location, date, daily_hosp_admits, daily_hosp_admits_for_eval, obs_data,
        period, model_type
      )
  }


  # check if we need to subset the model draws for plotting
  if (length(unique(model_draws$draw)) > 500) {
    model_draws <- model_draws %>%
      filter(draw %in% sample(1:max(model_draws$draw), 100))
  }

  y_draws <- model_draws


  # Get plot metadata
  plot_metadata <- get_plot_metadata(y_draws, y)
  locations <- plot_metadata$location
  include_ww <- plot_metadata$include_ww
  forecast_date <- plot_metadata$forecast_date
  hosp_reporting_delay <- plot_metadata$hosp_reporting_delay
  model_type <- plot_metadata$model_type
  model_file_name <- plot_metadata$model_file_name
  plot_color <- plot_metadata$plot_color
  plot_shape <- ifelse(y == "pred_ww", 24, 21)
  y_label <- plot_metadata$y_label
  title <- plot_metadata$title

  set_color <- c(
    "state-level aggregated wastewater" = "darkred",
    "hospital admissions only" = "purple4",
    "site-level observation error" = "darkorange4",
    "site-level time-varying concentration" = "darkmagenta",
    "site-level infection dynamics" = "cornflowerblue"
  )


  y_draws <- y_draws %>%
    filter(date >= ymd(forecast_date - days(days_pre_forecast_date_plot))) %>%
    group_by(hosp_reporting_delay) %>%
    mutate(
      last_hosp_data_date = max(y_draws$date[!is.na(y_draws$daily_hosp_admits)])
    ) %>%
    ungroup()
  y_median <- y_draws %>%
    group_by(date, location, model_type) %>%
    summarise(median = quantile(value, 0.5, na.rm = TRUE))
  y_draws <- y_draws %>% left_join(y_median, by = c(
    "date",
    "location", "model_type"
  ))


  if (show_calibration_data == TRUE) {
    # Temporarily replace the observed data column with the data the model was
    # calibrated to not evaluated against
    y_draws2 <- y_draws %>% mutate(
      obs_data = case_when(
        (name == "pred_hosp" & period == "calibration") ~ daily_hosp_admits,
        (name == "pred_hosp_per_100k" & period == "calibration") ~
          daily_hosp_admits * 1e5 / pop,
        (name == "pred_hosp_per_100k" & period != "calibration") ~
          daily_hosp_admits_for_eval * 1e5 / pop,
        TRUE ~ obs_data
      )
    )
  }

  last_obs_data <- y_draws %>%
    filter(
      !is.na(obs_data),
      date == max(date)
    ) %>%
    distinct(obs_data) %>%
    pull()
  max_obs_data <- max(y_draws$obs_data, na.rm = TRUE)
  y_axis_lim <- max(3 * last_obs_data, 1.5 * max_obs_data)
  # Make a plot with the draws and the median
  plot <- ggplot() +
    geom_line(
      data = y_draws %>% filter(period == "calibration"),
      aes(
        x = date, y = value,
        group = draw,
        color = model_type
      ),
      size = 0.1, alpha = 0.1
    ) +
    geom_line(
      data = y_draws %>% filter(period != "forecast"),
      aes(
        x = date, y = value,
        group = draw,
        color = model_type
      ),
      size = 0.1, alpha = 0.1
    ) +
    geom_line(
      data = y_draws,
      aes(
        x = date, y = value,
        group = draw,
        color = model_type
      ),
      size = 0.1, alpha = 0.1
    ) +
    geom_point(
      data = y_draws %>% filter(period == "calibration"),
      aes(x = date, y = obs_data),
      color = "black", fill = "black", alpha = 0.5,
      shape = plot_shape
    ) +
    geom_point(
      data = y_draws %>% filter(period != "calibration"),
      aes(x = date, y = obs_data),
      color = "black", alpha = 0.1, shape = plot_shape, fill = "white"
    ) +
    geom_vline(
      xintercept = forecast_date, linetype = "dashed",
      color = "black"
    ) +
    geom_vline(
      data = y_draws, aes(xintercept = last_hosp_data_date),
      linetype = "dashed",
      color = "gray"
    ) +
    scale_color_manual(values = set_color) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    xlab("") +
    ylab(y_label) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 10, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    guides(color = "none")
  trans <- if (log_scale) "log" else "linear"

  if (isFALSE(log_scale)) {
    plot <- plot +
      coord_cartesian(ylim = c(0, y_axis_lim))
  } else {
    plot <- plot + scale_y_continuous(trans = "log10")
  }

  if (y == "pred_ww") {
    # Fill in the WW data point
    plot <- plot + geom_point(
      data = y_draws %>% filter(period == "nowcast"),
      aes(x = date, y = obs_data),
      color = "black", fill = "black", alpha = 0.5,
      shape = plot_shape
    )
  }

  if (isTRUE(show_median)) {
    plot <- plot + geom_line(
      data = y_draws,
      aes(
        x = date, y = median,
        color = model_type
      ),
      size = 1, alpha = 1
    )
  }

  if (
    length(locations) == 1 &&
      length(model_type) == 1 &&
      length(hosp_reporting_delay) == 1
  ) {
    # If just one location, assume we just want to plot that location
    p <- plot + ggtitle(
      glue::glue(
        "{model_type} : observed and estimated {title} in ",
        "{locations} on {forecast_date}"
      )
    )
    full_file_path <- file.path(
      figure_file_path, locations
    )
    create_dir(full_file_path)
    save_figure <- ifelse(
      y == "pred_ww" && model_file_name == "hosp_only", FALSE, TRUE
    )


    if (save_figure && isTRUE(write_files)) {
      message("Saving figure")
      ggsave(
        file.path(
          full_file_path,
          glue::glue("{y}_draws_{model_file_name}_{trans}.png")
        ),
        plot = p,
        width = 12, # 8
        height = 5,
        units = "in",
        bg = "white"
      )
    }
  }

  if (!is.na(grouping_var) && grouping_var == "model_type") {
    p <- plot + facet_wrap({{ grouping_var }}) +
      ggtitle(
        glue::glue(
          "Observed and estimated {title} on {forecast_date} with ",
          "{hosp_reporting_delay} day hospital reporting delay in {locations}"
        )
      )
    if (isTRUE(write_files)) {
      full_file_path <- file.path(
        figure_file_path, locations
      )
      create_dir(full_file_path)
      ggsave(
        file.path(
          full_file_path,
          glue::glue("mult_models_hosp_forecast_{trans}.png")
        ),
        plot = p,
        width = 15,
        height = 5,
        bg = "white"
      )
    }
  }

  return(p)
}


#' Get the metadata needed for plotting
#'
#' @param y the output your plotting, default is NA in which case colors will be
#' based on other variables
#' @return a list with the plot color, label for y axis, and the output
#' description to use in the title
#' @export
#'
#' @examples
get_plot_metadata <- function(df, y = NA) {
  model_type <- df %>%
    select(model_type) %>%
    unique() %>%
    pull()
  forecast_date <- df %>%
    select(forecast_date) %>%
    unique() %>%
    pull()
  location <- df %>%
    select(location) %>%
    unique() %>%
    pull()

  if ("include_ww" %in% colnames(df)) {
    include_ww <- df %>%
      select(include_ww) %>%
      unique() %>%
      pull()
  } else {
    include_ww <- NA
  }

  if ("hosp_reporting_delay" %in% colnames(df)) {
    hosp_reporting_delay <- df %>%
      pull(hosp_reporting_delay) %>%
      unique()
  } else {
    hosp_reporting_delay <- NA
  }



  model_file_name <- case_when(
    model_type == "state-level aggregated wastewater" ~ "state_agg_WW_hosp",
    model_type == "hospital admissions only" ~ "hosp_only",
    model_type == "site-level observation error" ~ "site_level_obs_error",
    model_type == "site-level infection dynamics" ~ "site_level_inf_dyn",
    model_type == "site-level time-varying concentration" ~ "site_level_time_varying_C"
  )

  if (!is.na(y)) {
    plot_color <- case_when(
      y == "pred_hosp" ~ "darkred",
      y == "pred_hosp_per_100k" ~ "darkred",
      y == "pred_hosp" & model_type == "hospital admissions only" ~ "purple4",
      y == "pred_ww" ~ "darkgreen",
      TRUE ~ "gray"
    )

    y_label <- case_when(
      y == "pred_hosp" ~ "Daily hospital admissions",
      y == "pred_hosp_per_100k" ~ "Daily hospital admissions per 100k",
      y == "pred_ww" ~ "Wastewater genome copies/mL"
    )

    title <- case_when(
      y == "pred_hosp" ~ "daily hospital admissions",
      y == "pred_hosp_per_100k" ~ "daily hospital admissions per 100k",
      y == "pred_ww" ~ "genome copies/mL"
    )
  } else {
    plot_color <- NA
    y_label <- NA
    title <- NA
  }


  plot_shape <- ifelse(y == "pred_ww", 24, 21)

  return(list(
    plot_color = plot_color,
    y_label = y_label,
    title = title,
    model_type = model_type,
    model_file_name = model_file_name,
    location = location,
    forecast_date = forecast_date,
    plot_shape = plot_shape,
    include_ww = include_ww,
    hosp_reporting_delay = hosp_reporting_delay
  ))
}



#' Get plot of parameter distributions
#'
#' @param df dataframe either of parameter draws or of the filepath to obtain
#'  the parameter draws
#' @param figure_file_path directory to save the figure
#' @param from_full_df if TRUE then df is draws object, if FALSE then df is a
#' dataframe of filepaths to load in the parameter object
#' @param write_files if TRUE then write files to specified location, if FALSE
#' then don't
#' @param ...
#'
#' @return plot object of histograms of key parameters
#' @export
#'
#' @examples
get_plot_param_distribs <- function(df,
                                    figure_file_path,
                                    from_full_df = FALSE,
                                    write_files = TRUE, ...) {
  if (isFALSE(from_full_df)) {
    draws <- arrow::read_parquet(
      file =
        df$parameters_file_path
    )
  } else {
    draws <- df
  }




  posterior_params <- draws %>%
    filter(name %in% c(
      "phi_h", "eta_sd", "log10_g", "i0",
      "infection_feedback",
      "autoreg_rt", "initial_growth", "p_hosp_sd"
    ))

  # get metadata for filename and title
  plot_metadata <- get_plot_metadata(draws)
  location <- plot_metadata$location
  forecast_date <- plot_metadata$forecast_date
  hosp_reporting_delay <- plot_metadata$hosp_reporting_delay
  model_type <- plot_metadata$model_type
  model_file_name <- plot_metadata$model_file_name


  p <- ggplot(posterior_params) +
    geom_histogram(aes(x = value), alpha = 0.3) +
    facet_wrap(~name, scales = "free_x", nrow = 2) +
    theme_bw() +
    xlab("") +
    ylab("") +
    ggtitle(
      glue::glue(
        "Parameter draws for {model_type} in {location}",
        " from forecast on {forecast_date}"
      )
    ) +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )

  if (isTRUE(write_files)) {
    full_file_path <- file.path(
      figure_file_path, location
    )
    create_dir(full_file_path)

    ggsave(
      file.path(
        full_file_path,
        glue::glue("posterior_params_{model_file_name}.png")
      ),
      plot = p,
      width = 10,
      height = 5,
      bg = "white"
    )
  }


  return(p)
}


#' Plot of site-level Rt
#'
#' @param df dataframe of either model draws or filepath to load in model draws
#' @param figure_file_path where to save the figure
#' @param from_full_df if TRUE then df is draws object, if FALSE then df is a
#' dataframe of filepaths to load in the parameter object
#' @param write_files if TRUE then write files to specified location, if FALSE
#' then don't
#' @return plot object of R(t)s in each site with state level median overlaid
#' @export
get_rt_subpop_level <- function(df,
                                figure_file_path,
                                from_full_df = FALSE,
                                write_files = TRUE) {
  if (isFALSE(from_full_df)) {
    model_draws <- arrow::read_parquet(
      file =
        df$model_draws_file_path
    )
  } else {
    model_draws <- df
  }

  # Get the median for plotting
  rt_draws <- model_draws %>%
    filter(
      name == "R_site_t"
    )
  rt_draws <- rt_draws %>% left_join(
    rt_draws %>%
      group_by(date, subpop) %>%
      summarise(median_Rt = quantile(value, 0.5, na.rm = TRUE))
  )

  state_rt <- model_draws %>%
    filter(
      name == "R(t)"
    ) %>%
    group_by(date) %>%
    summarise(
      median_Rt_state = quantile(value, 0.5, na.rm = TRUE)
    )

  # Subset to 100 draws if greater than 500 passed in
  if (length(unique(rt_draws$draw)) > 500) {
    rt_draws <- rt_draws %>%
      filter(draw %in% sample(1:max(rt_draws$draw), 100))
  }

  plot_metadata <- get_plot_metadata(model_draws)
  location <- plot_metadata$location
  include_ww <- plot_metadata$include_ww
  forecast_date <- plot_metadata$forecast_date
  hosp_reporting_delay <- plot_metadata$hosp_reporting_delay
  model_type <- plot_metadata$model_type
  model_file_name <- plot_metadata$model_file_name
  n_sites <- length(unique(rt_draws$site))

  title <- glue::glue("Subpopulation R(t) as of {forecast_date} in {location}")

  plot <- ggplot(rt_draws) +
    geom_line(aes(x = date, y = value, group = draw, color = as.factor(subpop)),
      show.legend = FALSE,
      alpha = 0.05, size = 0.5
    ) +
    geom_line(aes(x = date, y = median_Rt, color = as.factor(subpop)),
      show.legend = FALSE, size = 1.3
    ) +
    geom_line(
      data = state_rt,
      aes(x = date, y = median_Rt_state), color = "black"
    ) +
    geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
    facet_wrap(~subpop) +
    xlab("") +
    ylab("R(t)") +
    theme_bw() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    ggtitle(title) +
    coord_cartesian(ylim = c(0, 3))
  theme(
    axis.text.x = element_text(
      size = 8, vjust = 1,
      hjust = 1, angle = 45
    ),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(
      size = 12,
      vjust = 0.5, hjust = 0.5
    )
  )

  if (isTRUE(write_files)) {
    full_file_path <- file.path(
      figure_file_path, location
    )
    create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("subpop_level_Rt_from_{model_file_name}.png")
      ),
      plot = plot,
      width = max(3 * round(sqrt(n_sites)), 6),
      height = max(2 * round(sqrt(n_sites)), 4),
      units = "in",
      bg = "white"
    )
  }



  return(plot)
}

#' Get R(t) for a single location
#'
#' @param quantiles dataframe of forecasts with quantiles including for R(t)
#' @param figure_file_path higher level directory where data from this pipeline will be saved
#' @param from_full_df if TRUE then df is draws object, if FALSE then df is a
#' dataframe of filepaths to load in the parameter object
#' @param write_files if TRUE then write files to specified location, if FALSE
#' then don't
#'
#' @return plot of R(t) for a single location, model, and forecast date
#' @export
#'
#' @examples
get_rt_from_draws <- function(df,
                              figure_file_path,
                              from_full_df = FALSE,
                              write_files = TRUE) {
  if (isFALSE(from_full_df)) {
    model_draws <- arrow::read_parquet(
      file =
        df$model_draws_file_path
    )
  } else {
    model_draws <- df
  }

  # Get the median for plotting
  rt_draws <- model_draws %>%
    filter(name == "R(t)")

  rt_draws <- rt_draws %>%
    left_join(
      rt_draws %>%
        group_by(date) %>%
        summarise(
          median_Rt = quantile(value, 0.5, na.rm = TRUE)
        )
    )

  # Subset to 100 draws if greater than 500 passed in
  if (length(unique(rt_draws$draw)) > 500) {
    rt_draws <- rt_draws %>%
      filter(draw %in% sample(1:max(rt_draws$draw), 100))
  }


  plot_metadata <- get_plot_metadata(model_draws)
  location <- plot_metadata$location
  include_ww <- plot_metadata$include_ww
  forecast_date <- plot_metadata$forecast_date
  hosp_reporting_delay <- plot_metadata$hosp_reporting_delay
  model_type <- plot_metadata$model_type
  model_file_name <- plot_metadata$model_file_name

  title <- glue::glue("R(t) as of {forecast_date} in {location} with {model_type}")

  p <- ggplot(rt_draws) +
    geom_line(aes(x = date, y = value, group = draw, color = period),
      alpha = 0.2,
      show.legend = FALSE
    ) +
    geom_line(aes(x = date, y = median_Rt),
      show.legend = FALSE
    ) +
    geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
    geom_hline(aes(yintercept = 1), linetype = "dotted") +
    xlab("") +
    ylab("R(t)") +
    theme_bw() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    ggtitle(title) +
    scale_y_continuous(trans = "log10") +
    theme(
      axis.text.x = element_text(
        size = 10, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(
        size = 12,
        vjust = 0.5, hjust = 0.5
      )
    )

  p

  if (isTRUE(write_files)) {
    full_file_path <- file.path(
      figure_file_path, location
    )
    create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("Rt_from_{model_file_name}.png")
      ),
      plot = p,
      width = 7,
      height = 7,
      units = "in",
      bg = "white"
    )
  }

  return(p)
}



#' Title
#'
#' @param df dataframe of either quantiles or the path to load in quantiles
#' @param figure_file_path directory to save figure
#' @param days_to_show_forecast duration of forecast to plot, default is 14
#' @param from_full_df if TRUE then df is quantiles object, if FALSE then df is a
#' dataframe of filepaths to load in the parameter object
#' @param write_files if TRUE then write files to specified location, if FALSE
#' then don't
#'
#' @return plot of bar chart of hospitalizations with mean state level
#' wastewater overlaid
#' @export
#'
#' @examples
get_combo_quantile_plot <- function(df,
                                    figure_file_path,
                                    days_to_show_forecast = 14,
                                    from_full_df = FALSE,
                                    write_files = TRUE) {
  if (isFALSE(from_full_df)) {
    quantiles_long <- arrow::read_parquet(
      file =
        df$quantiles_file_path
    )
  } else {
    quantiles_long <- df
  }


  quantiles_wide_sel_state <- pivot_data_for_plotting(quantiles_long)

  quantiles_wide <- quantiles_wide_sel_state %>%
    filter(date <= ymd(forecast_date) + days(days_to_show_forecast))

  plot_metadata <- get_plot_metadata(quantiles_wide_sel_state)
  location <- plot_metadata$location
  include_ww <- plot_metadata$include_ww
  forecast_date <- plot_metadata$forecast_date
  hosp_reporting_delay <- plot_metadata$hosp_reporting_delay
  model_type <- plot_metadata$model_type
  model_file_name <- plot_metadata$model_file_name

  coeff <- median(
    quantiles_wide$`quantity_0.5`[quantiles_wide$name == "pred_hosp"],
    na.rm = TRUE
  ) / median(
    quantiles_wide$`quantity_0.5`[
      quantiles_wide$name == "exp_state_ww_conc"
    ],
    na.rm = TRUE
  )

  max_obs_data <- quantiles_wide %>%
    select(daily_hosp_admits) %>%
    pull() %>%
    max(na.rm = TRUE)

  p <- ggplot(quantiles_wide) +
    geom_bar(
      data = quantiles_wide %>% filter(name == "pred_hosp"),
      aes(x = date, y = daily_hosp_admits), stat = "identity",
      color = "gray", fill = "gray", alpha = 0.3
    ) +
    geom_ribbon(
      data = quantiles_wide %>% filter(name == "pred_hosp"),
      aes(x = date, ymin = `quantity_0.25`, ymax = `quantity_0.75`),
      alpha = 0.1, fill = "darkred"
    ) +
    geom_ribbon(
      data = quantiles_wide %>% filter(name == "pred_hosp"),
      aes(x = date, ymin = `quantity_0.025`, ymax = `quantity_0.975`),
      alpha = 0.1, fill = "darkred"
    ) +
    geom_line(
      data = quantiles_wide %>% filter(name == "pred_hosp"),
      aes(x = date, y = `quantity_0.5`), color = "darkred"
    ) +
    geom_ribbon(
      data = quantiles_wide %>% filter(name == "exp_state_ww_conc"),
      aes(
        x = date, ymin = coeff * `quantity_0.25`,
        ymax = coeff * `quantity_0.75`
      ),
      alpha = 0.1, fill = "darkgreen"
    ) +
    geom_ribbon(
      data = quantiles_wide %>% filter(name == "exp_state_ww_conc"),
      aes(
        x = date, ymin = coeff * `quantity_0.025`,
        ymax = coeff * `quantity_0.975`
      ),
      alpha = 0.1, fill = "darkgreen"
    ) +
    geom_line(
      data = quantiles_wide %>% filter(name == "exp_state_ww_conc"),
      aes(x = date, y = coeff * `quantity_0.5`), color = "darkgreen"
    ) +
    geom_vline(xintercept = forecast_date, linetype = "dashed") +
    coord_cartesian(ylim = c(0, 3 * max_obs_data)) +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 10, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    ) +
    xlab("") +
    scale_y_continuous(
      # Features of the first axis
      name = "Daily Hospital Admissions",

      # Add a second axis and specify its features
      sec.axis = sec_axis(
        trans = ~ . / coeff,
        name = "Genome copies per mL"
      )
    ) +
    ggtitle(glue::glue("Hospital admissions and mean wastewater concentration in {location} from {model_type} model")) # nolint



  if (isTRUE(write_files)) {
    full_file_path <- file.path(figure_file_path, location)
    create_dir(full_file_path)
    ggsave(
      file.path(
        full_file_path,
        glue::glue("combined_quantiles_{model_file_name}.png")
      ),
      plot = p,
      width = 9,
      height = 4,
      units = "in",
      bg = "white"
    )
  }

  return(p)
}



#' Plot of lab-site level wastewater draws
#'
#' @param df dataframe of draws or dataframe containing filepath to load in draws
#' @param figure_file_path path to save figure
#'  @param from_full_df if TRUE then df is draws object, if FALSE then df is a
#' dataframe of filepaths to load in the parameter object
#' @param write_files if TRUE then write files to specified location, if FALSE
#' then don't
#' @return plot object of site level wastewater trajectories with observed data
#' overlaid
#' @export
#'
#' @examples
get_plot_labsite_ww_draws <- function(df,
                                      figure_file_path,
                                      from_full_df = FALSE,
                                      write_files = TRUE) {
  if (isFALSE(from_full_df)) {
    all_draws <- arrow::read_parquet(
      file =
        df$model_draws_file_path
    )
  } else {
    all_draws <- df
  }

  # Subset to 100 draws if greater than 500 passed in
  if (length(unique(all_draws$draw)) > 500) {
    all_draws <- all_draws %>%
      filter(draw %in% sample(1:max(all_draws$draw), 100))
  }


  ww_draws <- all_draws %>% filter(name == "pred_ww")


  lab_sites_to_plot <- ww_draws %>%
    select(lab_wwtp_unique_id) %>%
    filter(!is.na(lab_wwtp_unique_id)) %>%
    unique() %>%
    pull()

  plot_metadata <- get_plot_metadata(all_draws)
  forecast_date <- plot_metadata$forecast_date
  location <- plot_metadata$location
  model_file_name <- plot_metadata$model_file_name
  model_type <- plot_metadata$model_type


  p <- ggplot(ww_draws %>% filter(
    date <= forecast_date + days(0),
    lab_wwtp_unique_id %in% lab_sites_to_plot
  )) +
    geom_line(
      aes(
        x = date, y = value, group = draw,
        color = as.factor(lab_wwtp_unique_id)
      ),
      linewidth = 0.1, alpha = 0.1, show.legend = FALSE
    ) +
    geom_line(
      data = ww_draws %>% filter(
        period != "forecast",
        lab_wwtp_unique_id %in% lab_sites_to_plot
      ),
      aes(
        x = date, y = value, group = draw,
        color = as.factor(lab_wwtp_unique_id)
      ),
      linewidth = 0.1, alpha = 0.3, show.legend = FALSE
    ) +
    geom_point(aes(x = date, y = ww), size = 1, shape = 21) +
    geom_point(
      data = ww_draws %>% filter(
        period != "forecast",
        lab_wwtp_unique_id %in% lab_sites_to_plot
      ),
      aes(x = date, y = ww),
      fill = "black", size = 1, shape = 21
    ) +
    geom_point(
      data = ww_draws %>% filter(below_LOD == 1),
      aes(x = date, y = ww), size = 1, shape = 21, color = "red"
    ) +
    geom_point(
      data = ww_draws %>% filter(flag_as_ww_outlier == 1),
      aes(x = date, y = ww), size = 1.5, shape = 21, color = "purple"
    ) +
    geom_hline(aes(yintercept = lod_sewage), linetype = "dashed") +
    geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
    facet_wrap(~lab_wwtp_unique_id, scales = "free") +
    theme_bw() +
    xlab("") +
    ylab("Genome copies/mL") +
    ggtitle(glue::glue(
      "Lab-site-level wastewater concentrations in {location} from {model_type} model"
    ))

  p2 <- ggplot(ww_draws %>% filter(
    date <= forecast_date + days(0),
    lab_wwtp_unique_id %in% lab_sites_to_plot
  )) +
    geom_line(
      aes(
        x = date, y = log(value), group = draw,
        color = as.factor(lab_wwtp_unique_id)
      ),
      linewidth = 0.1, alpha = 0.1, show.legend = FALSE
    ) +
    geom_line(
      data = ww_draws %>% filter(
        period != "forecast",
        lab_wwtp_unique_id %in% lab_sites_to_plot
      ),
      aes(
        x = date, y = log(value), group = draw,
        color = as.factor(lab_wwtp_unique_id)
      ),
      linewidth = 0.1, alpha = 0.3, show.legend = FALSE
    ) +
    geom_point(aes(x = date, y = log(ww)), size = 1, shape = 21) +
    geom_point(
      data = ww_draws %>% filter(
        period != "forecast",
        lab_wwtp_unique_id %in% lab_sites_to_plot
      ),
      aes(x = date, y = log(ww)),
      fill = "black", size = 1, shape = 21
    ) +
    geom_point(
      data = ww_draws %>% filter(below_LOD == 1),
      aes(x = date, y = log(ww)), size = 1, shape = 21, color = "red"
    ) +
    geom_point(
      data = ww_draws %>% filter(flag_as_ww_outlier == 1),
      aes(x = date, y = log(ww)), size = 1.5, shape = 21, color = "purple"
    ) +
    geom_hline(aes(yintercept = log(lod_sewage)), linetype = "dashed") +
    geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
    facet_wrap(~lab_wwtp_unique_id, scales = "free") +
    theme_bw() +
    xlab("") +
    ylab("log(Genome copies/mL)") +
    ggtitle(glue::glue(
      "Lab-site-level wastewater concentrations in {location} from {model_type} model"
    ))




  if (isTRUE(write_files)) {
    print("Saving figure")
    full_file_path <- file.path(
      figure_file_path, location
    )
    create_dir(file.path(full_file_path))
    ggsave(
      file.path(
        full_file_path,
        glue::glue("lab_site_level_ww_conc_{model_file_name}.png")
      ),
      plot = p,
      width = 20,
      height = 20,
      units = "in",
      bg = "white"
    )
    ggsave(
      file.path(
        full_file_path,
        glue::glue("lab_site_level_log_conc_{model_file_name}.png")
      ),
      plot = p2,
      width = 20,
      height = 20,
      units = "in",
      bg = "white"
    )
  }

  return(p2)
}


#' Plot of lab-site level wastewater quantiles
#'
#' @param df dataframe of draws or dataframe containing filepath to load in draws
#' @param figure_file_path path to save figure
#'  @param from_full_df if TRUE then df is draws object, if FALSE then df is a
#' dataframe of filepaths to load in the parameter object
#' @param write_files if TRUE then write files to specified location, if FALSE
#' then don't
#' @return plot object of site level wastewater median, 50th, and 95% CI
#'  with observed data overlaid
#' @export
#'
#' @examples
get_ww_site_plots <- function(df,
                              figure_file_path,
                              from_full_df = FALSE,
                              write_files = TRUE) {
  if (isFALSE(from_full_df)) {
    model_draws <- arrow::read_parquet(
      file =
        df$model_draws_file_path
    )
  } else {
    model_draws <- df
  }


  # For now do this, later get quantiles for all sites and use that directly
  quantiles <- get_quantiles_on_site_level_ww(model_draws)

  ww_site <- quantiles %>% filter(date <= ymd(forecast_date) + days(14))

  plot_metadata <- get_plot_metadata(ww_site)
  location <- plot_metadata$location
  include_ww <- plot_metadata$include_ww
  forecast_date <- plot_metadata$forecast_date
  model_type <- plot_metadata$model_type
  model_file_name <- plot_metadata$model_file_name


  ww_site <- ww_site %>% mutate(
    lab_name = ifelse(!is.na(lab), paste0(
      "Lab: ",
      as.character(lab)
    ),
    "Lab not specified"
    )
  )

  ww_site_test <- ww_site %>% filter(!is.na(lab) & !is.na(site))
  n_sites <- ww_site %>%
    select(site_lab) %>%
    unique() %>%
    nrow()

  p <- ggplot(ww_site) +
    geom_line(aes(x = date, y = median_ww, color = as.factor(site_lab)),
      show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = lb_25th, ymax = ub_75th,
        fill = as.factor(site_lab)
      ),
      alpha = 0.3, show.legend = FALSE
    ) +
    geom_ribbon(
      aes(
        x = date, ymin = lb_025th, ymax = ub_975th,
        fill = as.factor(site_lab)
      ),
      alpha = 0.3, show.legend = FALSE
    ) +
    geom_point(aes(x = date, y = log_conc)) +
    geom_point(
      data = ww_site %>% filter(below_LOD == 1),
      aes(x = date, y = log_conc), color = "red", size = 1.1
    ) +
    geom_point(
      data = ww_site %>% filter(flag_as_ww_outlier == 1),
      aes(x = date, y = log_conc), color = "blue", size = 1.1
    ) +
    geom_vline(aes(xintercept = forecast_date), linetype = "dashed") +
    facet_wrap(~site_lab, scales = "free") +
    xlab("") +
    ylab("Log(Genome copies per mL)") +
    ggtitle(glue::glue(
      "Site-level expected observed wastewater concentration in {location} from {model_type} model"
    )) +
    theme_bw() +
    scale_color_discrete() +
    scale_fill_discrete() +
    scale_x_date(
      date_breaks = "2 weeks",
      labels = scales::date_format("%Y-%m-%d")
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 8, vjust = 1,
        hjust = 1, angle = 45
      ),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      plot.title = element_text(
        size = 10,
        vjust = 0.5, hjust = 0.5
      )
    )


  if (isTRUE(write_files)) {
    full_file_path <- full_file_path <- file.path(
      figure_file_path, location
    )
    create_dir(full_file_path)

    ggsave(
      file.path(
        full_file_path,
        glue::glue("site_level_WW_conc_{model_file_name}.png")
      ),
      plot = p,
      width = max(3 * round(sqrt(n_sites)), 6),
      height = max(2 * round(sqrt(n_sites)), 4),
      units = "in",
      bg = "white"
    )
  }

  return(p)
}


#' Get R(t) box plot across states
#'
#' @param df_of_filepaths dataframe containing the filepaths needed to load
#' in the R(t) trajectories for each state
#' @param figure_file_path directory to save the figure
#' @param write_files if TRUE then write files to specified location, if FALSE
#' then don't
#' @return plot object of R(t) as a horizontal box plot for each state
#' @export
#'
#' @examples
get_rt_boxplot_across_states <- function(df_of_filepaths,
                                         figure_file_path,
                                         write_files = TRUE) {
  # Need to be able to load in multiple model runs and combine
  for (i in seq_len(nrow(df_of_filepaths))) {
    model_draws_i <- arrow::read_parquet(
      file =
        df_of_filepaths$model_draws_file_path[i]
    )

    model_draws_i <- model_draws_i %>%
      filter(name == "R(t)") %>%
      select(
        name, t, value, draw, include_ww, forecast_date, hosp_reporting_delay,
        location, date, daily_hosp_admits, daily_hosp_admits_for_eval, obs_data,
        period, model_type
      )

    if (i == 1) {
      rt_draws <- model_draws_i
    } else {
      rt_draws <- rbind(rt_draws, model_draws_i)
    }
  }

  plot_metadata <- get_plot_metadata(rt_draws)
  location <- plot_metadata$location
  include_ww <- plot_metadata$include_ww
  forecast_date <- plot_metadata$forecast_date
  hosp_reporting_delay <- plot_metadata$hosp_reporting_delay
  model_type <- plot_metadata$model_type
  model_file_name <- plot_metadata$model_file_name
  if (length(model_file_name) > 1) {
    model_file_name <- "comb_models"
    model_type <- "multiple models"
  }


  # find median R(t)
  r_t_draws_w_median <- rt_draws %>%
    select(location, value, draw, name, forecast_date, date, t) %>%
    left_join(
      rt_draws %>%
        group_by(location, date) %>%
        summarise(
          R_t_median = quantile(value, 0.5, na.rm = TRUE)
        ),
      by = c("location", "date")
    ) %>%
    filter(date == forecast_date) %>%
    arrange(R_t_median, "desc")

  r_t_draws_w_median$location <- factor(r_t_draws_w_median$location,
    levels = unique(as.character(
      r_t_draws_w_median$location
    ))
  )

  p <- ggplot(r_t_draws_w_median, aes(
    x = location,
    y = value, fill = R_t_median
  )) +
    geom_boxplot() +
    geom_hline(aes(yintercept = 1), linetype = "dashed") +
    coord_flip() +
    theme_bw() +
    ylab("R(t) estimate") +
    xlab("State") +
    # scale_fill_gradientn(colours = terrain.colors(10))+
    scale_fill_gradient(low = "blue", high = "red") +
    ggtitle(glue::glue("R(t) estimate as of {forecast_date} from {model_type} model")) +
    guides(fill = guide_legend(title = "R(t)")) +
    scale_y_continuous(trans = "log")

  if (isTRUE(write_files)) {
    full_file_path <- file.path(
      figure_file_path
    )
    create_dir(full_file_path)

    ggsave(
      file.path(
        full_file_path,
        glue::glue("R_t_all_states-{model_file_name}.png")
      ),
      plot = p,
      width = 7,
      height = 7,
      units = "in",
      bg = "white"
    )
  }
  return(p)
}


#' Get plot COVID hub submission
#'
#' @param df dataframe containing the 23 quantiles in the forecast dates for all
#' states for submitting to the hub
#' @param days_to_show_truth_data days we want to go back to show the observed
#' data before making our forecast, default is 90
#' @param truth_data_path link to get the truth data for comparison,
#' default is from the hub
#'
#' @return
#' @export
#'
#' @examples
get_plot_covidhub_submission <- function(df,
                                         days_to_show_truth_data = 90,
                                         truth_data_path = "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/data-truth/truth-Incident%20Hospitalizations.csv") { # nolint
  location_code <- df %>%
    pull(location) %>%
    unique()
  forecast_date <- df %>%
    pull(forecast_date) %>%
    unique()


  truth_data <- read_csv(truth_data_path) %>%
    filter(
      location == location_code,
      date >= forecast_date - days(days_to_show_truth_data)
    )

  location <- truth_data %>%
    pull(location_name) %>%
    unique()

  plot_title <- glue::glue("{location} as of {forecast_date}")

  quant_plot <- plot_quantiles(df,
    time_column = target_end_date,
    observation_column = value,
    quantile_level_column = quantile
  )

  plot <- quant_plot +
    ggplot2::geom_line(
      data = truth_data,
      mapping = ggplot2::aes(
        x = date,
        y = value
      ),
      alpha = 1,
      size = 2,
      color = "black",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = truth_data,
      mapping = ggplot2::aes(
        x = date,
        y = value
      ),
      alpha = 1,
      size = 4,
      color = "black",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_y_continuous(
      trans = "log"
    ) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::labs(
      y = "Daily COVID-19 hospital admissions",
      x = ""
    ) + ggplot2::ggtitle(plot_title)


  return(plot)
}
