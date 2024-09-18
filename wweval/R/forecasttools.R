# Functions from forecasttools
#' Aggregate individual trajectory
#' timeseries or forecasts to quantile
#' timeseries or forecasts
#'
#' Given a tidy data frame of
#' trajectories, aggregate it to
#' a quantile timeseries for the
#' given quantile values
#'
#' @param trajectories tidy data frame or tibble
#' of trajectories
#' @param quantiles quantiles to output for each
#' timepoint (default the FluSight 2023 quantiles:
#' `c(0.01, 0.025, seq(0.05, 0.95, 0.05), 0.975, 0.99)`
#' @param timepoint_cols name of the column(s) in`trajectories`
#' that identifies unique timepoints. Default `timepoint`.
#' @param value_col name of the column in `trajectories`
#' with the trajectory values (for which we wish to
#' compute quantiles), e.g. `hosp`, `weekly_hosp`, `cases`,
#' etc. Default `value`.
#' @param quantile_value_name What to name
#' the column containing quantile values in
#' the output table. Default `"quantile_value"`
#' @param quantile_level_name What to name
#' the column containing quantile levels in
#' the output table. Default `"quantile_level"`
#' @param id_cols additional id columns in
#' `trajectories` to group by before aggregating,
#' e.g. a `location` column if `trajectories` contains
#' trajectories over the same time period for multiple
#' locations, such as different US States and Territories.
#' If NULL, ignored. Default NULL.
#' @export
trajectories_to_quantiles <- function(trajectories,
                                      quantiles = c(
                                        0.01, 0.025,
                                        seq(0.05, 0.95, 0.05),
                                        0.975, 0.99
                                      ),
                                      timepoint_cols = "timepoint",
                                      value_col = "value",
                                      quantile_value_name =
                                        "quantile_value",
                                      quantile_level_name =
                                        "quantile_level",
                                      id_cols = NULL) {
  grouped_df <- trajectories |>
    dplyr::rename(
      value_col = {{ value_col }}
    ) |>
    dplyr::group_by(
      dplyr::across(c(
        {{ timepoint_cols }}, {{ id_cols }}
      ))
    )

  quant_df <- grouped_df |>
    dplyr::reframe(
      {{ quantile_value_name }} := quantile(value_col,
        probs = !!quantiles
      ),
      {{ quantile_level_name }} := !!quantiles
    )
  return(quant_df)
}

#' Pull NHSN data from HealthData.gov
#'
#' Pull relevant epidemiological
#' data from NHSN, defaulting to the
#' HealthData.gov public API endpoint.
#'
#' @param api_endpoint API endpoint to
#' use. Defaults to the HTTPS:// Socrata
#' endpoint for HHS Protect / NHSN
#' on HealthData.gov as of 2023-10-23, namely
#' `[https://healthdata.gov/resource/g62h-syeh]`
#' @param api_key_id Key ID of an API key to use
#' when querying the dataset. Not required,
#' but polite and reduces throttling.
#' You can create one at
#' [https://healthdata.gov/profile/edit/developer_settings].
#' Default `NULL` (no API key).
#' @param api_key_secret Associated key secret
#' for the API key given in `api_key_id`.
#' Default `NULL` (no API key).
#' @param start_date Pull only rows with dates
#' greater than or equal to this date. If `NULL`,
#' no minimum date. Default `NULL`.
#' @param end_date Pull only rows with dates
#' less than or equal to this date. If `NULL`,
#' no maximum date. Default `NULL`.
#' @param columns Vector of columns to retrieve, in
#' addition to `date` and `state`, which are always
#' retrieved. If `NULL`, retrieve all columns.
#' Default `NULL`.
#' @param states value or values to filter on for the `state` column
#' of the NHSN dataset. If `NULL`, do not filter on that column.
#' Default `NULL`.
#' @param order_by column or columns to order (sort) by.
#' Default `c("state", "date")` (sort first by state,
#' then by date).
#' @param desc Boolean. Whether to order descending instead of
#' ascending. Default `FALSE` (order ascending).
#' @param limit maximum number of rows to return. Default `1e5`
#' (100000)
#' @param error_on_limit Boolean. Raise an error if the number
#' of rows returned is equal to the maximum? Default `TRUE`.
#' This ensures that one does not silently end up with a
#' subset of the total set of rows matching the query. If a subset
#' is desired, one can set `error_on_limit = FALSE`.
#' @param ... other arguments passed to [nhsn_soda_query()]
#' @return the pulled data, as a [tibble::tibble()].
#' @export
pull_nhsn <- function(api_endpoint =
                        "https://healthdata.gov/resource/g62h-syeh.json",
                      api_key_id = NULL,
                      api_key_secret = NULL,
                      start_date = NULL,
                      end_date = NULL,
                      columns = NULL,
                      states = NULL,
                      order_by = c("state", "date"),
                      desc = FALSE,
                      limit = 1e5,
                      error_on_limit = TRUE,
                      ...) {
  check_package_is_installed("httr")

  query <- nhsn_soda_query(
    api_endpoint,
    start_date = start_date,
    end_date = end_date,
    columns = columns,
    states = states,
    order_by = order_by,
    desc = desc,
    limit = limit,
    ...
  )

  socrata_url <- as.character(query)

  credentials <- !is.null(api_key_id) & !is.null(api_key_secret)

  if (credentials) {
    response <- httr::GET(
      socrata_url,
      httr::authenticate(api_key_id, api_key_secret)
    )
  } else {
    cli::cli_warn(c(
      "No API key ID and secret provided. ",
      "This is considered impolite and ",
      "may result in your requests to the ",
      "server getting throttled. Create an ",
      "API key id/secret pair by visiting ",
      "https://healthdata.gov/profile/edit/developer_settings"
    ))
    response <- httr::GET(
      socrata_url
    )
  }

  if (response$status != 200) {
    cli::cli_abort("Bad response {response}")
  }

  df <- jsonlite::fromJSON(httr::content(response, "text")) |>
    tibble::as_tibble()

  if (error_on_limit && !dim(df)[1] < limit) {
    cli::cli_abort(c(
      "Query retrieved a number of",
      "records equal to the query limit. ",
      "Some matching records may therefore",
      "be excluded. Try a narrower query, a ",
      "higher limit, or, if this was intended, ",
      "set `error_on_limit = FALSE`"
    ))
  }
  return(df)
}

#' Return a [soql::soql_where()] construct
#' for a given column being in a list of values
#'
#' @param soql_list A `soql` query object, which
#' can be piped in. If one hasn't been
#' created yet, use or pipe in [soql::soql()].
#' @param column The column to filter on
#' @param match_values A vector of values that column
#' must match
#' @return A new soql object with the filter added,
#' for use in other functions.
#' @export
soql_is_in <- function(soql_list, column, match_values) {
  query <- glue::glue("{column}='{match_values}'") |>
    paste(collapse = " OR ")
  return(soql::soql_where(soql_list, query))
}


#' Construct a Socrata open data
#' API (SODA) query for the NSHN
#' dataset
#' @param api_endpoint Base API endpoint URL to use
#' when constructing the query.
#' @param start_date Pull only rows with dates
#' greater than or equal to this date. If `NULL`,
#' no minimum date. Default `NULL`.
#' @param end_date Pull only rows with dates
#' less than or equal to this date. If `NULL`,
#' no maximum date. Default `NULL`.
#' @param columns Vector of columns to retrieve, in
#' addition to `date` and `state`, which are always
#' retrieved. If `NULL`, retrieve all columns.
#' Default `NULL`.
#' @param states Vector of states or territories to
#' retrieve, by two letter US postal service code.
#' If `NULL`, retrieve all. Default `NULL`.
#' @param limit limit to the number of rows to retrieve.
#' Default 1e5.
#' @param order_by Vector of columns by which to order the
#' results. Default `c("state", "date")`
#' @param desc whether to order descending instead of
#' ascending. Default `FALSE` (order ascending).
#' @param ... additional arguments (ignored for now)
#' @return the query as [soql::soql()] output
#' @export
nhsn_soda_query <- function(api_endpoint,
                            start_date = NULL,
                            end_date = NULL,
                            columns = NULL,
                            states = NULL,
                            limit = 1e5,
                            order_by = c("state", "date"),
                            desc = FALSE,
                            ...) {
  query <- soql::soql() |>
    soql::soql_add_endpoint(api_endpoint)

  if (!is.null(columns)) {
    query <- query |>
      soql::soql_select(paste(
        unique(
          c("state", "date", columns)
        ),
        collapse = ","
      ))
  }

  if (!is.null(start_date)) {
    query <- query |>
      soql::soql_where(
        glue::glue("date >= '{start_date}'")
      )
  }

  if (!is.null(end_date)) {
    query <- query |>
      soql::soql_where(
        glue::glue("date <= '{end_date}'")
      )
  }

  if (!is.null(states)) {
    query <- query |>
      soql_is_in(
        "state", states
      )
  }

  query <- query |>
    soql::soql_order(
      paste(unique(order_by),
        collapse = ","
      ),
      desc = desc
    )

  ## do limit string formatting
  ## manually since soql::soql_limit()
  ## coerces input to numeric and then
  ## string formats with XeY notation
  ## (e.g. 100000 as '1e5'), which endpoints
  ## will fail to parse
  query$clauses$limit <- sprintf("%d", as.numeric(limit))

  return(query)
}

#' Plot a timeseries of quantiles
#'
#' @param data timeseries of quantiles as tidy data,
#' with one row per timepoint per quantile level
#' @param time_column name of the column in `data`
#' containing timepoints
#' @param observation_column name of the column
#' in data containing observed values at the
#' given quantile levels
#' @param quantile_level_column name of the column
#' in `data` giving the quantile level (e.g.
#' `0.01` for the 0.01 quantile / 1st percentile)
#' @param linesize `size` parameter passed to [ggplot2::geom_line()].
#' Default 2.
#' @param pointsize `size` parameter passed to [ggplot2::geom_point()]
#' Default 4.
#' @param linecolor `color` parameter passed to [ggplot2::geom_line()].
#' Default "darkblue.
#' @param pointcolor `color` parameter passed to [ggplot2::geom_point()]
#' Default "darkblue".
#' @return the resultant plot, as a ggplot objec
#' @export
plot_quantiles <- function(data,
                           time_column,
                           observation_column,
                           quantile_level_column,
                           linesize = 2,
                           pointsize = 4,
                           pointcolor = "darkblue",
                           linecolor = "darkblue") {
  return(ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = {{ time_column }},
      y = {{ observation_column }},
      group = {{ quantile_level_column }},
      alpha = 1 - abs({{ quantile_level_column }} - 0.5)
    ),
    data = data
  ) +
    ggplot2::geom_line(
      size = linesize,
      color = linecolor
    ) +
    ggplot2::geom_point(
      size = pointsize,
      color = pointcolor
    ) +
    ggplot2::scale_alpha_continuous(guide = NULL))
}

#' Convert a two-letter location abbreviation to a
#' two-character FluSight location code
#'
#' Given a vector of state/territory two-letter
#' USPS short names (e.g. MA, TX, PR), return
#' the corresponding FluSight challenge location
#' code (legacy FIPS code for states and territories,
#' `US` for the US).
#'
#' @param abbrs vector of USPS two letter name abbreviations
#' @return vector of the same length recoded as flusight
#' location codes
#' @export
loc_abbr_to_flusight_code <- function(abbrs) {
  mask <- match(
    x = abbrs,
    table = wweval::flusight_location_table$short_name
  )
  return(
    wweval::flusight_location_table$location_code[mask]
  )
}
