#' Pull daily NHSN data from HealthData.gov
#'
#' Pull relevant epidemiological
#' data from NHSN, defaulting to the
#' HealthData.gov public API endpoint.
#'
#' @param api_endpoint API endpoint to
#' use. Defaults to the HTTPS:// Socrata
#' endpoint for HHS Protect / NHSN
#' on HealthData.gov as of 2023-10-23, namely
#' <https://healthdata.gov/resource/g62h-syeh>
#' @param api_key_id Key ID of an API key to use
#' when querying the dataset. Not required,
#' but polite and reduces throttling.
#' You can create one at
#' <https://healthdata.gov/profile/edit/developer_settings>.
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
pull_daily_nhsn <- function(
  api_endpoint = "https://healthdata.gov/resource/g62h-syeh.json",
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
  ...
) {
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
nhsn_soda_query <- function(
  api_endpoint,
  start_date = NULL,
  end_date = NULL,
  columns = NULL,
  states = NULL,
  limit = 1e5,
  order_by = c("state", "date"),
  desc = FALSE,
  ...
) {
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
        "state",
        states
      )
  }

  query <- query |>
    soql::soql_order(
      paste(unique(order_by), collapse = ","),
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


#' Pull and save the current state of NHSN data on HealthData.gov
#'
#' Pull data using [pull_daily_nhsn()], format it for pipeline ingestion,
#' and write it to disk as a date-stamped `.csv` file.
#'
#' @param location_data_path Path to a csv file containing data
#' about constituent locations, with columns `"abbreviation"` and
#' `"population"`
#' @param output_dir Directory in which to save the output.
#' @param force Overwrite an existing `.csv` with the same date-stamp?
#' Boolean, default `FALSE`.
#' @return NULL, saving the data to disk as a side effect.
#' @examples
#' \dontrun{
#' # this will write a file named `<today's date>.csv` to a directory named
#' # `input/hosp_data/vintage_datasets`.
#' locs <- file.path("input", "locations.csv")
#' out <- file.path("input", "hosp_data", "vintage_datasets")
#' pull_and_write_hosp_data(locs, out)
#' }
#' @export
pull_and_write_hosp_data <- function(
  location_data_path,
  output_dir,
  force = FALSE
) {
  pull_date <- lubridate::today()

  location_data <- readr::read_csv(location_data_path) |>
    dplyr::select(
      state = "abbreviation",
      pop = "population"
    )

  raw_data <- wweval::pull_daily_nhsn(
    start_date = "2023-01-01",
    columns = c(
      "previous_day_admission_adult_covid_confirmed",
      "previous_day_admission_pediatric_covid_confirmed"
    )
  )

  data <- raw_data |>
    dplyr::mutate(
      daily_hosp_admits = as.numeric(
        .data$previous_day_admission_adult_covid_confirmed
      ) +
        as.numeric(
          .data$previous_day_admission_pediatric_covid_confirmed
        ),
      date = as.Date(.data$date) - lubridate::ddays(1)
    ) |>
    ## convert from previous day to date-of-event indexing,
    ## following covidcast/epidatr
    dplyr::inner_join(location_data, by = "state") |>
    dplyr::select(
      "date",
      ABBR = "state",
      "daily_hosp_admits",
      "pop"
    )

  output_path <- fs::path(output_dir, pull_date, ext = "csv")

  if (fs::file_exists(output_path) && !force) {
    cli::cli_abort(c(
      "File {output_path} already exists. Run with force = TRUE",
      "to overwrite it"
    ))
  } else {
    cli::cli_inform("Saving output to {output_path}...")
    readr::write_csv(data, output_path)
  }
}
