# Sample data to use in tests
sample_ww_data <- dplyr::tibble(
  date = as.Date(c("2020-12-30", "2021-01-02", "2021-01-03", "2021-01-04")),
  site_index = c(1, 1, 2, 2),
  lab_site_index = c(1, 2, 3, 3),
  below_LOD = c(0, 1, 0, 1),
  ww = c(100, 200, 150, 250),
  lod_sewage = c(10, 20, 15, 25),
  ww_pop = c(5000, 5500, 3000, 3000),
  other_col = c(10, 20, 30, 40)
)

# Create sample hospital admissions data
sample_hosp_data <- dplyr::tibble(
  date = as.Date(c("2020-12-30", "2020-12-31", "2021-01-01"))
)


# Test that the function returns correct counts
test_that("Function returns correct counts", {
  result <- get_ww_data_sizes(sample_ww_data)

  expect_equal(result$owt, nrow(sample_ww_data))
  expect_equal(result$n_censored, sum(sample_ww_data$below_LOD == 1))
  expect_equal(result$n_uncensored, sum(sample_ww_data$below_LOD == 0))
})

# Test that function handles error properly when LOD column is missing
test_that("Error is thrown when LOD column is missing", {
  expect_error(
    get_ww_data_sizes(sample_ww_data, "nonexistent_column"),
    "LOD column name isn't present in input dataset"
  )
})

# Test that function works with different LOD column names
test_that("Function works with different LOD column names", {
  # Rename below_LOD to new_LOD_col for testing purposes
  renamed_sample <- dplyr::rename(sample_ww_data, new_LOD_col = below_LOD)

  result <- get_ww_data_sizes(renamed_sample, "new_LOD_col")

  expect_equal(result$n_censored, sum(renamed_sample$new_LOD_col == 1))
})

# Test that number of unique sites and lab_sites are calculated correctly
test_that("Number of unique sites and lab_sites are calculated correctly", {
  result <- get_ww_data_sizes(sample_ww_data)

  expect_equal(result$n_ww_sites, length(unique(sample_ww_data$site_index)))
  expect_equal(result$n_ww_lab_sites, length(unique(sample_ww_data$lab_site_index)))
})

# Test that the function returns a list with the correct names
test_that("Function returns a list with correct names", {
  result <- get_ww_data_sizes(sample_ww_data)

  expected_names <- c("owt", "n_censored", "n_uncensored", "n_ww_sites", "n_ww_lab_sites")
  expect_equal(names(result), expected_names)
})




# Test that function returns correct indices
test_that("Function returns correct indices", {
  result <- get_ww_data_indices(sample_ww_data,
    sample_hosp_data,
    owt = nrow(sample_ww_data)
  )

  expect_equal(result$ww_censored, which(sample_ww_data$below_LOD == 1))
  expect_equal(result$ww_uncensored, which(sample_ww_data$below_LOD == 0))
})

# Test that function throws an error when owt does not match expected length
test_that("Error is thrown when owt does not match expected length", {
  expect_error(
    get_ww_data_indices(sample_ww_data,
      sample_hosp_data,
      owt = nrow(sample_ww_data) + 1
    ),
    "Length of censored vectors incorrect"
  )
})

# Test that sampled times are calculated correctly
test_that("Sampled times are calculated correctly", {
  result <- get_ww_data_indices(sample_ww_data,
    sample_hosp_data,
    owt = nrow(sample_ww_data)
  )

  expected_times <- data.frame(
    date = seq(
      from = min(sample_hosp_data$date),
      to = max(sample_ww_data$date), by = "days"
    ),
    t = 1:as.integer(max(sample_ww_data$date) -
      min(sample_hosp_data$date) + 1) # nolint
  )
  ww_data <- sample_ww_data |>
    dplyr::left_join(expected_times, by = "date")
  expected_t <- ww_data$t

  expect_equal(result$ww_sampled_times, expected_t)
})

# Test that sampled sites and lab-sites indices are correct
test_that("Sampled sites and lab-sites indices are correct", {
  result <- get_ww_data_indices(sample_ww_data,
    sample_hosp_data,
    owt = nrow(sample_ww_data)
  )

  expect_equal(result$ww_sampled_sites, sample_ww_data$site_index)
  expect_equal(result$ww_sampled_lab_sites, sample_ww_data$lab_site_index)
})

# Test that lab-site to site map is correct
test_that("Lab-site to site map is correct", {
  result <- get_ww_data_indices(sample_ww_data,
    sample_hosp_data,
    owt = nrow(sample_ww_data)
  )

  # Create the expected mapping manually for the test case
  set_mapping <- c(1, 1, 2)

  expect_equal(result$lab_site_to_site_map, set_mapping)
})

# Test that function returns correct log LOD values
test_that("Function returns correct log LOD values", {
  result <- get_ww_values(sample_ww_data)

  expected_lod <- log(sample_ww_data$lod_sewage)
  expect_equal(result$ww_lod, expected_lod)
})

# Test that population averages are calculated correctly when one_pop_per_site is TRUE
test_that("Population averages are calculated correctly for one_pop_per_site = TRUE", {
  result <- get_ww_values(sample_ww_data)

  expected_pop_avg <- sample_ww_data |>
    dplyr::group_by(site_index) |>
    dplyr::summarise(pop_avg = mean(ww_pop)) |>
    dplyr::pull(pop_avg)

  expect_equal(result$pop_ww, expected_pop_avg)
})

# Test that population vector is returned correctly when one_pop_per_site is FALSE
test_that("Population vector is returned correctly for one_pop_per_site = FALSE", {
  result <- get_ww_values(sample_ww_data,
    one_pop_per_site = FALSE
  )

  expect_equal(result$pop_ww, sample_ww_data$ww_pop)
})

# Test that function returns correct log concentration values
test_that("Function returns correct log concentration values", {
  result <- get_ww_values(sample_ww_data)

  # Adding a small constant to avoid taking log of zero
  expected_log_conc <- log(sample_ww_data$ww + 1e-8)

  expect_equal(result$log_conc, expected_log_conc)
})

# Test that function handles different measurement column names
test_that("Function handles different measurement column names", {
  # Rename 'ww' to 'new_ww_col' for testing purposes
  renamed_sample <- dplyr::rename(sample_ww_data, new_ww_col = ww)

  result <- get_ww_values(renamed_sample,
    ww_measurement_col_name = "new_ww_col"
  )

  expected_log_conc <- log(renamed_sample$new_ww_col + 1e-8)

  expect_equal(result$log_conc, expected_log_conc)
})

test_that("Function handles different LOD value column names", {
  # Rename 'lod_sewage' to 'new_lod_sewage' for testing purposes
  renamed_sample <- dplyr::rename(sample_ww_data, new_lod_sewage = lod_sewage)

  result <- get_ww_values(renamed_sample,
    ww_lod_value_col_name = "new_lod_sewage"
  )

  expected_lod <- log(renamed_sample$new_lod_sewage)

  expect_equal(result$ww_lod, expected_lod)
})

# Test that function handles different population column names
test_that("Function handles different population column names", {
  # Rename 'ww_pop' to 'new_ww_pop' for testing purposes
  renamed_sample <- dplyr::rename(sample_ww_data, new_ww_pop = ww_pop)

  result_true <- get_ww_values(renamed_sample,
    ww_site_pop_col_name = "new_ww_pop",
    one_pop_per_site = TRUE
  )

  expected_pop_avg_true <- renamed_sample |>
    dplyr::group_by(site_index) |>
    dplyr::summarise(pop_avg = mean(new_ww_pop)) |>
    dplyr::pull(pop_avg)

  expect_equal(result_true$pop_ww, expected_pop_avg_true)
})
