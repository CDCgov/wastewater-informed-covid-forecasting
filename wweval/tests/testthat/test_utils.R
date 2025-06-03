test_that(
  paste0(
    "save_rds_with_suffix() uses object name if ",
    "basename is not provided, ",
    "uses basename if provided, ",
    "and respects output_dir, ",
    "suffix, and ext values. ",
    "Objects can be read back via ",
    "read_rds_with_suffix()."
  ),
  {
    mock_object <- list(a = 1, b = 2)

    withr::with_tempdir({
      temp_dir <- getwd()
      temp_subdir <- fs::path(temp_dir, "test")
      fs::dir_create(temp_subdir)
      expected_path_one <- fs::path(temp_subdir, "mock_object.rds")
      expect_false(fs::file_exists(expected_path_one))
      save_rds_with_suffix(mock_object, temp_subdir)
      expect_true(fs::file_exists(expected_path_one))
      expect_equal(
        mock_object,
        read_rds_with_suffix(
          "mock_object",
          temp_subdir
        )
      )

      custom_basename <- "custom_name"
      expected_path_two <- fs::path(temp_subdir, "custom_name.rds")
      expect_false(fs::file_exists(expected_path_two))
      save_rds_with_suffix(mock_object, temp_subdir, basename = custom_basename)
      expect_true(fs::file_exists(expected_path_two))
      expect_equal(
        mock_object,
        read_rds_with_suffix(
          custom_basename,
          temp_subdir
        )
      )

      custom_suffix <- "_suffix"
      expected_path_three <- fs::path(temp_subdir, "mock_object_suffix.rds")
      expect_false(fs::file_exists(expected_path_three))
      save_rds_with_suffix(mock_object, temp_subdir, suffix = custom_suffix)
      expect_true(fs::file_exists(expected_path_three))
      expect_equal(
        mock_object,
        read_rds_with_suffix("mock_object", temp_subdir, suffix = custom_suffix)
      )

      custom_ext <- "custom_ext"
      expected_path_four <- fs::path(temp_subdir, "mock_object.custom_ext")
      expect_false(fs::file_exists(expected_path_four))
      save_rds_with_suffix(mock_object, temp_subdir, ext = custom_ext)
      expect_true(fs::file_exists(expected_path_four))
      expect_equal(
        mock_object,
        read_rds_with_suffix("mock_object", temp_subdir, ext = custom_ext)
      )
    })
  }
)


test_that("assert_needed_env_vars() works as expected", {
  withr::with_envvar(
    c(
      "TEST_ENV_VAR_ONE" = "a",
      "TEST_ENV_VAR_TWO" = "b"
    ),
    {
      expect_no_warning(
        assert_needed_env_vars(c(
          "TEST_ENV_VAR_ONE",
          "TEST_ENV_VAR_TWO"
        ))
      )
      expect_error(
        assert_needed_env_vars(
          "THIS_SHOULD_BE_MISSING_UEFDJIREX"
        ),
        "Could not find required"
      )
      expect_error(
        assert_needed_env_vars(
          c(
            "TEST_ENV_VAR_ONE",
            "TEST_ENV_VAR_TWO",
            "THIS_SHOULD_BE_MISSING_UEFDJIREX"
          )
        ),
        "Could not find required"
      )
    }
  )
})

test_that("order_col behavior corresponds to manual expectation", {
  df <- tibble::tibble(
    x = c("b", "c", "a", "c", "b", "a", "a"),
    y = rnorm(7),
    z = 5
  )

  df_ordered <- order_col(df, "x", c("c", "b", "a")) |>
    dplyr::arrange(.data$x)
  df_ordered_desc <- order_col(df, "x", rev(c("c", "b", "a"))) |>
    dplyr::arrange(.data$x)

  expect_equal(
    df_ordered$x,
    factor(
      c("c", "c", "b", "b", "a", "a", "a"),
      levels = c("c", "b", "a"),
      ordered = TRUE
    )
  )

  expect_equal(
    df_ordered_desc$x,
    factor(
      c("a", "a", "a", "b", "b", "c", "c"),
      levels = c("a", "b", "c"),
      ordered = TRUE
    )
  )

  expect_error(
    order_col(df, "x", c("c", "b", "a", "a")),
    "duplicated"
  )
})
