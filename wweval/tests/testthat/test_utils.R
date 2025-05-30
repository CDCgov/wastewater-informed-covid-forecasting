test_that(
  paste0(
    "to_rds_with_suffix uses object name if ",
    "save_basename is not provided, ",
    "uses save_basename if provided, ",
    "and respects output_dir, ",
    "save_suffix, and ext values"
  ),
  {
    mock_object <- list(a = 1, b = 2)

    withr::with_tempdir({
      temp_dir <- getwd()
      temp_subdir <- fs::path(temp_dir, "test")
      fs::dir_create(temp_subdir)
      expected_path_one <- fs::path(temp_subdir, "mock_object.rds")
      expect_false(fs::file_exists(expected_path_one))
      to_rds_with_suffix(mock_object, temp_subdir)
      expect_true(fs::file_exists(expected_path_one))

      custom_basename <- "custom_name"
      expected_path_two <- fs::path(temp_subdir, "custom_name.rds")
      expect_false(fs::file_exists(expected_path_two))
      to_rds_with_suffix(
        mock_object,
        temp_subdir,
        save_basename = custom_basename
      )
      expect_true(fs::file_exists(expected_path_two))

      custom_suffix <- "_suffix"
      expected_path_three <- fs::path(temp_subdir, "mock_object_suffix.rds")
      expect_false(fs::file_exists(expected_path_three))
      to_rds_with_suffix(mock_object, temp_subdir, save_suffix = custom_suffix)
      expect_true(fs::file_exists(expected_path_three))

      custom_ext <- "custom_ext"
      expected_path_four <- fs::path(temp_subdir, "mock_object.custom_ext")
      expect_false(fs::file_exists(expected_path_four))
      to_rds_with_suffix(mock_object, temp_subdir, ext = custom_ext)
      expect_true(fs::file_exists(expected_path_four))
    })
  }
)
