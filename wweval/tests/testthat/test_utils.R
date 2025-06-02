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
