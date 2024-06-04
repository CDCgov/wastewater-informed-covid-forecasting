testthat::test_that("End to end test of the pipeline. Should run without error.", {
  repo_path <- here::here()

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)

  #######
  # input
  #######
  repo_input <- fs::path(repo_path, "input")
  tmp_input <- fs::path(tmp_dir, "input")
  fs::dir_create(tmp_input)

  repo_locations <- fs::path(repo_input, "locations.csv")
  tmp_locations <- fs::path(tmp_input, "locations.csv")
  fs::file_copy(repo_locations, tmp_locations)

  repo_pmfs <- fs::path(repo_input, "saved_pmfs")
  tmp_pmfs <- fs::path(tmp_input, "saved_pmfs")
  fs::dir_copy(repo_pmfs, tmp_pmfs)

  repo_toml <- fs::path(repo_input, "params.toml")
  tmp_toml <- fs::path(tmp_input, "params.toml")
  fs::file_copy(repo_toml, tmp_toml)

  #######
  # src
  #######
  repo_src <- fs::path(repo_path, "src")
  tmp_src <- fs::path(tmp_dir, "src")
  fs::dir_copy(repo_src, tmp_src)

  #############
  ## binaries
  tmp_bin <- fs::path(tmp_dir, "bin")
  fs::dir_create(tmp_bin)

  #######
  # secrets
  #######
  repo_secrets <- fs::path(repo_path, "secrets.yaml")
  cfaforecastrenewalww::setup_secrets(repo_secrets)

  #######
  # diagnostic report
  #######
  repo_diag <- fs::path(repo_path, "model_diagnostics", "diagnostic_report.Rmd")
  tmp_diag <- fs::path(tmp_dir, "model_diagnostics", "diagnostic_report.Rmd")
  fs::dir_create(dirname(tmp_diag))
  fs::file_copy(repo_diag, tmp_diag)

  #######
  # targets file
  #######
  repo_targets <- fs::path(repo_path, "_targets.R")
  tmp_targets <- fs::path(tmp_dir, "_targets.R")
  targets_txt <- scan(
    repo_targets,
    what = character(),
    sep = "\n",
    blank.lines.skip = FALSE
  )

  targets_txt <- targets_txt[!grepl("setup_secrets", targets_txt)] |>
    paste0(collapse = "\n")
  stopifnot(
    "Cannot find NULL location in _targets.R to replace with 2-state test." =
      grepl("location = NULL,", targets_txt)
  )
  stopifnot(
    "Cannot find prod_run = FALSE in _targets.R to prod_run = TRUE." =
      grepl("prod_run = FALSE,", targets_txt)
  )
  targets_txt <- gsub("location = NULL,", "location = c(\"AK\", \"AL\"),", targets_txt)
  targets_txt <- gsub("prod_run = FALSE,", "prod_run = TRUE,", targets_txt)
  cat(targets_txt, file = tmp_targets)

  #######
  # try running pipeline
  #######
  testthat::expect_no_error(!!{
    withr::with_dir(tmp_dir, {
      targets::tar_make()
    })
  })

  #######
  # cleanup
  #######
  fs::dir_delete(tmp_dir)
})
