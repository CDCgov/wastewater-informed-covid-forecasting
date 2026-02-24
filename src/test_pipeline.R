e2e_result <- testthat::test_file(here::here(
  "src",
  "tests",
  "test_end_to_end.R"
))

if (inherits(e2e_result[[1]]$results[[1]], "error")) {
  message("test failure")
  quit(save = "no", status = 1)
}
