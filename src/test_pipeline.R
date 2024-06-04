e2e_result <- testthat::test_file(here::here("src", "tests", "test_end_to_end.R"))

if ("error" %in% class(e2e_result[[1]]$results[[1]])) {
  message("test failure")
  quit(save = "no", status = 1)
}
