test_that("get_parameter_display_name returns the expected display name for a known parameter", {
  expect_equal(
    get_parameter_display_name("log10_g"),
    "log10 genomes shed per\ninfected individual"
  )
})

test_that("get_parameter_display_name returns the raw parameter name for an known unknown parameter", {
  expect_equal(get_parameter_display_name("log10_gg"), "log10_gg")
})

test_that("get_parameter_display_name errors for non-string input", {
  expect_error(get_parameter_display_name(1), "string")
  expect_error(get_parameter_display_name(c("a", "b")), "length 1")
})
