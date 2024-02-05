test_that("Make sure filtering of NWSS data does what we expect.", {
  quiet({
    ww_data_raw <- get_ww_data(
      ww_data_source = "NWSS",
      geo_type = "state",
      ww_data_type = "pop_weighted_conc_w_thres",
      ww_target_type = "pcr_target_avg_conc",
      ww_geo_type = "site",
      ww_data_path = system.file("testdata",
        "fake_nwss.csv",
        package = "cfaforecastrenewalww"
      )
    )
  })

  # Make sure we've got the columns we expect, and removed the junk
  testthat::expect_setequal(
    names(ww_data_raw),
    c(
      "date", "location", "ww", "site", "lab",
      "lab_wwtp_unique_id", "ww_pop", "below_LOD", "lod_sewage"
    )
  )

  # Make sure the fake states were filtered out, as all their data should have been removed
  testthat::expect_setequal(
    ww_data_raw %>%
      pull(location) %>%
      unique(),
    c("CA", "WA", "NM", "US")
  )

  # LOD filtering
  below_lod <- ww_data_raw %>%
    filter(below_LOD == 1)
  not_below_lod <- ww_data_raw %>%
    filter(below_LOD == 0)

  testthat::expect_equal(
    below_lod$ww,
    below_lod$lod_sewage
  )

  testthat::expect_true(
    all(not_below_lod$ww >= not_below_lod$lod_sewage)
  )
})
