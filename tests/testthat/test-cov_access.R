test_that("Check cov_access function works as expected", {
  result <- cov_access(shape, "Travel Time to Cities (2015)")
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_cov_access", {
  rerun_test_setup("cov_access_test", list(shpdf, save_path))
  common <- readRDS(save_path)
  expect_equal(length(common$covs), 1)
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
})
