
test_that("Check agg_uniform function works as expected", {
  cov <- terra::rast(covdf$datapath[1])
  result <- agg_uniform(cov)
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_agg_uniform", {
  skip_on_cran()
  skip_on_ci()
  rerun_test_setup("agg_uniform_test", list(shpdf, covdf, save_path))
  common <- readRDS(save_path)
  common$agg <- unwrap_terra(common$agg)
  expect_is(common$agg, "SpatRaster")
})
