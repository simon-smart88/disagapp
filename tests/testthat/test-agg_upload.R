
test_that("Check agg_upload function works as expected", {
  mdg_shape <- resp_shape(shpdf)
  result <- agg_upload(mdg_shape, aggdf$datapath)
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_agg_upload", {
  rerun_test_setup("agg_upload_test", list(shpdf, covdf, aggdf, save_path))
  common <- readRDS(save_path)
  common$agg <- unwrap_terra(common$agg)
  expect_is(common$agg, "SpatRaster")
})

