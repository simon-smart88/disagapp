
test_that("Check agg_uniform function works as expected", {
  cov <- terra::rast(covdf$datapath[1])
  result <- agg_uniform(cov)
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_agg_uniform", {
  skip_on_cran()

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_agg_uniform")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")

  app$click("agg_uniform-run")

  agg <- app$get_value(export = "agg")
  agg <- unwrap_terra(agg)
  expect_is(agg, "SpatRaster")
  app$stop()
})
