
test_that("Check agg_upload function works as expected", {
  mdg_shape <- resp_shape(shpdf)
  result <- agg_upload(mdg_shape, aggdf$datapath, "name")
  expect_is(result, "SpatRaster")
  expect_named(result, "name")
})

test_that("Check agg_upload errors when a multi-layered SpatRaster is uploaded", {
  multilayer <- terra::rast(list.files(system.file("extdata", "covariates", package="disagapp"), full.names = TRUE))
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(multilayer, tmp)
  mdf <- data.frame(datapath = tmp, name = "multilayer.tif")
  expect_error(agg_upload(shape, mdf$datapath, "name"), "The uploaded file contains multiple layers")
})


test_that("{shinytest2} recording: e2e_agg_upload", {
  skip_on_cran()
  skip_on_os("windows")

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_agg_upload")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")

  app$upload_file("agg_upload-agg" = aggdf$datapath)
  app$click("agg_upload-run")

  agg <- app$get_value(export = "agg")
  agg <- unwrap_terra(agg)
  expect_is(agg, "SpatRaster")
  expect_named(agg, "Population")
  app$stop()
})

