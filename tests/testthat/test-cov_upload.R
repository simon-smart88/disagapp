
test_that("Check cov_upload function works as expected", {
  result <- cov_upload(shape, covdf)
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 4)
})

test_that("Check cov_upload handles CRS issues", {

  covdf_difcrs <- data.frame(datapath = system.file("extdata", "test_data", "different_projection.tif", package="disagapp"),
                            name = "different_projection.tif")

  result <- cov_upload(mad_shape, covdf_difcrs)
  result_crs <- terra::crs(result[[1]], describe = TRUE)
  expect_equal(result_crs$code, "4326")

  expect_error(cov_upload(lie_shape, covdf), "Some files do not overlap with the response data")

})

test_that("{shinytest2} recording: e2e_cov_upload", {
  skip_on_cran()

  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")

  covs <- app$get_value(export = "covs")
  covs <- unwrap_terra(covs)
  expect_is(covs, "list")
  expect_equal(length(covs), 4)
  expect_is(covs[[1]], "SpatRaster")
  expect_is(covs[[2]], "SpatRaster")
  expect_is(covs[[3]], "SpatRaster")
  expect_is(covs[[4]], "SpatRaster")

  app$stop()

})

