
test_that("Check cov_upload function works as expected", {
  result <- cov_upload(shape, covdf)
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 4)
})

test_that("Check cov_upload errors when non-tif files are uploaded", {
  txtdf <- data.frame(datapath = file.path(test_data_dir, "txt.txt"),
                      name = "txt.txt")
  expect_error(cov_upload(shape, txtdf), "txt.txt is not a .tif file")
})

test_that("Check cov_upload errors when a multi-layered SpatRaster is uploaded", {
  multilayer <- terra::rast(list.files(system.file("extdata", "covariates", package="disagapp"), full.names = TRUE))
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(multilayer, tmp)
  mdf <- data.frame(datapath = tmp, name = "multilayer.tif")
  expect_error(cov_upload(shape, mdf), "multilayer.tif contains multiple layers")
})


test_that("Check cov_upload can handle unnamed rasters", {
  no_name_raster <- terra::rast(system.file("extdata", "covariates", "Mean_temperature.tif", package="disagapp"))
  names(no_name_raster) <- ""
  tmp <- tempfile(fileext = ".tif")
  tmp_name <- tools::file_path_sans_ext(basename(tmp))
  terra::writeRaster(no_name_raster, tmp)
  nndf <- data.frame(datapath = tmp, name = "no_name.tif")
  result <- cov_upload(shape, nndf)
  expect_named(result, tmp_name)
  expect_named(result[[1]], tmp_name)
})


test_that("Check cov_upload handles CRS issues", {

  covdf_difcrs <- data.frame(datapath = file.path(test_data_dir, "different_projection.tif"),
                            name = "different_projection.tif")

  result <- cov_upload(shape, covdf_difcrs)
  result_crs <- terra::crs(result[[1]], describe = TRUE)
  expect_equal(result_crs$code, "4326")

  expect_error(cov_upload(lie_shape, covdf), "Some files do not overlap with the response data")

})

test_that("{shinytest2} recording: e2e_cov_upload", {
  skip_on_cran()
  skip_on_os("windows")

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_upload", timeout = 60000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
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

