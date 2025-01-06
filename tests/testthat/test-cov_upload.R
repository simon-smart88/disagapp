
mad_shape <- resp_shape(shpdf)
lie_shape <- resp_download(df, area_column, resp_column, country_code[1], admin_level)

test_that("Check cov_upload function works as expected", {
  result <- cov_upload(mad_shape, covdf)
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 4)
})

test_that("Check cov_upload handles CRS issues", {

  covdf_nocrs <- data.frame(datapath = system.file("extdata", "test_data", "no_crs.tif", package="disagapp"),
                      name = "no_crs.tif")

  expect_error(cov_upload(mad_shape, covdf_nocrs), "Some files do not have a coordinate reference system")

  covdf_difcrs <- data.frame(datapath = system.file("extdata", "test_data", "different_projection.tif", package="disagapp"),
                            name = "different_projection.tif")

  result <- cov_upload(mad_shape, covdf_difcrs)
  result_crs <- terra::crs(result[[1]], describe = TRUE)
  expect_equal(result_crs$code, "4326")

  expect_error(cov_upload(lie_shape, covdf), "Some files do not overlap with the response data")

})

test_that("{shinytest2} recording: e2e_cov_upload", {
  rerun_test_setup("cov_upload_test", list(shpdf, covdf, save_path))
  common <- readRDS(save_path)
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 4)
})

