
mad_shape <- resp_shape(shpdf)
lie_shape <- resp_download(df, area_column, resp_column, country_code[1], admin_level)

test_that("Check cov_upload function works as expected", {
  result <- cov_upload(covdf, mad_shape)
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 4)
})

test_that("Check cov_upload handles CRS issues", {

  covdf_nocrs <- data.frame(datapath = system.file("extdata", "test_data", "no_crs.tif", package="disagapp"),
                      name = "no_crs.tif")

  expect_error(cov_upload(covdf_nocrs, mad_shape), "Some files do not have a coordinate reference system")

  covdf_difcrs <- data.frame(datapath = system.file("extdata", "test_data", "different_projection.tif", package="disagapp"),
                            name = "different_projection.tif")

  result <- cov_upload(covdf_difcrs, mad_shape)
  result_crs <- terra::crs(result[[1]], describe = TRUE)
  expect_equal(result_crs$code, "4326")

  expect_error(cov_upload(covdf, lie_shape), "Some files do not overlap with the response data")

})

test_that("{shinytest2} recording: e2e_cov_upload", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_upload")

  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")

  if (is_ci){
    save_path <- tempfile(fileext = ".rds")
  } else {
    save_path <- "~/temprds/saved_file.rds"
  }

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 4)
})

