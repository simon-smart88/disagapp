test_that("Check cov_landuse function works as expected", {
  result <- cov_landuse(shape, 2019, c("Crops"))
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
})

test_that("Check cov_landuse function returns errors", {
  expect_error(cov_landuse(123, 2019, c("Crops")), "shape must be an sf object")
  expect_error(cov_landuse(shape, 2019, c("banana")), "banana is not a valid land use type. ")
  expect_error(cov_landuse(shape, 2020, c("Crops")), "Land use data is only available between 2015 and 2019")
})

test_that("{shinytest2} recording: e2e_cov_bioclim", {
  skip_on_cran()

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_landuse", timeout = 60000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  app$upload_file("resp_download-spread" = df_path)
  app$set_inputs("resp_download-response_column" = resp_column)
  app$set_inputs("resp_download-area_column" = area_column)
  app$set_inputs("resp_download-country" = "Liechtenstein")
  app$set_inputs("resp_download-admin" = "ADM1")
  app$click("resp_download-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_landuse")
  app$set_inputs("cov_landuse-year" = 2019)
  app$set_inputs("cov_landuse-uses" = c("Crops"))
  app$click(selector = "#cov_landuse-run")
  app$wait_for_value(input = "cov_landuse-complete")

  covs <- app$get_value(export = "covs")
  covs <- unwrap_terra(covs)
  expect_is(covs, "list")
  expect_is(covs[[1]], "SpatRaster")
  expect_equal(length(covs), 1)
  app$stop()
})
