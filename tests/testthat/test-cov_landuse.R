test_that("Check cov_landuse function works as expected", {
  result <- cov_landuse(shape, 2019, c("Crops"))
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
})

test_that("Check cov_landuse function returns errors", {
  expect_error(cov_landuse(123, 2019, c("Crops")), "Shape must be an sf object")
  expect_error(cov_landuse(shape, 2019, c("banana")), "banana is not a valid land use type. ")
  expect_error(cov_landuse(shape, 2020, c("Crops")), "Land use data is only available between 2015 and 2019")
})

test_that("{shinytest2} recording: e2e_cov_landuse", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_landuse", timeout = 600000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  app$upload_file("resp_download-spread" = df_path)
  app$set_inputs("resp_download-response_column" = resp_column)
  app$set_inputs("resp_download-area_column" = area_column)
  app$set_inputs("resp_download-country" = "Liechtenstein")
  app$set_inputs("resp_download-admin" = "ADM1")
  app$click("resp_download-run")

  # not needed but save fails without this
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_landuse")
  app$set_inputs("cov_landuse-year" = 2019)
  app$set_inputs("cov_landuse-uses" = c("Crops"))
  app$click(selector = "#cov_landuse-run")
  app$wait_for_value(input = "cov_landuse-complete")

  # not needed but save fails without this
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
