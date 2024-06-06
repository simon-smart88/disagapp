test_that("Check cov_landuse function works as expected", {
  result <- cov_landuse(shape, 2019, c("Crops"))
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
})

#not yet working
test_that("{shinytest2} recording: e2e_cov_landuse", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_landuse", timeout = 60000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

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
