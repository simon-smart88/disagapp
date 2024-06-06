# test_that("Check cov_water function works as expected", {
#   result <- cov_water(shape, arcgisutils::auth_client())
#   expect_is(result, "SpatRaster")
#
#   expect_error(cov_water(shpdf, arcgisutils::auth_client()), "Shape must be an sf object")
#   expect_error(cov_water(shape, "token"), "Token must be an httr2_token")
# })

test_that("{shinytest2} recording: e2e_cov_water", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_nightlight", timeout = 30000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_water")
  app$click(selector = "#cov_water-run")
  app$wait_for_value(input = "cov_water-complete")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs <- unwrap_terra(common$covs)

  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
