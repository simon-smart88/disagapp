test_that("Check fit_fit function works as expected", {

  result <- fit_fit(data = test_data,
                    priors = NULL,
                    family = "poisson",
                    link = "log",
                    iterations = 100,
                    field = TRUE,
                    iid = TRUE,
                    async = FALSE)

  expect_is(result, "disag_model")
  expect_is(result$data$covariate_rasters, "SpatRaster")

  test_data$covariate_rasters <- terra::wrap(test_data$covariate_rasters)

  result <- fit_fit(data = test_data,
                    priors = NULL,
                    family = "poisson",
                    link = "log",
                    iterations = 100,
                    field = TRUE,
                    iid = TRUE,
                    async = TRUE)

  expect_is(result, "disag_model")
  expect_is(result$data$covariate_rasters, "PackedSpatRaster")

})

test_that("Check fit_fit function works in the app", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_fit_fit", timeout = 60000)
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file(`core_load-load_session` = test_common_path)
  app$click("core_load-goLoad_session")
  app$set_inputs(tabs = "fit")
  app$set_inputs(fitSel = "fit_fit")
  app$click(selector = "#fit_fit-run")
  app$wait_for_value(input = "fit_fit-complete")

  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_path)
  expect_is(common$fit, "disag_model")
})
