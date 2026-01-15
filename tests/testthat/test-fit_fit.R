test_that("Check fit_fit function works as expected", {

  skip_on_ci()

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
  skip_on_ci()
  skip_on_cran()
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_fit_fit", timeout = 60000)
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file("core_load-file" = test_common_path)
  app$click("core_load-run")
  app$set_inputs(tabs = "fit")
  app$set_inputs(fitSel = "fit_fit")
  app$click(selector = "#fit_fit-run")
  app$wait_for_value(input = "fit_fit-complete")
  fit <- app$get_value(export = "fit")
  expect_is(fit, "disag_model")
})
