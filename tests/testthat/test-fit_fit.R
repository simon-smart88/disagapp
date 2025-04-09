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
  rerun_test_setup("fit_fit_test", list(test_common_path, save_path))
  common <- readRDS(save_path)
  expect_is(common$fit, "disag_model")
})
