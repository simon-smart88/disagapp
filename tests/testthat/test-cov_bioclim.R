test_that("Check cov_bioclim function works as expected", {
  result <- cov_bioclim("LIE", c("Mean temperature", "Mean diurnal range"))
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 2)
})

test_that("Check cov_bioclim function returns errors as expected", {
  expect_error(cov_bioclim("LIE", "chicken"), "chicken is not a valid bioclim variable")
})

test_that("{shinytest2} recording: e2e_cov_nightlight", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_nightlight")
  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_bioclim")
  app$set_inputs("cov_bioclim-country" = "Liechtenstein")
  app$set_inputs("cov_bioclim-variables" = c("Mean temperature", "Mean diurnal range"))
  app$click("cov_bioclim-run")
  common <- app$get_value(export = "common")
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 2)
})
