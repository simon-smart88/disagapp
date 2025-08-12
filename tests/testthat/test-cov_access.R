online <- check_url("https://data.malariaatlas.org/")

test_that("Check cov_access function works as expected", {
  skip_if_not(online)
  result <- cov_access(lie_shape, "Travel Time to Cities (2015)")
  expect_is(result, "SpatRaster")
})

test_that("Check cov_access function returns errors as expected", {
  expect_error(cov_access(123, "Travel Time to Cities (2015)"), "Shape must be an sf object")
  expect_error(cov_access(shape, 123), "layer must be a character string")
  expect_error(cov_access(shape, "layer"), "layer is not a valid accessibility layer")
})

test_that("{shinytest2} recording: e2e_cov_access", {
  skip_if_not(online)
  skip_on_cran()
  skip_on_os("windows")

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_access", timeout = 60000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = lie_shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "response")
  app$click("resp_shape-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_access")
  app$set_inputs("cov_access-layer" = "Travel Time to Cities (2015)")
  app$click(selector = "#cov_access-run")
  app$wait_for_value(input = "cov_access-complete")

  covs <- app$get_value(export = "covs")
  covs <- unwrap_terra(covs)
  expect_is(covs, "list")
  expect_is(covs[[1]], "SpatRaster")
  app$stop()
})
