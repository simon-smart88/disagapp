test_that("Check cov_water function works as expected", {
  result <- cov_water(lie_shape, "LIE")
  expect_is(result, "SpatRaster")
})

test_that("Check cov_water function works as expected for multiple countries", {
  shape <- resp_download(mdf, area_column, resp_column, country_code, admin_level)
  result <- cov_water(shape, country_code)
  expect_is(result, "SpatRaster")
  n_cells <- terra::ncell(result)
  expect_gt(n_cells, 100000)
})

test_that("Check cov_nightlight function returns errors as expected", {
  expect_error(cov_water(lie_shape, "ZZZ"), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_water(lie_shape, c("ZZZ", "LIE")), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_water(123, country_code[1]), "Shape must be an sf object")
  expect_error(cov_water(shape, "LIE"), "The downloaded nightlight data does not overlap with the response data")
})

test_that("{shinytest2} recording: e2e_cov_water", {
  skip_on_cran()

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_water", timeout = 60000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = lie_shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "response")
  app$click("resp_shape-run")

  app$set_inputs("cov_water-country" = "Liechtenstein")
  app$click(selector = "#cov_water-run")
  app$wait_for_value(input = "cov_water-complete")

  covs <- app$get_value(export = "covs")
  covs <- unwrap_terra(covs)
  expect_is(covs, "list")
  expect_is(covs[[1]], "SpatRaster")
  expect_equal(length(covs), 1)
  app$stop()

})
