test_that("Check cov_nightlight function works as expected", {
  result <- cov_nightlight(lie_shape, "LIE", 2022)
  expect_is(result, "SpatRaster")
})

test_that("Check cov_nightlight function works as expected for multiple countries", {
  shape <- resp_download(mdf, area_column, resp_column, country_code, admin_level)
  result <- cov_nightlight(shape, country_code, 2022)
  expect_is(result, "SpatRaster")
  n_cells <- terra::ncell(result)
  expect_gt(n_cells, 100000)
})

test_that("Check cov_nightlight function returns errors as expected", {
  expect_error(cov_nightlight(lie_shape, "ZZZ", 2022), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_nightlight(lie_shape, c("ZZZ", "LIE"), 2022), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_nightlight(lie_shape, "LIE", "chicken"), "Nighttime data is only available between 2015 and 2023")
  expect_error(cov_nightlight(lie_shape, "LIE", 2000), "Nighttime data is only available between 2015 and 2023")
  expect_error(cov_nightlight(123, country_code[1], 2022), "Shape must be an sf object")
  expect_error(cov_nightlight(shape, "LIE", 2022), "The downloaded nightlight data does not overlap with the response data")
})

test_that("{shinytest2} recording: e2e_cov_nightlight", {
  skip_on_cran()
  skip_on_os("windows")

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_nightlight", timeout = 60000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = lie_shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "response")
  app$click("resp_shape-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_nightlight")
  app$set_inputs("cov_nightlight-year" = "2022")
  app$set_inputs("cov_nightlight-country" = "Liechtenstein")
  app$click(selector = "#cov_nightlight-run")
  app$wait_for_value(input = "cov_nightlight-complete")

  covs <- app$get_value(export = "covs")
  covs <- unwrap_terra(covs)
  expect_is(covs, "list")
  expect_is(covs[[1]], "SpatRaster")
  app$stop()
})
