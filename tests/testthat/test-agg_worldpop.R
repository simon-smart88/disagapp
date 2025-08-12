
lie_che_shape <- resp_download(mdf, area_column, resp_column, country_code, admin_level)

test_that("Check agg_worldpop function works as expected for single country", {

  result <- agg_worldpop(lie_shape, country_code[1], "Constrained", "100m", 2020)
  expect_is(result, "SpatRaster")

  result <- agg_worldpop(lie_shape, country_code[1], "Constrained", "1km", 2020)
  expect_is(result, "SpatRaster")

  result <- agg_worldpop(lie_shape, country_code[1], "Unconstrained", "100m", 2000)
  expect_is(result, "SpatRaster")

  result <- agg_worldpop(lie_shape, country_code[1], "Unconstrained", "1km", 2000)
  expect_is(result, "SpatRaster")

  result <- agg_worldpop(lie_shape, country_code[1], "Unconstrained", "1km", 2020)
  expect_is(result, "SpatRaster")
})


test_that("Check agg_worldpop function works as expected for multiple countries", {
  result <- agg_worldpop(lie_che_shape, country_code, "Constrained", "1km", 2020)
  expect_is(result, "SpatRaster")
  n_cells <- terra::ncell(result)
  expect_gt(n_cells, 50000)
})


test_that("Check agg_worldpop function returns errors", {
  expect_error(agg_worldpop(lie_shape, country_code[1], "aaaa", "1km", 2012), "Method must be either \"Constrained\" or \"Unconstrained\"")
  expect_error(agg_worldpop(lie_shape, country_code[1], "Constrained", "aaaa", 2020),"Resolution must be either \"100m\" or \"1km\"")
  expect_error(agg_worldpop(lie_shape, country_code[1], "Constrained", "1km", 2012), "Constrained population data is only available for 2020")
  expect_error(agg_worldpop(lie_shape, country_code[1], "Unconstrained", "1km", 1999), "Unconstrained data is only available between 2000 and 2020")
  expect_error(agg_worldpop(lie_shape, "ZZZ", "Unconstrained", "1km", 2000), "ZZZ is not a valid IS03 country code.")
})

test_that("{shinytest2} recording: e2e_agg_worldpop", {
  skip_on_cran()
  skip_on_os("windows")

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_agg_worldpop", timeout = 30000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = lie_shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "response")
  app$click("resp_shape-run")

  app$set_inputs("agg_worldpop-method" = "Unconstrained")
  app$set_inputs("agg_worldpop-year" = 2020)
  app$set_inputs("agg_worldpop-resolution" = "1km")
  app$set_inputs("agg_worldpop-country" = "Liechtenstein")
  app$click(selector = "#agg_worldpop-run")
  app$wait_for_value(input = "agg_worldpop-complete")

  agg <- app$get_value(export = "agg")
  agg <- unwrap_terra(agg)
  expect_is(agg, "SpatRaster")
  app$stop()
})

