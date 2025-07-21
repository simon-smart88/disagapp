cdf_path <- file.path("data", "cdf.csv")
cdf <- read.csv(cdf_path)

cdf_area_column <- "area"
cdf_resp_column <- "response"
shape_area_column <- "Name_2"

test_that("Check resp_combine function works as expected", {
  result <- resp_combine(cdf, cdf_area_column, cdf_resp_column, shape, shape_area_column)
  expect_is(result, "sf")
  expect_equal(nrow(result), 5)
})

test_that("{shinytest2} recording: e2e_resp_combine", {
  skip_on_cran()

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_combine")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_combine")
  app$upload_file("resp_combine-shape" = shpdf_small$datapath)
  app$upload_file("resp_combine-spread" = cdf_path)
  app$set_inputs("resp_combine-spread_response_column" = cdf_resp_column)
  app$set_inputs("resp_combine-spread_area_column" = cdf_area_column)
  app$set_inputs("resp_combine-shape_area_column" = shape_area_column)
  app$click("resp_combine-run")

  shape <- app$get_value(export = "shape")
  expect_is(shape, "sf")
  expect_equal(nrow(shape), 5)
  app$stop()
})
