
test_that("Check resp_shape function works as expected", {
  result <- resp_shape(shpdf)
  expect_is(result, "sf")
  expect_equal(nrow(result), 109)
})

test_that("{shinytest2} recording: e2e_resp_shape", {
  skip_on_cran()
  skip_on_ci()
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_shape", timeout = 10000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  shape <- app$get_value(export = "shape")
  expect_is(shape, "sf")
  expect_equal(nrow(shape), 109)
  app$stop()
})
