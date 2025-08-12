mad_shape <- resp_shape(shpdf)
mad_shape_size <- as.numeric(object.size(mad_shape))

test_that("Check resp_simplify function works as expected", {
  result <- resp_simplify(mad_shape, 1000)
  expect_is(result, "sf")
  expect_equal(nrow(result), 109)
  simple_size <- as.numeric(object.size(result))
  expect_lt(simple_size, mad_shape_size)
})

test_that("{shinytest2} recording: e2e_resp_simplify", {
  skip_on_cran()
  skip_on_os("windows")

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_simplify")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$set_inputs(respSel = "resp_simplify")
  app$set_inputs("resp_simplify-distance" = 1000)
  app$click("resp_simplify-run")

  shape <- app$get_value(export = "shape")
  expect_is(shape, "sf")
  expect_equal(nrow(shape), 109)
  simple_size <- as.numeric(object.size(shape))
  expect_lt(simple_size, mad_shape_size)
  app$stop()
})
