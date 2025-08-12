test_that("Check resp_edit function works as expected", {
  mad_shape <- resp_shape(shpdf)
  poly <- matrix(c(40, 40, 55, 55, 40, -10, -20, -20, -10, -10), ncol = 2)
  colnames(poly) <- c("longitude", "latitude")
  result <- resp_edit(mad_shape, poly, "outside")
  expect_is(result, "sf")
  expect_lt(nrow(result), 50)
  result <- resp_edit(mad_shape, poly, "inside")
  expect_is(result, "sf")
  expect_gt(nrow(result), 60)
})

test_that("{shinytest2} recording: e2e_resp_edit", {
  skip_on_cran()
  skip_on_os("windows")

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_shape")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$set_inputs(respSel = "resp_edit")
  app$set_inputs("resp_edit-type" = "outside")
  app$click("resp_edit-run")

  shape <- app$get_value(export = "shape")
  expect_is(shape, "sf")
  expect_lt(nrow(shape), 50)
  app$stop()
})
