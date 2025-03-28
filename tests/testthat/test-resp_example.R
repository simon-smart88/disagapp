
test_that("Check resp_example function works as expected", {
  result <- resp_example("mad")
  expect_is(result, "sf")
  expect_equal(nrow(result), 109)
})

test_that("{shinytest2} recording: e2e_resp_example", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_shape", timeout = 10000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_example")
  app$click("resp_example-run")
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")
  expect_equal(nrow(common$shape), 109)
  app$stop()
})
