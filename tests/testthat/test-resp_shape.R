shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="shinydisag"), full.names = TRUE),
                    name = list.files(system.file("extdata/shapes", package="shinydisag")))

test_that("Check resp_shape function works as expected", {
  result <- resp_shape(shpdf)
  expect_is(result, "sf")
  expect_equal(nrow(result), 109)
})

test_that("{shinytest2} recording: e2e_resp_shape", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "shinydisag"), name = "e2e_resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")
  expect_equal(nrow(common$shape), 109)
})
