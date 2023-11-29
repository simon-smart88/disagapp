shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="shinydisag"), full.names = TRUE),
                    name = list.files(system.file("extdata/shapes", package="shinydisag")))

test_that("Check resp_edit function works as expected", {
  shape <- resp_shape(shpdf)
  poly <- matrix(c(40, 40, 55, 55, 40, -10, -20, -20, -10, -10), ncol = 2)
  colnames(poly) <- c("longitude", "latitude")
  result <- resp_edit(shape, poly, "Outside")
  expect_is(result, "sf")
  expect_equal(nrow(result), 47)
  result <- resp_edit(shape, poly, "Inside")
  expect_is(result, "sf")
  expect_equal(nrow(result), 62)
})

test_that("{shinytest2} recording: e2e_resp_shape", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "shinydisag"), name = "e2e_resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$set_inputs("resp_edit-type" = "Outside")
  app$click("resp_edit-run")
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")
  expect_equal(nrow(common$shape), 47)
})
