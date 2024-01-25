aggdf <- data.frame(datapath = list.files(system.file("extdata/aggregation", package="disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata/aggregation", package="disagapp")))

test_that("Check agg_upload function works as expected", {
  result <- agg_upload(aggdf$datapath)
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_agg_upload", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_upload")
  app$upload_file("agg_upload-agg" = aggdf$datapath)
  app$click("agg_upload-run")
  common <- app$get_value(export = "common")
  expect_is(common$agg, "SpatRaster")
})
