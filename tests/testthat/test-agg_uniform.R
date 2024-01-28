covdf <- data.frame(datapath = list.files(system.file("extdata/covariates", package="disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata/covariates", package="disagapp")))

test_that("Check agg_uniform function works as expected", {
  cov <- terra::rast(covdf$datapath[1])
  result <- agg_uniform(cov)
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_agg_uniform", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_agg_uniform")
  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")
  app$click("agg_uniform-run")
  common <- app$get_value(export = "common")
  expect_is(common$agg, "SpatRaster")
})
