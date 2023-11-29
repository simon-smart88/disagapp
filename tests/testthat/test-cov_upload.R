covdf <- data.frame(datapath = list.files(system.file("extdata/covariates", package="shinydisag"), full.names = TRUE),
                    name = list.files(system.file("extdata/covariates", package="shinydisag")))

test_that("Check cov_upload function works as expected", {
  result <- cov_upload(covdf)
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 4)
})

test_that("{shinytest2} recording: e2e_cov_upload", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "shinydisag"), name = "e2e_cov_upload")
  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")
  common <- app$get_value(export = "common")
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 4)
})
