
test_that("Check cov_nightlight function works as expected", {
  result <- cov_nightlight(shape, 2022, Sys.getenv("NASA_bearer"))
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_cov_nightlight", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_nightlight")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_nightlight")
  app$set_inputs("cov_nightlight-year" = "2022")
  app$click("cov_nightlight-run")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs <- unwrap_terra(common$covs)

  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
