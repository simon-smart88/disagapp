
test_that("Check agg_upload function works as expected", {
  mdg_shape <- resp_shape(shpdf)
  result <- agg_upload(mdg_shape, aggdf$datapath)
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_agg_upload", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_upload")

  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_upload")
  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")

  app$set_inputs(tabs = "agg")
  app$set_inputs(aggSel = "agg_upload")
  app$upload_file("agg_upload-agg" = aggdf$datapath)
  app$click("agg_upload-run")

  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_path)
  common$agg <- unwrap_terra(common$agg)
  expect_is(common$agg, "SpatRaster")

  app$stop()

  })

