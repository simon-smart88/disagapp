agg_upload_test <- function(shpdf, covdf, aggdf, save_path){
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
  tryCatch({app$get_download("core_save-save_session", filename = save_path)},
           finally = app$stop())
}
