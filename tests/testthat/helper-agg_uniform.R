agg_uniform_test <- function(shpdf, covdf, save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_agg_uniform")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")
  app$click("agg_uniform-run")
  app$set_inputs(main = "Save")
  tryCatch({app$get_download("core_save-save_session", filename = save_path)},
           finally = app$stop())
}
