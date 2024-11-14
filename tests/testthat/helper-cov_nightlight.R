cov_nightlight_test <- function(shpdf, save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_nightlight", timeout = 120000)
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_nightlight")
  app$set_inputs("cov_nightlight-year" = "2022")
  app$click(selector = "#cov_nightlight-run")
  app$wait_for_value(input = "cov_nightlight-complete")

  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)
  app$stop()
}
