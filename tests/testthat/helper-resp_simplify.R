resp_simplify_test <- function(shpdf, save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_shape")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$set_inputs(respSel = "resp_simplify")
  app$set_inputs("resp_simplify-distance" = 1000)
  app$click("resp_simplify-run")
  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)
  app$stop()
}
