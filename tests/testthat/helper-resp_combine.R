resp_combine_test <- function(shpdf_small, cdf_path, cdf_resp_column, cdf_area_column, shape_area_column, save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_combine")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_combine")
  app$upload_file("resp_combine-shape" = shpdf_small$datapath)
  app$upload_file("resp_combine-spread" = cdf_path)
  app$set_inputs("resp_combine-spread_response_column" = cdf_resp_column)
  app$set_inputs("resp_combine-spread_area_column" = cdf_area_column)
  app$set_inputs("resp_combine-shape_area_column" = shape_area_column)
  app$click("resp_combine-run")
  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)
  app$stop()
}
