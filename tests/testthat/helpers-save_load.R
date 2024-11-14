empty_save_test <- function(save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_empty_save")
  app$set_inputs(tabs = "resp")
  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)
  app$stop()
}

shape_save_test <- function(shpdf, save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_shape_save")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)
  app$stop()
}

settings_save_test <- function(save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_settings_save")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_example")
  app$set_inputs("resp_example-dataset" = "scot")
  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_access")
  app$set_inputs("cov_access-layer" = "Motorized Travel Time to Healthcare (2020)")
  app$set_inputs(covSel = "cov_landuse")
  app$set_inputs("cov_landuse-uses" = c("Crops", "PermanentWater", "SeasonalWater"))
  app$set_inputs("cov_landuse-year" = "2017")
  app$set_inputs(tabs = "agg")
  app$set_inputs(aggSel = "agg_worldpop")
  app$set_inputs("agg_worldpop-country" = "Liechtenstein")
  app$set_inputs("agg_worldpop-resolution" = "100m")
  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)
  app$stop()
}
