fit_fit_test <- function(test_common_path, save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_fit_fit", timeout = 60000)
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file("core_load-load_session" = test_common_path)
  app$click("core_load-goLoad_session")
  app$set_inputs(tabs = "fit")
  app$set_inputs(fitSel = "fit_fit")
  app$click(selector = "#fit_fit-run")
  app$wait_for_value(input = "fit_fit-complete")
  app$set_inputs(main = "Save")
  tryCatch({app$get_download("core_save-save_session", filename = save_path)},
           finally = app$stop())
}
