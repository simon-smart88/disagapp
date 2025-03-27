pred_pred_test <- function(test_common_path, save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_fit_fit", timeout = 60000)
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file("core_load-run" = test_common_path)
  app$click("core_load-goLoad_session")

  app$set_inputs(tabs = "pred")
  app$set_inputs(predSel = "pred_pred")
  app$set_inputs("pred_pred-cases" = TRUE)
  app$set_inputs("pred_pred-iid" = TRUE)
  app$set_inputs("pred_pred-uncertain" = TRUE)
  app$click(selector = "#pred_pred-run")
  app$wait_for_value(input = "pred_pred-complete")
  app$set_inputs(main = "Save")
  tryCatch({app$get_download("core_save-save_session", filename = save_path)},
           finally = app$stop())
}
