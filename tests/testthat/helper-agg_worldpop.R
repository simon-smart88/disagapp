agg_worldpop_test <- function(df_path, resp_column, area_column, save_path){
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_agg_worldpop", timeout = 30000)

  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  app$upload_file("resp_download-spread" = df_path)
  app$set_inputs("resp_download-response_column" = resp_column)
  app$set_inputs("resp_download-area_column" = area_column)
  app$set_inputs("resp_download-country" = "Liechtenstein")
  app$set_inputs("resp_download-admin" = "ADM1")
  app$click("resp_download-run")

  app$set_inputs("agg_worldpop-method" = "Unconstrained")
  app$set_inputs("agg_worldpop-year" = 2020)
  app$set_inputs("agg_worldpop-resolution" = "1km")
  app$set_inputs("agg_worldpop-country" = "Liechtenstein")
  app$click(selector = "#agg_worldpop-run")
  app$wait_for_value(input = "agg_worldpop-complete")

  app$set_inputs(main = "Save")
  tryCatch({app$get_download("core_save-save_session", filename = save_path)},
           finally = app$stop())
}
