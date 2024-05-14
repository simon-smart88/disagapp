
df <- data.frame("area" = c("Triesen", "Schellenberg", "Gamprin", "Triesenberg",
                            "Eschen", "Ruggell", "Mauren", "Schaan", "Balzers",
                            "Planken","Vaduz"),
                 "response" = 1:11)

save_path <- "~/temprds/saved_file.rds"

# not functioning yet
test_that("{shinytest2} recording: e2e_complete_analysis", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_complete_analysis")

  df_path <- tempfile(fileext = ".csv")
  write.csv(df, df_path)

  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  #app$upload_file("resp_download-spread" = df_path)
  app$upload_file("resp_download-spread" = "../../lie.csv")
  app$set_inputs(`resp_download-area_column` = "area")
  app$set_inputs(`resp_download-response_column` = "response")
  app$set_inputs(`resp_download-country` = "Liechtenstein")
  app$click("resp_download-run")

  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_access")
  app$click("cov_access-run")
  app$set_inputs(covSel = "cov_bioclim")
  app$set_inputs(`cov_bioclim-variables` = c("Mean temperature", "Total precipitation"))
  app$click("cov_bioclim-run")
  app$set_inputs(covSel = "cov_landuse")
  app$set_inputs(`cov_landuse-uses` = c("BuiltUp", "Crops"))
  app$click("cov_landuse-run")
  # exclude for now as no token
  # app$set_inputs(covSel = "cov_nightlight")
  # app$click("cov_nightlight-run")

  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs <- unwrap_terra(common$covs)
  expect_length(common$covs, 5)

  app$set_inputs(tabs = "agg")
  app$set_inputs(aggSel = "agg_worldpop")
  app$set_inputs(`agg_worldpop-method` = "Unconstrained")
  app$click("agg_worldpop-run")

  app$set_inputs(tabs = "prep")
  app$click("prep_summary-run")
  app$set_inputs(main = "Results")
  app$set_inputs(`prep_summary-original_covs_rows_selected` = 2)
  app$click("prep_summary-resample")
  app$click("prep_summary-rescale")
  app$set_inputs(prepSel = "prep_prep")
  app$set_inputs(`prep_prep-id_var` = "shapeID")
  app$click("prep_prep-run")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs_prep <- unwrap_terra(common$covs_prep)
  expect_length(common$covs_prep, 1)
  expect_is(common$covs_prep, "SpatRaster")
  expect_is(common$prep, "disag_data")

  app$set_inputs(tabs = "fit")
  app$click("fit_fit-run")
  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  expect_is(common$fit, "disag_model")

  app$set_inputs(tabs = "pred")
  app$click("pred_pred-run")
  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  expect_is(common$pred, "disag_prediction")

})
