
df <- data.frame("area" = c("Triesen", "Schellenberg", "Gamprin", "Triesenberg",
                            "Eschen", "Ruggell", "Mauren", "Schaan", "Balzers",
                            "Planken","Vaduz"),
                 "response" = 1:11)

save_path <- "~/temprds/saved_file.rds"

test_that("{shinytest2} recording: e2e_complete_analysis", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_complete_analysis")

  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  app$upload_file("resp_download-spread" = "../../lie.csv")
  app$set_inputs(`resp_download-area_column` = "area")
  app$set_inputs(`resp_download-response_column` = "response")
  app$set_inputs(`resp_download-country` = "Liechtenstein")
  app$click("resp_download-run")

  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_bioclim")
  app$set_inputs(`cov_bioclim-variables` = c("Mean temperature", "Total precipitation"))
  app$click("cov_bioclim-run")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs <- unwrap_terra(common$covs)
  expect_length(common$covs, 2)

  app$set_inputs(tabs = "agg")
  app$set_inputs(aggSel = "agg_worldpop")
  app$set_inputs("agg_worldpop-method" = "Unconstrained")
  app$set_inputs("agg_worldpop-country" = "Liechtenstein")
  app$click("agg_worldpop-run")
  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$agg <- unwrap_terra(common$agg)
  expect_is(common$agg, "SpatRaster")

  app$set_inputs(tabs = "prep")
  app$set_inputs(prepSel = "prep_mesh")
  app$set_inputs("prep_mesh-mesh_edge" = c(0.1, 0.3))
  app$set_inputs("prep_mesh-mesh_offset" = c(0.1, 0.3))
  app$click("prep_mesh-run")
  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  expect_is(common$mesh, "inla.mesh")

  app$set_inputs(tabs = "prep")
  app$set_inputs(prepSel = "prep_summary")
  app$click("prep_summary-run")
  app$click("prep_summary-resample")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs_prep <- unwrap_terra(common$covs_prep)
  expect_length(common$covs_prep, 1)
  expect_is(common$covs_prep, "SpatRaster")

  app$set_inputs(prepSel = "prep_scale")
  app$click("prep_scale-run")

  app$set_inputs(prepSel = "prep_final")
  app$set_inputs(`prep_final-id_var` = "shapeName")
  app$click("prep_final-run")

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
  app$set_inputs(predSel = "pred_pred")
  app$click("pred_pred-run")
  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$pred$prediction <- unwrap_terra(common$pred$prediction)
  expect_is(common$pred$prediction, "SpatRaster")

})
