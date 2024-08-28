
# this works
test_that("{shinytest2} recording: e2e_empty_markdown", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_empty_markdown")
  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  sess_file <- app$get_download("rep_markdown-dlRMD")
  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  writeLines(lines, sess_file)
  rmarkdown::render(sess_file)
  html_file <- gsub("Rmd","html", sess_file)
  expect_gt(file.info(html_file)$size, 1000)

  # leave this for now
  app$set_inputs(repSel = "rep_refPackages")
  app$set_inputs(refFileType = "HTML")
  ref_file <- app$get_download("dlrefPackages")
  expect_gt(file.info(ref_file)$size, 10000)
  })

# this is very temperamental
test_that("{shinytest2} recording: e2e_markdown_from_complete_analysis", {

  skip_on_ci()

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_complete_analysis", timeout = 120000)

  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  app$upload_file("resp_download-spread" = df_path)
  app$set_inputs(`resp_download-area_column` = "area")
  app$set_inputs(`resp_download-response_column` = "response")
  app$set_inputs(`resp_download-country` = "Liechtenstein")
  app$click("resp_download-run")

  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_bioclim")
  app$set_inputs(`cov_bioclim-variables` = c("Mean temperature", "Total precipitation"))
  app$click(selector = "#cov_bioclim-run")
  app$wait_for_value(input = "cov_bioclim-complete")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs <- unwrap_terra(common$covs)
  expect_length(common$covs, 2)

  app$set_inputs(tabs = "agg")
  app$set_inputs(aggSel = "agg_worldpop")
  app$set_inputs("agg_worldpop-method" = "Unconstrained")
  app$set_inputs("agg_worldpop-country" = "Liechtenstein")
  app$click(selector = "#agg_worldpop-run")
  app$wait_for_value(input = "agg_worldpop-complete")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$agg <- unwrap_terra(common$agg)
  expect_is(common$agg, "SpatRaster")

  app$set_inputs(tabs = "prep")
  app$set_inputs(prepSel = "prep_mesh")
  app$set_inputs("prep_mesh-mesh_edge" = c(0.1, 0.3))
  app$set_inputs("prep_mesh-mesh_offset" = c(0.1, 0.3))
  app$click(selector = "#prep_mesh-run")
  app$wait_for_value(input = "prep_mesh-complete")
  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  expect_is(common$mesh, "inla.mesh")

  app$set_inputs(tabs = "prep")
  app$set_inputs(prepSel = "prep_summary")
  app$click("prep_summary-run")
  app$set_inputs("prep_summary-resample_layer" = "Mean temperature")
  app$click("prep_summary-resample")

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
  common$pred$`prediction (rate)` <- unwrap_terra(common$pred$`prediction (rate)`)
  expect_is(common$pred$`prediction (rate)`, "SpatRaster")

  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  sess_file <- app$get_download("rep_markdown-dlRMD")
  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)
  target_line <- grep("response_directory <- ", lines)
  lines[target_line] <- 'response_directory <- system.file("extdata", "test_data", package="disagapp")'
  writeLines(lines, sess_file)

  rmarkdown::render(sess_file)
  html_file <- gsub("Rmd", "html", sess_file)
  expect_gt(file.info(html_file)$size, 1000)

})
#
# system.file("extdata/shapes", package="disagapp")
#
#
# # test_that("{shinytest2} recording: e2e_table_download", {
# #   app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "shinyscholar"), name = "e2e_table_download")
# #   app$set_inputs(tabs = "select")
# #   app$set_inputs(selectSel = "select_user")
# #   app$upload_file(`select_user-ras` = path)
# #   app$set_inputs(`select_user-name` = "bio")
# #   app$click("select_user-run")
# #   app$set_inputs(main = "Table")
# #   table_file <- app$get_download("dl_table")
# #   df <- read.csv(table_file)
# #   expect_equal(nrow(df),100)
# #   })
# #
# # test_that("{shinytest2} recording: e2e_plot_downloads", {
# #   app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "shinyscholar"), name = "e2e_plot_downloads")
# #   app$set_inputs(tabs = "select")
# #   app$set_inputs(selectSel = "select_user")
# #   app$upload_file(`select_user-ras` = path)
# #   app$set_inputs(`select_user-name` = "bio")
# #   app$click("select_user-run")
# #   app$set_inputs(tabs = "plot")
# #   app$set_inputs(plotSel = "plot_scatter")
# #   app$click("plot_scatter-run")
# #   app$set_inputs(main = "Save")
# #   scatter_file <- app$get_download("dl_scatter")
# #
# #   app$set_inputs(plotSel = "plot_hist")
# #   app$set_inputs(`plot_hist-pal` = "YlOrRd")
# #   app$click("plot_hist-run")
# #   app$set_inputs(main = "Save")
# #   hist_file <- app$get_download("dl_hist")
# #
# #   expect_gt(file.info(scatter_file)$size, 1000)
# #   expect_gt(file.info(hist_file)$size, 1000)
# #
# # })
#
