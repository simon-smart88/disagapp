
# this works
test_that("{shinytest2} recording: e2e_empty_markdown", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_empty_markdown")
  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  sess_file <- app$get_download("dlRMD")
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
test_that("{shinytest2} recording: e2e_markdown_from_uploads", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_markdown_from_uploads")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_upload")
  app$upload_file("cov_upload-cov" = covdf$datapath)
  app$click("cov_upload-run")

  app$set_inputs(tabs = "agg")
  app$set_inputs(aggSel = "agg_upload")
  app$upload_file("agg_upload-agg" = aggdf$datapath)
  app$click("agg_upload-run")

  app$set_inputs(tabs = "prep")
  app$set_inputs(prepSel = "prep_mesh")
  app$set_inputs("prep_mesh-mesh_edge" = c(0.1, 0.3))
  app$set_inputs("prep_mesh-mesh_offset" = c(0.1, 0.3))
  app$click("prep_mesh-run")

  app$set_inputs(prepSel = "prep_summary")
  app$click("prep_summary-run")
  app$click("prep_summary-resample")

  app$set_inputs(prepSel = "prep_scale")
  app$click("prep_scale-run")

  app$set_inputs(tabs = "prep")
  app$set_inputs(prepSel = "prep_final")
  app$set_inputs("prep_final-id_var" = "ID_2")
  app$set_inputs("prep_final-resp_var" = "inc")
  app$click("prep_final-run")

  app$set_inputs(tabs = "fit")
  app$click("fit_fit-run")

  app$set_inputs(tabs = "pred")
  app$click("pred_pred-run")

  app$set_inputs(tabs = "rep")
  app$set_inputs(repSel = "rep_markdown")
  sess_file <- app$get_download("dlRMD")
  expect_false(is.null(sess_file))
  lines <- readLines(sess_file)

  target_line <- grep("shapefile_directory <- ", lines)
  lines[target_line] <- 'shapefile_directory <- system.file("extdata/shapes", package="disagapp")'
  target_line <- grep("covariate_directory <- ", lines)
  lines[target_line] <- 'covariate_directory <- system.file("extdata/covariates", package="disagapp")'
  target_line <- grep("aggregation_directory <- ", lines)
  lines[target_line] <- 'aggregation_directory <- system.file("extdata/aggregation", package="disagapp")'

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
