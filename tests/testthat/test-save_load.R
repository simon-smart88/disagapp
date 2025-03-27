test_that("{shinytest2} recording: e2e_empty_save", {
  rerun_test_setup("empty_save_test", list(save_path))
  common <- readRDS(save_path)
  expect_length(common$covs, 0)
})

test_that("{shinytest2} recording: e2e_empty_load", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_empty_load")
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file("core_load-run" = save_path)
  app$click("core_load-goLoad_session")
  common <- app$get_value(export = "common")
  expect_length(common$covs, 0)
  app$stop()
})

test_that("{shinytest2} recording: e2e_shape_save", {
  rerun_test_setup("shape_save_test", list(shpdf, save_path))
  common <- readRDS(save_path)
  expect_is(common$shape, "sf")
})

test_that("{shinytest2} recording: e2e_shape_load", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_empty_load")
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file("core_load-run" = save_path)
  app$click("core_load-goLoad_session")
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")
  app$stop()
})

test_that("{shinytest2} recording: e2e_settings_save", {
  rerun_test_setup("settings_save_test", list(save_path))
  common <- readRDS(save_path)
  expect_equal(common$state$resp_example$dataset, "scot")
  expect_equal(common$state$cov_access$layer, "Motorized Travel Time to Healthcare (2020)")
  expect_equal(common$state$cov_landuse$uses, c("Crops", "PermanentWater", "SeasonalWater"))
  expect_equal(common$state$cov_landuse$year, "2017")
  expect_equal(common$state$agg_worldpop$country, "Liechtenstein")
  expect_equal(common$state$agg_worldpop$resolution, "100m")
})

test_that("{shinytest2} recording: e2e_settings_load", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_empty_load")
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file("core_load-run" = save_path)
  app$click("core_load-goLoad_session")
  expect_equal(app$get_value(input = "resp_example-dataset"), "scot")
  expect_equal(app$get_value(input = "cov_access-layer"), "Motorized Travel Time to Healthcare (2020)")
  expect_equal(app$get_value(input = "cov_landuse-uses"), c("Crops", "PermanentWater", "SeasonalWater"))
  expect_equal(app$get_value(input = "cov_landuse-year"), "2017")
  expect_equal(app$get_value(input = "agg_worldpop-country"), "Liechtenstein")
  expect_equal(app$get_value(input = "agg_worldpop-resolution"), "100m")
  app$stop()
})


