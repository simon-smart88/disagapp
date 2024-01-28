shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata/shapes", package="disagapp")))

#set semi-temporary path so that the same file can be read between different tests
save_path <- "~/temprds/saved_file.rds"

test_that("{shinytest2} recording: e2e_empty_save", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_empty_save")
  app$set_inputs(tabs = "resp")
  app$set_inputs(main = "Save")
  save_file <- app$get_download("save_session", filename = save_path)
  common <- readRDS(save_file)
  expect_true(is.null(common$covs))
})

test_that("{shinytest2} recording: e2e_empty_load", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_empty_load")
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file(load_session = save_path)
  app$click("goLoad_session")
  common <- app$get_value(export = "common")
  expect_true(is.null(common$covs))
})

test_that("{shinytest2} recording: e2e_shape_save", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_shape_save")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$set_inputs(main = "Save")
  save_file <- app$get_download("save_session", filename = save_path)
  common <- readRDS(save_file)
  expect_is(common$shape, "sf")
  })

test_that("{shinytest2} recording: e2e_shape_load", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_shape_load")
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file(load_session = save_path)
  app$click("goLoad_session")
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")
})

test_that("{shinytest2} recording: e2e_settings_save", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_settings_save")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$set_inputs("resp_shape-example" = FALSE)
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
  save_file <- app$get_download("save_session", filename = save_path)
  common <- readRDS(save_file)
  expect_equal(common$state$resp_shape$example, FALSE)
  expect_equal(common$state$cov_access$layer, "Motorized Travel Time to Healthcare (2020)")
  expect_equal(common$state$cov_landuse$uses, c("Crops", "PermanentWater", "SeasonalWater"))
  expect_equal(common$state$cov_landuse$year, "2017")
  #expect_equal(common$state$agg_worldpop$country, "Liechtenstein")
  #need to add lines to manually export
  expect_equal(common$state$agg_worldpop$resolution, "100m")

})

test_that("{shinytest2} recording: e2e_settings_load", {
  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_settings_load")
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file(load_session = save_path)
  app$click("goLoad_session")

    expect_equal(app$get_value(input = "resp_shape-example"), FALSE)
  expect_equal(app$get_value(input = "cov_access-layer"), "Motorized Travel Time to Healthcare (2020)")
  expect_equal(app$get_value(input = "cov_landuse-uses"), c("Crops", "PermanentWater", "SeasonalWater"))
  expect_equal(app$get_value(input = "cov_landuse-year"), "2017")
  #expect_equal(app$get_value(input = "agg_worldpop-country", "Liechtenstein"))
  expect_equal(app$get_value(input = "agg_worldpop-resolution"), "100m")

})


