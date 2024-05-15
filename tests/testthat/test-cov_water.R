shp <- list.files(system.file("extdata/shapes", package="disagapp"), pattern = ".shp", full.names = TRUE)
shape <- sf::st_read(shp, quiet = TRUE)
shape <- shape[shape$Name_1 == "Alaotra Mangoro",]

test_that("Check cov_water function works as expected", {
  result <- cov_water(shape, arcgisutils::auth_client())
  expect_is(result, "SpatRaster")
})

save_path <- "~/temprds/saved_file.rds"

test_that("{shinytest2} recording: e2e_cov_nightlight", {

  shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="disagapp"), full.names = TRUE),
                      name = list.files(system.file("extdata/shapes", package="disagapp")))

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_nightlight")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_water")
  app$click("cov_water-run")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  common$covs <- unwrap_terra(common$covs)

  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
