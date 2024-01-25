shp <- list.files(system.file("extdata/shapes", package="disagapp"), pattern = ".shp", full.names = TRUE)
shape <- sf::st_read(shp, quiet = TRUE)
shape <- shape[shape$Name_1 == "Alaotra Mangoro",]

test_that("Check cov_nightlight function works as expected", {
  result <- cov_nightlight(shape, 2022)
  expect_is(result, "SpatRaster")
})

test_that("{shinytest2} recording: e2e_cov_nightlight", {

  shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="disagapp"), full.names = TRUE),
                      name = list.files(system.file("extdata/shapes", package="disagapp")))

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_nightlight")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$set_inputs("resp_shape-example" = FALSE) #will need deleting later
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")
  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_nightlight")
  app$set_inputs("cov_nightlight-year" = "2022")
  app$click("cov_nightlight-run")
  common <- app$get_value(export = "common")
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
