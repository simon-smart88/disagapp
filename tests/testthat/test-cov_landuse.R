shp <- list.files(system.file("extdata/shapes", package="disagapp"), pattern = ".shp", full.names = TRUE)
shape <- sf::st_read(shp, quiet = TRUE)
shape <- shape[shape$Name_1 == "Alaotra Mangoro",]

shp2 <- list.files(system.file("extdata/shapes", package="disagapp"), full.names = TRUE)

# test_that("Check cov_landuse function works as expected", {
#   result <- cov_landuse(shape, 2019, c("Crops"))
#   expect_is(result, "list")
#   expect_is(result[[1]], "SpatRaster")
# })

test_that("{shinytest2} recording: e2e_cov_landuse", {

  temp_shape <- tempfile(fileext = ".shp")
  sf::st_write(shape, temp_shape, quiet = TRUE)
  # shpdf <- data.frame(datapath = list.files(path = dirname(temp_shape), pattern = gsub(".shp", "", basename(temp_shape)), full.names = TRUE),
  #                     name = list.files(path = dirname(temp_shape), pattern = gsub(".shp", "", basename(temp_shape))))

  shpdf <- data.frame(datapath = shp2,
                      name = basename(shp2))


  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_landuse")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_landuse")
  app$set_inputs("cov_landuse-year" = 2019)
  app$set_inputs("cov_landuse-uses" = c("Crops"))
  app$click("cov_landuse-run")

  common <- app$get_value(export = "common")
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
