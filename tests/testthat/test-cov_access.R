shp <- list.files(system.file("extdata/shapes", package="disagapp"), pattern = ".shp", full.names = TRUE)
shape <- sf::st_read(shp, quiet = TRUE)
shape <- shape[shape$Name_1 == "Alaotra Mangoro",]

save_path <- "~/temprds/saved_file.rds"

# test_that("Check cov_access function works as expected", {
#   result <- cov_access(shape, "Travel Time to Cities (2015)")
#   expect_is(result, "SpatRaster")
# })

test_that("{shinytest2} recording: e2e_cov_access", {

  temp_shape <- tempfile(fileext = ".shp")
  sf::st_write(shape, temp_shape, quiet = TRUE)
  shpdf <- data.frame(datapath = list.files(path = dirname(temp_shape), pattern = gsub(".shp", "", basename(temp_shape)), full.names = TRUE),
                      name = list.files(path = dirname(temp_shape), pattern = gsub(".shp", "", basename(temp_shape))))

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_access")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_shape")
  app$upload_file("resp_shape-shape" = shpdf$datapath)
  app$set_inputs("resp_shape-resp_var" = "inc")
  app$click("resp_shape-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_access")
  app$set_inputs("cov_access-layer" = "Travel Time to Cities (2015)")
  app$click("cov_access-run")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)
  expect_equal(length(common$covs), 1)
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")

})
