shp <- list.files(system.file("extdata/shapes", package="shinydisag"), pattern = ".shp", full.names = TRUE)
shape <- sf::st_read(shp, quiet = TRUE)
shape <- shape[shape$Name_1 == "Alaotra Mangoro",]

df <- data.frame("area" = shape$Name_2, "response" = 1:nrow(shape))
df_area_column <- "area"
df_resp_column <- "response"
shape_area_column <- "Name_2"

test_that("Check resp_combine function works as expected", {
  result <- resp_combine(df, df_area_column, df_resp_column, shape, shape_area_column)
  expect_is(result, "sf")
  expect_equal(nrow(result), 5)
})

test_that("{shinytest2} recording: e2e_resp_combine", {

  temp_shape <- tempfile(fileext = ".shp")
  sf::st_write(shape, temp_shape, quiet = TRUE)
  shpdf <- data.frame(datapath = list.files(path = dirname(temp_shape), pattern = gsub(".shp", "", basename(temp_shape)), full.names = TRUE),
                      name = list.files(path = dirname(temp_shape), pattern = gsub(".shp", "", basename(temp_shape))))

  temp_spread <- tempfile(fileext = ".csv")
  write.csv(df, temp_spread)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "shinydisag"), name = "e2e_resp_combine")

  app$upload_file("resp_combine-shape" = shpdf$datapath)
  app$upload_file("resp_combine-spread" = temp_spread)

  app$set_inputs("resp_combine-df_response_column" = df_resp_column)
  app$set_inputs("resp_combine-df_area_column" = df_area_column)
  app$set_inputs("resp_combine-shape_area_column" = shape_area_column)
  app$click("resp_combine-run")
  common <- app$get_value(export = "common")
  expect_is(common$shape, 'sf')
  expect_equal(nrow(common$shape), 5)
})
