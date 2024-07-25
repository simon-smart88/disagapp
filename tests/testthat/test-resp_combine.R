cdf <- data.frame("area" = shape$Name_2, "response" = 1:nrow(shape))
cdf_area_column <- "area"
cdf_resp_column <- "response"
shape_area_column <- "Name_2"

test_that("Check resp_combine function works as expected", {
  result <- resp_combine(cdf, cdf_area_column, cdf_resp_column, shape, shape_area_column)
  expect_is(result, "sf")
  expect_equal(nrow(result), 5)
})

test_that("{shinytest2} recording: e2e_resp_combine", {

  temp_spread <- tempfile(fileext = ".csv")
  write.csv(cdf, temp_spread)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_combine")

  app$upload_file("resp_combine-shape" = shpdf_small$datapath)
  app$upload_file("resp_combine-spread" = temp_spread)

  app$set_inputs("resp_combine-df_response_column" = cdf_resp_column)
  app$set_inputs("resp_combine-df_area_column" = cdf_area_column)
  app$set_inputs("resp_combine-shape_area_column" = shape_area_column)
  app$click("resp_combine-run")

  if (is_ci){
    save_path <- tempfile(fileext = ".rds")
  } else {
    save_path <- "~/temprds/saved_file.rds"
  }

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)

  expect_is(common$shape, 'sf')
  expect_equal(nrow(common$shape), 5)
})
