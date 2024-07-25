test_that("Check cov_bioclim function works as expected for single country", {
  shape <- resp_download(df, area_column, resp_column, country_code[1], admin_level)

  result <- cov_bioclim(country_code[1], c("Mean temperature", "Mean diurnal range"), shape)
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 2)
  n_cells <- terra::ncell(result[[1]])
  expect_gt(n_cells, 400)
  expect_lt(n_cells, 500)
})

test_that("Check cov_bioclim function works as expected for multiple countries", {
  shape <- resp_download(mdf, area_column, resp_column, country_code, admin_level)

  result <- cov_bioclim(country_code, c("Mean temperature", "Mean diurnal range"), shape)
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 2)
  n_cells <- terra::ncell(result[[1]])
  expect_gt(n_cells, 100000)
})

test_that("Check cov_bioclim function returns errors as expected", {
  expect_error(cov_bioclim("ZZZ", c("Mean temperature", "Mean diurnal range"), shape), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_bioclim(c("ZZZ", "LIE"), c("Mean temperature", "Mean diurnal range"), shape), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_bioclim("LIE", "chicken", shape), "chicken is not a valid bioclim variable")
  expect_error(cov_bioclim(country_code[1], c("Mean temperature", "Mean diurnal range"), 123), "Shape must be an sf object")
})

test_that("{shinytest2} recording: e2e_cov_bioclim", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_nightlight", timeout = 60000)

  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  app$upload_file("resp_download-spread" = df_path)
  app$set_inputs("resp_download-response_column" = resp_column)
  app$set_inputs("resp_download-area_column" = area_column)
  app$set_inputs("resp_download-country" = "Liechtenstein")
  app$set_inputs("resp_download-admin" = "ADM1")
  app$click("resp_download-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_bioclim")
  app$set_inputs("cov_bioclim-country" = "Liechtenstein")
  app$set_inputs("cov_bioclim-variables" = c("Mean temperature", "Mean diurnal range"))
  app$click(selector = "#cov_bioclim-run")
  app$wait_for_value(input = "cov_bioclim-complete")

  if (is_ci){
    save_path <- tempfile(fileext = ".rds")
  } else {
    save_path <- "~/temprds/saved_file.rds"
  }

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)

  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 2)
})
