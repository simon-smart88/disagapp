
test_that("Check cov_bioclim function works as expected", {
  shape <- resp_download(df, area_column, resp_column, country_code, admin_level)

  result <- cov_bioclim("LIE", c("Mean temperature", "Mean diurnal range"), shape)
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
  expect_equal(length(result), 2)
})

test_that("Check cov_bioclim function returns errors as expected", {
  expect_error(cov_bioclim("LIE", "chicken"), "chicken is not a valid bioclim variable")
})

test_that("{shinytest2} recording: e2e_cov_bioclim", {

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_cov_bioclim")

  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  app$upload_file("resp_download-spread" = "../../lie.csv")
  #app$upload_file("resp_download-spread" = df_path)
  app$set_inputs("resp_download-response_column" = resp_column)
  app$set_inputs("resp_download-area_column" = area_column)
  app$set_inputs("resp_download-country" = "Liechtenstein")
  app$set_inputs("resp_download-admin" = "ADM1")
  app$click("resp_download-run")

  app$set_inputs(tabs = "cov")
  app$set_inputs(covSel = "cov_bioclim")
  app$set_inputs("cov_bioclim-country" = "Liechtenstein")
  app$set_inputs("cov_bioclim-variables" = c("Mean temperature", "Mean diurnal range"))
  app$click("cov_bioclim-run")

  #not needed but save fails without this
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)

  # common <- app$get_value(export = "common")
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 2)
})
