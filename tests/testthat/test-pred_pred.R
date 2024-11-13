# test_that("Check pred_pred function works as expected", {
#
#   prediction <- pred_pred(fit = result,
#                           aggregation = r,
#                           cases = FALSE,
#                           predict_iid = FALSE,
#                           uncertain = FALSE,
#                           async = FALSE)
#
#   expect_is(prediction, "list")
#   expect_length(prediction, 4)
#   expect_is(prediction$`prediction (rate)`, "SpatRaster")
#   expect_null(prediction$iid)
#   expect_is(prediction$field, "SpatRaster")
#   expect_is(prediction$covariates, "SpatRaster")
#
#   prediction <- pred_pred(fit = result,
#                           aggregation = r,
#                           cases = TRUE,
#                           predict_iid = TRUE,
#                           uncertain = TRUE,
#                           N = 100,
#                           CI = 0.95,
#                           async = FALSE)
#
#   expect_is(prediction, "list")
#   expect_length(prediction, 7)
#   expect_is(prediction$`prediction (rate)`, "SpatRaster")
#   expect_is(prediction$`prediction (cases)`, "SpatRaster")
#   expect_is(prediction$field, "SpatRaster")
#   expect_is(prediction$iid, "SpatRaster")
#   expect_is(prediction$uncertainty_lower, "SpatRaster")
#   expect_is(prediction$uncertainty_upper, "SpatRaster")
#   expect_is(prediction$covariates, "SpatRaster")
#
#   prediction <- pred_pred(fit = result,
#                           aggregation = r,
#                           cases = TRUE,
#                           predict_iid = TRUE,
#                           uncertain = TRUE,
#                           N = 100,
#                           CI = 0.95,
#                           async = TRUE)
#
#   expect_is(prediction, "list")
#   expect_length(prediction, 7)
#   expect_is(prediction$`prediction (rate)`, "PackedSpatRaster")
#   expect_is(prediction$`prediction (cases)`, "PackedSpatRaster")
#   expect_is(prediction$field, "PackedSpatRaster")
#   expect_is(prediction$iid, "PackedSpatRaster")
#   expect_is(prediction$uncertainty_lower, "PackedSpatRaster")
#   expect_is(prediction$uncertainty_upper, "PackedSpatRaster")
#   expect_is(prediction$covariates, "PackedSpatRaster")
#
# })

test_that("Check fit_fit function works in the app", {

  # setup common for test
  test_common$fit <- result
  test_common$fit$data$covariate_rasters <- wrap_terra(test_common$fit$data$covariate_rasters)
  test_common$meta$fit_fit$iid <- TRUE
  saveRDS(test_common, test_common_path)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_fit_fit", timeout = 60000)
  app$set_inputs(tabs = "intro")
  app$set_inputs(introTabs = "Load Prior Session")
  app$upload_file(`core_load-load_session` = test_common_path)
  app$click("core_load-goLoad_session")

  app$set_inputs(tabs = "pred")
  app$set_inputs(predSel = "pred_pred")
  app$set_inputs("pred_pred-cases" = TRUE)
  app$set_inputs("pred_pred-iid" = TRUE)
  app$set_inputs("pred_pred-uncertain" = TRUE)
  app$click(selector = "#pred_pred-run")
  app$wait_for_value(input = "pred_pred-complete")
  app$set_inputs(main = "Save")
  app$get_download("core_save-save_session", filename = save_path)

  common <- readRDS(save_path)
  common$pred$`prediction (rate)` <- unwrap_terra(common$pred$`prediction (rate)`)
  common$pred$`prediction (cases)` <- unwrap_terra(common$pred$`prediction (cases)`)
  common$pred$field <- unwrap_terra(common$pred$field)
  common$pred$iid <- unwrap_terra(common$pred$iid)
  common$pred$uncertainty_lower <- unwrap_terra(common$pred$uncertainty_lower)
  common$pred$uncertainty_upper <- unwrap_terra(common$pred$uncertainty_upper)
  common$pred$covariates <- unwrap_terra(common$pred$covariates)

  expect_is(common$pred$`prediction (rate)`, "SpatRaster")
  expect_is(common$pred$`prediction (cases)`, "SpatRaster")
  expect_is(common$pred$field, "SpatRaster")
  expect_is(common$pred$iid, "SpatRaster")
  expect_is(common$pred$uncertainty_lower, "SpatRaster")
  expect_is(common$pred$uncertainty_upper, "SpatRaster")
  expect_is(common$pred$covariates, "SpatRaster")
})




