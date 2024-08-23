test_that("Check printVecAsis function works as expected", {
  expect_equal(printVecAsis(1) , 1)
  expect_equal(printVecAsis("a") , "\"a\"")
  expect_equal(printVecAsis(c(1, 2, 3)) , "c(1, 2, 3)")
  expect_equal(printVecAsis(c("a", "b", "c")) , "c(\"a\", \"b\", \"c\")")
})

test_that("Check wrap_terra function works as expected", {

  expect_true(is.null(wrap_terra(NULL)))
  no_ras_list <- list(a = 1, c = 2)
  expect_equal(wrap_terra(no_ras_list), no_ras_list)

  single_ras <- terra::rast(aggdf$datapath)
  expect_is(wrap_terra(single_ras), "PackedSpatRaster")

  list_ras <- cov_upload(resp_shape(shpdf), covdf)
  expect_is(wrap_terra(list_ras), "list")
  expect_is(wrap_terra(list_ras)[[1]], "PackedSpatRaster")

})

test_that("Check unwrap_terra function works as expected", {

  expect_true(is.null(unwrap_terra(NULL)))
  no_ras_list <- list(a = 1, c = 2)
  expect_equal(unwrap_terra(no_ras_list), no_ras_list)

  single_ras <- terra::wrap(terra::rast(aggdf$datapath))
  expect_is(unwrap_terra(single_ras), "SpatRaster")

  list_ras <- cov_upload(resp_shape(shpdf), covdf)
  list_ras <- lapply(list_ras, terra::wrap)
  expect_is(unwrap_terra(list_ras), "list")
  expect_is(unwrap_terra(list_ras)[[1]], "SpatRaster")

})

