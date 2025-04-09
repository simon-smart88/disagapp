test_that("Check cov_landuse function works as expected", {
  result <- cov_landuse(shape, 2019, c("Crops"))
  expect_is(result, "list")
  expect_is(result[[1]], "SpatRaster")
})

test_that("Check cov_landuse function returns errors", {
  expect_error(cov_landuse(123, 2019, c("Crops")), "shape must be an sf object")
  expect_error(cov_landuse(shape, 2019, c("banana")), "banana is not a valid land use type. ")
  expect_error(cov_landuse(shape, 2020, c("Crops")), "Land use data is only available between 2015 and 2019")
})

test_that("{shinytest2} recording: e2e_cov_landuse", {
  skip_on_cran()
  rerun_test_setup("cov_landuse_test", list(df_path, resp_column, area_column, save_path))
  common <- readRDS(save_path)
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
