test_that("Check cov_water function works as expected", {
  result <- cov_water(lie_shape, "LIE")
  expect_is(result, "SpatRaster")
})

test_that("Check cov_water function works as expected for multiple countries", {
  shape <- resp_download(mdf, area_column, resp_column, country_code, admin_level)
  result <- cov_water(shape, country_code)
  expect_is(result, "SpatRaster")
  n_cells <- terra::ncell(result)
  expect_gt(n_cells, 100000)
})

test_that("Check cov_nightlight function returns errors as expected", {
  expect_error(cov_water(lie_shape, "ZZZ"), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_water(lie_shape, c("ZZZ", "LIE")), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_water(123, country_code[1]), "Shape must be an sf object")
  expect_error(cov_water(shape, "LIE"), "The downloaded nightlight data does not overlap with the response data")
})

test_that("{shinytest2} recording: e2e_cov_water", {
  skip_on_cran()
  rerun_test_setup("cov_water_test", list(df_path, resp_column, area_column, save_path))
  common <- readRDS(save_path)
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
