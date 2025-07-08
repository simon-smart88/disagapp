test_that("Check cov_nightlight function works as expected", {
  result <- cov_nightlight(lie_shape, "LIE", 2022)
  expect_is(result, "SpatRaster")
})

test_that("Check cov_nightlight function works as expected for multiple countries", {
  shape <- resp_download(mdf, area_column, resp_column, country_code, admin_level)
  result <- cov_nightlight(shape, country_code, 2022)
  expect_is(result, "SpatRaster")
  n_cells <- terra::ncell(result)
  expect_gt(n_cells, 100000)
})

test_that("Check cov_nightlight function returns errors as expected", {
  expect_error(cov_nightlight(lie_shape, "ZZZ", 2022), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_nightlight(lie_shape, c("ZZZ", "LIE"), 2022), "ZZZ is not a valid IS03 country code. ")
  expect_error(cov_nightlight(lie_shape, "LIE", "chicken"), "Nighttime data is only available between 2015 and 2023")
  expect_error(cov_nightlight(lie_shape, "LIE", 2000), "Nighttime data is only available between 2015 and 2023")
  expect_error(cov_nightlight(123, country_code[1], 2022), "Shape must be an sf object")
  expect_error(cov_nightlight(shape, "LIE", 2022), "The downloaded nightlight data does not overlap with the response data")
})

test_that("{shinytest2} recording: e2e_cov_nightlight", {
  skip_on_ci()
  skip_on_cran()
  rerun_test_setup("cov_nightlight_test", list(df_path, resp_column, area_column, save_path))
  common <- readRDS(save_path)
  common$covs <- unwrap_terra(common$covs)
  expect_is(common$covs[[1]], "SpatRaster")
  expect_equal(length(common$covs), 1)
})
