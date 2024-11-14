test_that("Check cov_water function works as expected", {
  skip_on_ci()
  if (Sys.getenv("ARCGIS_CLIENT") != ""){
    result <- cov_water(shape, arcgisutils::auth_client())
    expect_is(result, "SpatRaster")
    expect_error(cov_water(shpdf, arcgisutils::auth_client()), "Shape must be an sf object")
    expect_error(cov_water(shape, "token"), "Token must be an httr2_token")
  }
})

test_that("{shinytest2} recording: e2e_cov_water", {
  skip_on_ci()
  if (Sys.getenv("ARCGIS_CLIENT") != ""){
    rerun_test_setup("cov_water_test", list(shpdf, save_path))
    common <- readRDS(save_path)
    common$covs <- unwrap_terra(common$covs)
    expect_is(common$covs[[1]], "SpatRaster")
    expect_equal(length(common$covs), 1)
  }
})
