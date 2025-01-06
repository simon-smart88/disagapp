test_that("Check cov_nightlight function works as expected", {
  skip_on_ci()
  if (Sys.getenv("NASA_username") != ""){
    result <- cov_nightlight(shape, 2022, get_nasa_token(Sys.getenv("NASA_username"), Sys.getenv("NASA_password")))
    expect_is(result, "SpatRaster")
  }
})

test_that("{shinytest2} recording: e2e_cov_nightlight", {
  skip_on_ci()
  if (Sys.getenv("NASA_username") != ""){
    rerun_test_setup("cov_nightlight_test", list(shpdf, save_path))
    common <- readRDS(save_path)
    common$covs <- unwrap_terra(common$covs)
    expect_is(common$covs[[1]], "SpatRaster")
    expect_equal(length(common$covs), 1)
  }
})
