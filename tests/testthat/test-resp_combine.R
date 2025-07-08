cdf_path <- system.file("extdata", "test_data", "cdf.csv", package = "disagapp")
cdf <- read.csv(cdf_path)

cdf_area_column <- "area"
cdf_resp_column <- "response"
shape_area_column <- "Name_2"

test_that("Check resp_combine function works as expected", {
  result <- resp_combine(cdf, cdf_area_column, cdf_resp_column, shape, shape_area_column)
  expect_is(result, "sf")
  expect_equal(nrow(result), 5)
})

test_that("{shinytest2} recording: e2e_resp_combine", {
  skip_on_cran()
  rerun_test_setup("resp_combine_test", list(shpdf_small, cdf_path,
                                             cdf_resp_column, cdf_area_column,
                                             shape_area_column, save_path))
  common <- readRDS(save_path)
  expect_is(common$shape, "sf")
  expect_equal(nrow(common$shape), 5)
})
