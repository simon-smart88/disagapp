mad_shape <- resp_shape(shpdf)
mad_shape_size <- as.numeric(object.size(mad_shape))

test_that("Check resp_simplify function works as expected", {
  result <- resp_simplify(mad_shape, 1000)
  expect_is(result, "sf")
  expect_equal(nrow(result), 109)
  simple_size <- as.numeric(object.size(result))
  expect_lt(simple_size, mad_shape_size)
})

test_that("{shinytest2} recording: e2e_resp_simplify", {
  skip_on_ci()
  skip_on_cran()
  rerun_test_setup("resp_simplify_test", list(shpdf, save_path))
  common <- readRDS(save_path)
  expect_is(common$shape, "sf")
  expect_equal(nrow(common$shape), 109)
  simple_size <- as.numeric(object.size(common$shape))
  expect_lt(simple_size, mad_shape_size)
})
