test_that("Check resp_edit function works as expected", {
  mad_shape <- resp_shape(shpdf)
  poly <- matrix(c(40, 40, 55, 55, 40, -10, -20, -20, -10, -10), ncol = 2)
  colnames(poly) <- c("longitude", "latitude")
  result <- resp_edit(mad_shape, poly, "outside")
  expect_is(result, "sf")
  expect_lt(nrow(result), 50)
  result <- resp_edit(mad_shape, poly, "inside")
  expect_is(result, "sf")
  expect_gt(nrow(result), 60)
})

test_that("{shinytest2} recording: e2e_resp_edit", {
  skip_on_cran()
  rerun_test_setup("resp_edit_test", list(shpdf, save_path))
  common <- readRDS(save_path)
  expect_is(common$shape, "sf")
  expect_lt(nrow(common$shape), 50)
})
