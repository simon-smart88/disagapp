test_that("Check resp_download function works as expected for single country", {
  result <- resp_download(df, area_column, resp_column, country_code[1], admin_level)
  expect_is(result, "sf")
  expect_equal(nrow(result), 11)
})

test_that("Check resp_download function works as expected for multiple countries", {
  result <- resp_download(mdf, area_column, resp_column, country_code, admin_level)
  expect_is(result, "sf")
  expect_equal(nrow(result), 37)
})

test_that("Check resp_download returns errors with faulty inputs", {
  expect_error(resp_download(df, 1, resp_column, "LIE", "ADM1"), "area_column must be a character string")
  expect_error(resp_download(df, area_column, 1, "LIE", "ADM1"), "resp_column must be a character string")
  expect_error(resp_download(df, area_column, resp_column, 1, "ADM1"), "country_code must be a character string")
  expect_error(resp_download(df, area_column, resp_column, "LIE", 1), "admin_level must be a character string")
  expect_error(resp_download(df, "a", resp_column, "LIE", "ADM1"), "df does not contain the column\\(s\\): a")
  expect_error(resp_download(df, area_column, "a", "LIE", "ADM1"), "df does not contain the column\\(s\\): a")
  expect_error(resp_download(df, area_column, resp_column, "LIE", "a"), "admin_level must be either ADM1 or ADM2")
  expect_error(resp_download(df, area_column, resp_column, "ZZZ", "ADM1"), "ZZZ is not a valid IS03 country code")
})

test_that("Check resp_download reports errors when data cannot be merged", {
  tdf <- df
  tdf$area[11] <- "aaa"
  messages <- capture_messages(expect_warning(resp_download(tdf, area_column, resp_column, country_code[1], admin_level), "*"))
  expect_equal(messages[1], "Area data for Vaduz could not be matched with response data\n")
  expect_equal(messages[2], "Response data for aaa could not be matched with an area\n")

  tdf <- tdf[tdf$response < 11,]
  messages <- capture_messages(expect_warning(resp_download(tdf, area_column, resp_column, country_code[1], admin_level), "*"))
  expect_equal(messages[1], "Area data for Vaduz could not be matched with response data\n")

  tdf <- rbind(df, data.frame("area" = "aaa", response = 1))
  messages <- capture_messages(expect_warning(resp_download(tdf, area_column, resp_column, country_code[1], admin_level), "*"))
  expect_equal(messages[1], "Response data for aaa could not be matched with an area\n")
})

#works using the temp file but only after uploading local first
test_that("{shinytest2} recording: e2e_resp_download", {

  df_path <- tempfile(fileext = ".csv")
  write.csv(df, df_path, row.names = FALSE)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_resp_download")
  app$set_inputs(tabs = "resp")
  app$set_inputs(respSel = "resp_download")
  app$upload_file("resp_download-spread" = df_path)
  app$set_inputs("resp_download-response_column" = resp_column)
  app$set_inputs("resp_download-area_column" = area_column)
  app$set_inputs("resp_download-country" = "Liechtenstein")
  app$set_inputs("resp_download-admin" = "ADM1")
  app$click("resp_download-run")

  app$set_inputs(main = "Save")
  save_file <- app$get_download("core_save-save_session", filename = save_path)
  common <- readRDS(save_file)

  expect_is(common$shape, "sf")
  expect_equal(nrow(common$shape), 11)
})


