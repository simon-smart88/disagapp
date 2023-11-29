df <- data.frame("area" = c("Triesen", "Schellenberg", "Gamprin", "Triesenberg",
                            "Eschen", "Ruggell", "Mauren", "Schaan", "Balzers",
                            "Planken","Vaduz"),
                 "response" = 1:11)

area_column <- "area"
resp_column <- "response"
country_code <- "LIE"
admin_level <- "ADM1"

test_that("Check resp_download function works as expected", {
  result <- resp_download(df, area_column, resp_column, country_code, admin_level)
  expect_is(result, "sf")
  expect_equal(nrow(result), 11)
})

test_that("Check resp_download reports errors when data cannot be merged", {
  tdf <- df
  tdf$area[11] <- "aaa"
  messages <- capture_messages(expect_warning(resp_download(tdf, area_column, resp_column, country_code, admin_level), "*"))
  expect_equal(messages[1], "Area data for Vaduz could not be matched with response data\n")
  expect_equal(messages[2], "Response data for aaa could not be matched with an area\n")

  tdf <- tdf[tdf$response < 11,]
  messages <- capture_messages(expect_warning(resp_download(tdf, area_column, resp_column, country_code, admin_level), "*"))
  expect_equal(messages[1], "Area data for Vaduz could not be matched with response data\n")

  tdf <- rbind(df, data.frame("area" = "aaa", response = 1))
  messages <- capture_messages(expect_warning(resp_download(tdf, area_column, resp_column, country_code, admin_level), "*"))
  expect_equal(messages[1], "Response data for aaa could not be matched with an area\n")
})


#not working yet
test_that("{shinytest2} recording: e2e_resp_download", {

  df_path <- tempfile(fileext = ".csv")
  write.csv(df, df_path)

  app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "shinydisag"), name = "e2e_resp_download")
  app$upload_file("resp_download-spread" = df_path)
  app$set_inputs("resp_download-response_column" = resp_column)
  app$set_inputs("resp_download-area_column" = area_column)
  app$set_inputs("resp_download-country" = "Liechtenstein")
  app$set_inputs("resp_download-admin" = "ADM1")
  app$click("resp_download-run")
  common <- app$get_value(export = "common")
  expect_is(common$shape, "sf")
  expect_equal(nrow(common$shape), 11)
})

