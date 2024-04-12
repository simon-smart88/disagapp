country_code <- "LIE"

df <- data.frame("area" = c("Triesen", "Schellenberg", "Gamprin", "Triesenberg",
                            "Eschen", "Ruggell", "Mauren", "Schaan", "Balzers",
                            "Planken","Vaduz"),
                 "response" = 1:11)

area_column <- "area"
resp_column <- "response"
country_code <- "LIE"
admin_level <- "ADM1"

shape <- resp_download(df, area_column, resp_column, country_code, admin_level)

test_that("Check agg_worldpop function works as expected", {
  result <- agg_worldpop(shape, country_code, "Constrained", "100m", 2020)
  expect_is(result, "SpatRaster")

  result <- agg_worldpop(shape, country_code, "Constrained", "1km", 2020)
  expect_is(result, "SpatRaster")

  result <- agg_worldpop(shape, country_code, "Unconstrained", "100m", 2000)
  expect_is(result, "SpatRaster")

  result <- agg_worldpop(shape, country_code, "Unconstrained", "1km", 2000)
  expect_is(result, "SpatRaster")

  result <- agg_worldpop(shape, country_code, "Unconstrained", "1km", 2020)
  expect_is(result, "SpatRaster")
})

test_that("Check agg_worldpop function returns errors", {
expect_error(agg_worldpop(shape, country_code, "aaaa", "1km", 2012), "Method must be either \"Constrained\" or \"Unconstrained\"")
expect_error(agg_worldpop(shape, country_code, "Constrained", "aaaa", 2020),"Resolution must be either \"100m\" or \"1km\"")
expect_error(agg_worldpop(shape, country_code, "Constrained", "1km", 2012), "Constrained population data is only available for 2020")
expect_error(agg_worldpop(shape, country_code, "Unconstrained", "1km", 1999), "Unconstrained data is only available between 2000 and 2020")
expect_error(agg_worldpop(shape, "ZZZ", "Unconstrained", "1km", 2000), "The requested data could not be found")
})

# test_that("{shinytest2} recording: e2e_agg_worldpop", {
#   app <- shinytest2::AppDriver$new(app_dir = system.file("shiny", package = "disagapp"), name = "e2e_agg_worldpop")
#   app$set_inputs("agg_worldpop-method" = "Unconstrained")
#   app$set_inputs("agg_worldpop-year" = 2020)
#   app$set_inputs("agg_worldpop-resolution" = "1km")
#   app$set_inputs("agg_worldpop-country" = "Liechtenstein")
#   app$click("agg_worldpop-run")
#   common <- app$get_value(export = "common")
#   expect_is(common$agg, "SpatRaster")
# })
#
