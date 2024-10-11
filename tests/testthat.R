Sys.setenv("R_TEST" = "")
library(testthat)
library(disagapp)
library(shinytest2)

options(shinytest2.load_timeout = 30 * 1000)

test_check("disagapp")
