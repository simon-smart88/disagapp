Sys.setenv("R_TEST" = "")
library(testthat)
library(disagapp)
library(shinytest2)

test_check("disagapp")
