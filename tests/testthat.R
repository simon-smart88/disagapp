Sys.setenv("R_TEST" = "")
library(testthat)
library(disagapp)
library(shinytest2)

options(shinytest2.load_timeout = 30 * 1000)

test_check("disagapp")

if (.Platform$OS.type == "windows" && Sys.getenv("GITHUB_ACTIONS") == "true") {
  system('taskkill /F /IM "chrome.exe" /T')
}
