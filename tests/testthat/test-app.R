# File: tests/testthat/test-inst-apps.R
library(shinytest2)
library(shiny)

test_that("UnilSports app works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  appdir <- system.file(package = "UnilSports", "UnilSports_int")
  test_app(appdir)
  expect_equal(1,1)
})