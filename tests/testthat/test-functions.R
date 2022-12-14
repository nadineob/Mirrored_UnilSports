test_that("webscraping returns a class of dataframe", {
  schedule <- webscrape_sports(days= 14)
  expect_equal(class(schedule), c("tbl_df", "tbl", "data.frame"))
})

test_that("function returns an error when the parameter days is not numeric", {
  expect_error(webscrape_sports(days= "twelve"), "'days' must be numeric")
})



