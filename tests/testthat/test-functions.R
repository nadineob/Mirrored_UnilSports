test_that("webscraping returns a class of dataframe", {
  schedule <- webscrape_sports(days= 7)
  expect_equal(class(schedule), c("tbl_df", "tbl", "data.frame"))
})

test_that("function returns an error when the parameter days is not numeric", {
  expect_error(webscrape_sports(days= "twelve"), "'days' must be numeric")
})

test_that("MET webscraping function returns a class of dataframe", {
  MET <- webscrape_MET()
  expect_equal(class(MET), c("tbl_df", "tbl", "data.frame"))
})

test_that("function 3 behaves as expected", {
  load("mapping.rda")
  load("met_values.rda") 
  load("sport_schedule.rda")
  load("test_output.rda")
  
  output <- get_cleanschedule_met(sport_schedule,met_values,mapping)
  expect_true(all.equal(output,clean_sport_schedule))
})