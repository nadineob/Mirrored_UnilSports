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
  
  output <- get_cleanschedule_met(sport_schedule,met_values)
  expect_true(all.equal(output,clean_sport_schedule))
})

test_that("optimization function returns a list, has a table of 10 columns and ensures that the total calories returned is greater or equal to the calburn entered", {
  calburn <- 200
  date <- c('2022-12-14')
  activity <- c('Aquagym', 'Zumba', 'Pilates', 'Agrès',
                'Tai ji quan / Tous niveaux',
                'Musculation connectée / 1. Introduction',
                'Cirque', 'Aviron / Débutants', 'Salsa cubaine / Débutants')
  weight <- 80
  time <- c('07:00 – 08:00', '08:00 – 09:00', '12:00 – 13:00', '13:00 – 14:00',
            '17:00 – 18:00', '18:00 – 19:00', '19:00 – 20:00')
  flag_no_duplicate_activities <- 1
  load(here::here("data/clean_sport_schedule.rda"))
  optimize_output <- optimize_schedule(clean_sport_schedule, date, activity, time, calburn, weight,flag_no_duplicate_activities)
  
  expect_equal(class(optimize_output), "list")
  expect_identical(ncol(optimize_output$table_result), 10L)
  expect_gte(optimize_output$totalcal, calburn)
})

test_that("pie_optim function contains correct data", {
  calburn <- 200
  date <- c('2022-12-14')
  activity <- c('Aquagym', 'Zumba', 'Pilates', 'Agrès',
                'Tai ji quan / Tous niveaux',
                'Musculation connectée / 1. Introduction',
                'Cirque', 'Aviron / Débutants', 'Salsa cubaine / Débutants')
  weight <- 80
  time <- c('07:00 – 08:00', '08:00 – 09:00', '12:00 – 13:00', '13:00 – 14:00',
            '17:00 – 18:00', '18:00 – 19:00', '19:00 – 20:00')
  flag_no_duplicate_activities <- 1
  load(here::here("data/clean_sport_schedule.rda"))
  optimize_output <- optimize_schedule(clean_sport_schedule, date, activity, time, calburn, weight,flag_no_duplicate_activities)
  optim_plot <- optimize_output$table_result
  p1 <- pie_optim(optim_plot) 
  
  expect_gte(optim_plot$calburn, calburn)
  expect_equal(class(optim_plot), "data.frame")
  expect_equal(class(p1), "list")
})


