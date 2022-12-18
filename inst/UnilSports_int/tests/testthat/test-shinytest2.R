library(shinytest2)

test_that("{shinytest2} recording: testing_optimization_activities_for_1500_50_values", {
  app <- AppDriver$new(variant = platform_variant(), name = "testing_optimization_activities_for_1500_50_values", 
      height = 859, width = 1619)
  app$set_inputs(date = "2022-12-20")
  app$set_inputs(time = "09:00 – 10:00")
  app$set_inputs(time = c("09:00 – 10:00", "11:00 – 12:00"))
  app$set_inputs(time = c("09:00 – 10:00", "11:00 – 12:00", "12:00 – 13:00"))
  app$set_inputs(time = c("09:00 – 10:00", "11:00 – 12:00", "12:00 – 13:00", 
      "14:00 – 15:00"))
  app$set_inputs(activity = c("Basketball / Pratique libre à l'extérieur", "Football / Pratique libre", 
      "Musculation connectée / 1. Introduction"), wait_ = FALSE)
  app$set_inputs(no_dup = TRUE, wait_ = FALSE)
  app$click("opt")
  app$expect_screenshot()
  app$expect_values()
})
