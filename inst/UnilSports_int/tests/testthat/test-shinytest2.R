library(shinytest2)




test_that("{shinytest2} recording: testing_app", {
  app <- AppDriver$new(variant = platform_variant(), name = "testing_app", height = 859, 
      width = 1619)
  app$set_inputs(time = c("08:00 – 09:00", "10:00 – 11:00", "12:00 – 13:00", 
      "14:00 – 15:00"))
  app$set_inputs(date = "2022-12-14")
  app$set_inputs(activity = c("Football / Pratique libre", "Pole Sportive / Débutants (Cours 1)", 
      "Yoga / Avancés (Postures classiques)", "Basketball / Pratique libre à l'extérieur", 
      "Pole Sportive / Débutants (Cours 11 (supplémentaire))"))
  app$set_inputs(no_dup = TRUE)
  app$click("opt")
  app$expect_values()
})
