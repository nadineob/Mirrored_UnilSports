app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("testing_optimization_output_on_500_50")


app$setInputs(time = c("07:00 – 08:00", "10:00 – 11:00", "12:00 – 13:00", "19:00 – 20:00", "15:00 – 16:00"))
app$setInputs(date = "2022-12-20")
app$setInputs(activity = c("Basketball / Pratique libre à l'extérieur", "Musculation connectée / 1. Introduction"))
app$setInputs(calburn = 500)
app$setInputs(no_dup = TRUE)
app$setInputs(opt = "click")

app$snapshot()
