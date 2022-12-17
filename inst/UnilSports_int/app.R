####################### SHINY ############################################

library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(bslib)
library(sass)


source("../../R/functions.R")
# sport_schedule <- webscrape_sports()
# met_values     <- webscrape_MET()
# clean_sport_schedule <- get_cleanschedule_met(sport_schedule,met_values)

load("tests/clean_sport_schedule.rda")

shinyApp(build_ui(clean_sport_schedule), build_server(clean_sport_schedule))