####################### SHINY ############################################

library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(bslib)
library(sass)



# source(here::here(
#   rprojroot::find_root(rprojroot::root_criterion(function(path) file.exists(file.path(path, "DESCRIPTION")), "has DESCRIPTION")),
#   "R",
#   "functions.R"
# ))

# if (testthat::is_testing()) {
#   source(here::here("../00_pkg_src/UnilSports/R/functions.R"))
# } else {
#   source(here::here("R/functions.R"))
# }

# source(here::here("R/functions.R"))

pkgload::load_all("./")

# source(here::here(
#   whereami::thisfile(),
#   "R/functions2.R"
# ))


# sport_schedule <- webscrape_sports()
# met_values     <- webscrape_MET()
# clean_sport_schedule <- get_cleanschedule_met(sport_schedule,met_values)

# load(here::here(
#   rprojroot::find_root(rprojroot::is_testthat),
#   "data/clean_sport_schedule.rda"
# ))

# 
# mapping
# met_values
# sport_schedule
# clean_sport_schedule

shinyApp(build_ui(clean_sport_schedule), build_server(clean_sport_schedule))