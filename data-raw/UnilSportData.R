## code to prepare `UnilSports` dataset goes here

# Prepare a mapping table to map between UNIL Sport Activities and Activities in the MET table
library(readxl)
mapping <- read_excel(here::here("data-raw/Mapping_activities_MET.xlsx"))
usethis::use_data(mapping, overwrite = TRUE)

# Scrape data from the UNIL Sport Center website and prepare the dataset
source("R/functions.R")
sport_schedule <- webscrape_sports()
usethis::use_data(sport_schedule, overwrite = TRUE)

# Scrape data from the MET value website and prepare the dataset
met_values <- webscrape_MET()
usethis::use_data(met_values, overwrite = TRUE)

# Combine the dataset (UNIL Sport Schedule, MET Values)
clean_sport_schedule <- get_cleanschedule_met(sport_schedule,met_values,mapping)
usethis::use_data(clean_sport_schedule, overwrite = TRUE)

