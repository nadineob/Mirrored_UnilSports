get_cleanschedule_met <- function() {
  library(dplyr)
  library(readxl)
  
  # Import data
  load("data/sport_schedule.rda")
  load("data/met_values.rda")
  mapping <- read_excel("data/Mapping_activities_MET.xlsx")
  
  # Cleaning 
  unique(sport_schedule$Timetable)
  sport_schedule <- sport_schedule %>% filter(!Timetable == "tout le jour")
  # Remove "tout le jour" from the sport_schedule
  sport_schedule <- sport_schedule %>% filter(!Activity == "Sport libre")
  
  # Mapping table
  sport_schedule <- sport_schedule %>% 
    left_join(mapping, by = "Activity") %>%
    left_join(select(met_values,-Activity), by = "Specific Motion")
  
  timetemp <- as.data.frame(matrix(as.numeric(gsub("\\:",".",unlist(strsplit(sport_schedule$Timetable, "–")))),
                                   ncol = 2, byrow = TRUE))
  
  timetemp <- timetemp %>% 
    mutate(V1_minutes = floor(V1)*60+(V1 - floor(V1))*100) %>%
    mutate(V2_minutes = floor(V2)*60+(V2 - floor(V2))*100) %>%
    mutate(Duration_min = V2_minutes - V1_minutes) %>%
    select(V1, V2, Duration_min)
  
  colnames(timetemp) <- c("Start time", "End time", "Duration_min")
  sport_schedule <- cbind(sport_schedule, timetemp)
  sport_schedule <- sport_schedule %>% 
    select(Date, `Start time`, `End time`, Duration_min, Activity, Location, METs) %>%
    mutate(p = (Duration_min*METs*3.5)/200)
  
  checkna <- sum(is.na(sport_schedule)) #0 no NA
  return(sport_schedule)
}

# Example to call get_cleanschedule_met
cleanschedule <- get_cleanschedule_met()
