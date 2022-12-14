#' @title Webscraping function for Sports Schedule
#' @description A function that returns the sports table schedule from the Unil Sports Center
#' @import "rvest" "tidyverse" "lubridate"
#' @return  A data frame containing the sports schedule of 7 days. We have selected 7 days because Unil Sports Center website do not contain full sport schedules for dates further than 7days.
#' @export
#' @examples
#' # Data Frame generated in sport_schedule
#' sport_schedule <- webscrap_sports()
webscrape_sports <- function() {
  library(rvest)
  library(tidyverse)
  library(lubridate)

  days <- 7

  if (is.numeric(days) == F) {
    stop("'days' must be numeric")
  }
  # Adding today's link to the list
  schedule.html <- read_html("https://sport.unil.ch/?mid=92") # webpage
  a_elements <- schedule.html %>%
    html_nodes(".summary .level2>li>a") %>%
    html_attr("href")
  a_elements <- paste("https://sport.unil.ch/",a_elements, sep = "")

  #Create list of links for the master table
  new_list <- list()

  # Adding today's link to the new list
  new_list <- append(new_list,a_elements[1])

  # Consider today's date
  days <- days -1

  # Create list of days
  for (i in 1:days) {
    if (i == 1) {
      schedule.html <- read_html("https://sport.unil.ch/?mid=92") # webpage
      a_element <- schedule.html %>%
        html_nodes(".planning-navigation .next>a") %>%
        html_attr("href")
      new.html <- paste("https://sport.unil.ch/",a_element, sep = "")
      new_list <- append(new_list,new.html)
    } else {
      schedule.html <- read_html(new.html) # webpage
      a_element <- schedule.html %>%
        html_nodes(".planning-navigation .next>a") %>%
        html_attr("href")
      new.html <- paste("https://sport.unil.ch/",a_element, sep = "")
      new_list <- append(new_list,new.html)
    }
  }

  # Transform to a vector of characters
  new_vector <- unlist(new_list)

  # Creating two empty dataframes for table
  table_new <-data.frame()
  sport_schedule <- data.frame()

  # Adding today's date
  days <- days +1

  # Create sports table with the number of days require
  for (i in 1:days) {
    html <- read_html(new_vector[i])
    table_new <- rvest::html_element(html, css = "table.quotidien" ) %>%
      rvest::html_table() %>%
      slice(-1) %>% # Remove first Row (weird names)
      rename("Timetable" = "X1", "Activity"= "X2", "Location"="X3")
    previous.date <- html %>%
      html_nodes("div.planning-navigation td.back") %>% # Retrieve the yesterdays date
      html_text2()
    previous.date <- gsub("\\.", "-", previous.date)
    previous.date <- parse_date_time(previous.date, orders = c("ymd", "dmy", "mdy"))
    todays.date <- as.Date(previous.date) + 1 # Retrieve today's date
    table_new <-  table_new %>%
      mutate(Date = todays.date) %>%
      relocate(Date, .before = Timetable)
    sport_schedule<- rbind(sport_schedule,table_new) # Create table
  }
  usethis::use_data(sport_schedule, overwrite = TRUE)
  return(sport_schedule)
}



#' @title Webscraping function for MET Values
#' @description A function that returns the MET values table of more than 800 activities.
#' @import "rvest" "tidyverse"
#' @return  A data frame containing the MET values per sport activity.
#' @export
#' @examples
#' # # Data Frame generated in met_values
#' met_values <- webscrape_MET()
webscrape_MET <- function() {
  library(rvest)
  library(tidyverse)

  met_values.html <- read_html("https://golf.procon.org/met-values-for-800-activities/") # webpage

  # Retrieve METs Values table
  element <- met_values.html %>%
    html_element(css = "tbody.row-hover")
  element

  met_values <- element %>%
    html_table() %>%
    rename("Activity" = "X1", "Specific Motion"= "X2", "METs"="X3")

  # Create data to use for the functions
  usethis::use_data(met_values, overwrite = TRUE)
  return(met_values)
}



#' @title Wrangling Function
#' @description
#' A function that clean the output (data frame) returned from the webscrape_sports function.
#' During this function the MET values (data frame) retrieved by the webscrape_MET function will be mapped into the data in order to calculate the calorie burn amount per activity.
#' @import "dplyr" "readxl" "here"
#' @param sport_schedule The data frame output from the webscrape_sports. The output of this function should not be modified, so this function can apply the integer optimization technique properly.
#' @param met_values The data frame output from the webscrape_MET. The output of this function should not be modified, so this function can apply the integer optimization technique properly.
#' @return  A data frame containing the activities per day with their correspondingly Met values.
#' @export
#' @examples
#' # Use the data frames retrieved from the webscrape_sports and webscrape_MET functions
#' cleanschedule <- get_cleanschedule_met(sport_schedule,met_values)
get_cleanschedule_met <- function(sport_schedule,met_values) {
  library(dplyr)
  library(readxl)
  library(here)

  mapping <- read_excel(here::here("data-raw/Mapping_activities_MET.xlsx"))

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
  clean_sport_schedule <- sport_schedule %>%
    select(Date, `Start time`, `End time`, Duration_min, Activity, Location, METs) %>%
    mutate(p = (Duration_min*METs*3.5)/200)

  checkna <- sum(is.na(clean_sport_schedule)) #0 no NA
  return(clean_sport_schedule)
}



#' @title Optimization Function
#' @description
#' Function that evaluates the calorie burn per activity and time according to the parameters entered.
#' @import "lpSolve" "data.table" "dplyr"
#' @param cleanschedule The data frame output from the get_cleanschedule_met. The output of this function should not be modified, so this function can apply the integer optimization technique properly. .
#' @param date
#' @param activity
#' @param time
#' @param calburn
#' @param weight
#' @return
#' @export
#' @examples
#' # Define Inputs
#' calburn <- 500
#' date <- c('2022-12-02') # Use this format and change the date for today
#' activity <- c('Football / Pratique libre', 'Basketball / Pratique libre à l'extérieur',
#'               'Pilates', 'Musculation / Initiation')
#' weight <- 50
#' time <- c('07:00 – 08:00', '08:00 – 09:00', '12:00 – 13:00', '13:00 – 14:00',
#'          '17:00 – 18:00', '18:00 – 19:00', '19:00 – 20:00')
#' optimize_output <- optimize_schedule(cleanschedule, date, activity, time, calburn, weight)
#' # optimize_output[1] # 1 if successful and 0 if fail
#' # sum(optimize_output$table_result$calburn) # 753.375
optimize_schedule <- function(cleanschedule, date, activity, time, calburn, weight) {
  library(lpSolve)
  library(data.table)
  library(dplyr)

  ### Filter Date and Activity from cleanschedule
  cleanscheduletemp <- cleanschedule %>%
    filter(Date == date) %>%
    filter(Activity %in% activity)

  # Prepare time to be ready to filter
  cleanscheduletemp <- cleanscheduletemp %>% mutate(time = 0)
  cleanscheduletemp <- cleanscheduletemp[, c(1,2,3,9,4,5,6,7,8)]

  timetemp <- as.data.frame(matrix(as.numeric(gsub("\\:",".",unlist(strsplit(sort(time), "–")))),
                                   ncol = 2, byrow = TRUE))

  # Aggregate the connecting time slots
  # e.g. if the selected time slots are 7am-8am and 8am-9am, combine them together into a single timeslot
  # i.e. 7am-9am
  n <- length(time)
  selected_time <- data.frame()
  for (i in 1:n) {
    if(i==1) {
      output_temp <- timetemp[i,]
    }
    else{
      output_temp2 <- timetemp[i,]
      if(output_temp[2] == output_temp2[1]){
        output_temp[2] <- output_temp2[2]
      }
      else{
        selected_time <- rbind(selected_time, output_temp)
        output_temp <- timetemp[i,]
      }

    }
    if(i==n){
      selected_time <- rbind(selected_time, output_temp)
    }
  }

  # Filter only activities that are within the selected time slots
  k <- nrow(selected_time)
  for (j in 1:k){
    cleanscheduletemp$time <- cleanscheduletemp$time |
      (cleanscheduletemp$`Start time` >= selected_time[j,1] &
         cleanscheduletemp$`End time` <= selected_time[j,2])
    cleanscheduletemp$time <- 1*cleanscheduletemp$time
  }
  table_opt <- cleanscheduletemp %>% filter(time == 1) # Filter time from cleanscheduletemp

  ### Optimization

  # Set coefficients of the objective function
  n_activity <- nrow(table_opt)
  f.obj <- c(rep(1, n_activity))

  # Find calories burn per activity
  table_opt <- table_opt %>% mutate(calburn = p*weight)

  ## Find overlapping activities
  # Set constraints such that the optimizer won't select 2 activities that occur at the same time
  # for example, if activity A starts at 8.00 and ends at 9.00 and activity B starts at 8.45 and ends at 9.15,
  # they cannot be selected together (i.e. only one of them can be selected)
  table_opt_datatable <- as.data.table(table_opt)
  table_opt_datatable$`Start time` <- table_opt_datatable$`Start time`+0.01
  setkey(table_opt_datatable, 'Start time', 'End time')
  overlap_ix <- foverlaps(table_opt_datatable, table_opt_datatable, type="any", which=TRUE)
  overlap_ix <- overlap_ix[as.logical(!(overlap_ix[,1] == overlap_ix[,2])),]
  overlap_ix <- as.data.frame(overlap_ix)

  num_combination_overlap <- nrow(overlap_ix)
  overlap_constraints <- matrix(0,num_combination_overlap,n_activity)
  for (i in 1:num_combination_overlap) {
    overlap_constraints[i,as.numeric(overlap_ix[i,])] <- 1
  }
  overlap_constraints <- overlap_constraints[!duplicated(overlap_constraints),]
  num_combination_overlap <- nrow(overlap_constraints)
  rhs_overlap <- c(rep(1, num_combination_overlap))
  dir_overlap <- c(rep('<=', num_combination_overlap))

  # Set matrix corresponding to coefficients of constraints by rows
  f.con <- matrix(table_opt$calburn, nrow = 1)
  f.con <- rbind(f.con,overlap_constraints)
  # Set inequality signs
  f.dir <- c(">=",dir_overlap)

  # Set right hand side coefficients
  f.rhs <- c(calburn, rhs_overlap)

  # Final value (z)
  optim_output <- lp("min", f.obj, f.con, f.dir, f.rhs, int.vec = 1:n_activity, all.bin = TRUE)
  if (optim_output$status == 0) { # setting from 'lp': if successful, status = 0
    optim_result = 1 #successful
    cat(sprintf('The optimization is successful\n'))
  } else {
    optim_result = 0 #fail
    cat(sprintf('No feasible solution found\n'))
  }
  return(list(optim_result = optim_result, table_result = table_opt[as.logical(optim_output$solution),], activity_selected = table_opt))

}
