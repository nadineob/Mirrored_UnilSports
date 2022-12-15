#' @title Webscraping function for Sports Schedule
#' @description A function that returns the sports table schedule from the Unil Sports Center
#' @param days The number of days that you want to retrieve from the Unil Sports Center website
#' @param ... A parameter that allows the user to enter additional arguments that are not defined in the function
#' @return  A data frame containing the sports schedule of the number of days selected
#' @export
#' @examples
#' # Run the function as it is
#' webscrap_sports()
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
  return(sport_schedule)
}

#' @title Webscraping function for MET Values
#' @description A function that returns the MET values table of more than 800 activities.
#' @return  A data frame containing the MET values per sport activity.
#' @export
#' @examples
#' # Run the function as it is
#' webscrape_MET()

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
  return(met_values)
}

#' @title Wrangling Function
#' @description 
#' A function that clean the output (data frame) returned from the webscrape_sports function. 
#' During this function the MET values will be mapped into the data in order to calculate the calorie burn amount per activity.  
#' @return  A data frame containing the activities per day with their correspondingly Met values.
#' @export
#' @examples
#' # Use the data frame retrieved from the webscrape_sports function
#' get_cleanschedule_met()
get_cleanschedule_met <- function(sport_schedule,met_values) {
  library(dplyr)
  library(here)
  
  #Loading mapping file 
  load(here::here("data/mapping.rda"))
  
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
  
  checkna <- sum(is.na(clean_sport_schedule)) 
  return(clean_sport_schedule)
}


#' @title Optimization Function
#' @description 
#' Function that evaluates the calorie burn per activity and time according to the parameters entered.  
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
# calburn <- 500
# date <- c('2022-12-14')
# activity <- c('Aquagym', 'Zumba', 'Pilates', 'Agrès',
#              'Tai ji quan / Tous niveaux',
#              'Musculation connectée / 1. Introduction',
#              'Cirque', 'Aviron / Débutants', 'Salsa cubaine / Débutants')
# weight <- 50
# time <- c('07:00 – 08:00', '08:00 – 09:00', '12:00 – 13:00', '13:00 – 14:00',
#          '17:00 – 18:00', '18:00 – 19:00', '19:00 – 20:00')
# flag_no_duplicate_activities <- 1
# load(here::here("data/clean_sport_schedule.rda"))
# optimize_output <- optimize_schedule(clean_sport_schedule, date, activity, time, calburn, weight,flag_duplicate_activities)
# # optimize_output[1] # 1 if successful and 0 if fail
# # sum(optimize_output$table_result$calburn) # 753.375

optimize_schedule <- function(clean_sport_schedule, date, activity, time, calburn, weight, flag_no_duplicate_activities = 0) {
  library(lpSolve)
  library(data.table)
  library(dplyr)
  
  ### Filter Date and Activity from clean_sport_schedule  
  cleanscheduletemp <- clean_sport_schedule %>% 
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
  
  ### Just for your understanding: You can delete these comments. I pasted it in readme file.
  ### Optimization 
  # Objective function: minimize the number of chosen activities
  #           Equation: min sum(x_i)
  #                     where x_i = 1 if the activity i is chosen and 0 if it's not chosen
  # Constraints:
  # 1) The total calorie burn must exceed the target calorie
  #           Equation: sum(x_i*cal_i) >= calburn
  #                     where cal_i is the calorie burn of activity i and calburn is the target calorie
  #
  # 2) No overlapping time slots. 
  # We set constraints such that the optimizer won't select 2 or more activities that occur at the same time
  # for example, if activity A starts at 8.00 and ends at 9.00 and activity B starts at 8.45 and ends at 9.15,
  # they cannot be selected together (i.e. only one of them can be selected) 
  #           Equation: x_a + x_b + x_c +... <= 1 for all overlapping time intervals
  #                   if activity a, b, c,... have overlapping time slots 
  # 
  # 3) [Optional] Do not select the same activity
  # We set constraints such that the same activity cannot be selected.
  # for example, if there are several Football sessions, only 1 Football session can be selected.
  #           Equation: x_i + x_j + x_k + ... <= 1 for all duplicate activities
  #                   if activity i, j, k,... are the same activity 
  #
  
  n_activity <- nrow(table_opt)
  # Find calories burn per activity
  table_opt <- table_opt %>% mutate(calburn = p*weight)
  
  # For Constraint 2. Find overlapping activities
  table_opt_datatable <- as.data.table(table_opt)
  table_opt_datatable$`Start time` <- table_opt_datatable$`Start time`+0.01 
  setkey(table_opt_datatable, 'Start time', 'End time')
  overlap_ix <- foverlaps(table_opt_datatable, table_opt_datatable, type="any", which=TRUE)
  overlap_ix <- overlap_ix[as.logical(!(overlap_ix[,1] == overlap_ix[,2])),]
  overlap_ix <- as.data.frame(overlap_ix)
  checkoverlap = !is.na(overlap_ix[1,1]) # TRUE if there are overlapping activities/ FALSE if no overlap
  
  if (checkoverlap) {
    num_combination_overlap <- nrow(overlap_ix)
    overlap_constraints <- matrix(0,num_combination_overlap,n_activity)
    for (i in 1:num_combination_overlap) {
      overlap_constraints[i,as.numeric(overlap_ix[i,])] <- 1
    }
    overlap_constraints <- overlap_constraints[!duplicated(overlap_constraints),,drop = FALSE]
    num_combination_overlap <- nrow(overlap_constraints)
    rhs_overlap <- c(rep(1, num_combination_overlap))
    dir_overlap <- c(rep('<=', num_combination_overlap))
  }
  
  # For Constraint 3. we do not allow to choose the same activity names, add this constraint
  if (flag_no_duplicate_activities) {
    n_activity <- nrow(table_opt)
    
    ActivityNames <- table_opt$Activity
    ActivityNames_dup_uniq <- unique(ActivityNames[duplicated(ActivityNames)])
    if (!is_empty(ActivityNames_dup_uniq)) {
      n_dup <- length(ActivityNames_dup_uniq)
      duplicate_constraints <- matrix(0,n_dup,n_activity)
      for (i in 1:n_dup) {
        duplicate_constraints[i,] <- as.numeric(ActivityNames_dup_uniq[i] == ActivityNames)
      }
      rhs_dup <- c(rep(1, n_dup))
      dir_dup <- c(rep('<=', n_dup))
    } 
    else {
      # If there is no duplicate activity, set this flag back to 1 (so it won't add the constraints to f.con)
      flag_no_duplicate_activities <- 0
    }
  }
  
  # Set the objective function
  f.obj <- c(rep(1, n_activity))
  
  # Constraint 1. The total calorie burn must exceed the target calorie
  f.con <- matrix(table_opt$calburn, nrow = 1)
  f.dir <- c(">=")
  f.rhs <- c(calburn)
  
  # Constraint 2. Overlapping time slots
  if (checkoverlap) {
    f.con <- rbind(f.con,overlap_constraints)
    f.dir <- c(f.dir,dir_overlap)
    f.rhs <- c(f.rhs, rhs_overlap)
  }
  
  # Constraint 3. No duplicate activities
  if (flag_no_duplicate_activities) {
    f.con <- rbind(f.con,duplicate_constraints)
    f.dir <- c(f.dir,dir_dup)
    f.rhs <- c(f.rhs, rhs_dup)
  }
  
  # Run an integer optimization
  optim_output <- lp("min", f.obj, f.con, f.dir, f.rhs, int.vec = 1:n_activity, all.bin = TRUE)
  solution <- optim_output$solution
  if (optim_output$status == 0) { # setting from 'lp': if successful, status = 0
    optim_result = 1 #setting successful = 1 by ourselves to return in list
    cat(sprintf('The optimization is successful\n'))
  } else {
    # If the feasible solution cannot be found, find a combination with the highest calorie burn
    f.obj <- matrix(table_opt$calburn, nrow = 1)
    f.dir[1] <- "<"
    optim_output <- lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:n_activity, all.bin = TRUE)
    solution <- optim_output$solution
    if (optim_output$status == 0) {
      optim_result = 2 # fail but the function returns the solution with the highest total calories burn 
      cat(sprintf('The target calorie burn can not be met\n'))
    }
    else {
      optim_result = 0 #fail
      cat(sprintf('No feasible solution found\n'))
    }  
  }
  table_result = table_opt[as.logical(solution),]
  return(list(optim_result = optim_result, table_result = table_result, activity_selected = table_opt, totalcal = sum(table_result$calburn), totalduration = sum(table_result$Duration_min)))
  
}

#' @title piechart Function
#' @description 
#' @param 
#' @param  
#' @param 
#' @param 
#' @param 
#' @param 
#' @return  
#' @export
#' @examples

## Example
# calburn <- 500
# date <- c('2022-12-14')
# activity <- c('Aquagym', 'Zumba', 'Pilates', 'Agrès',
#              'Tai ji quan / Tous niveaux',
#              'Musculation connectée / 1. Introduction',
#              'Cirque', 'Aviron / Débutants', 'Salsa cubaine / Débutants')
# weight <- 50
# time <- c('07:00 – 08:00', '08:00 – 09:00', '12:00 – 13:00', '13:00 – 14:00',
#          '17:00 – 18:00', '18:00 – 19:00', '19:00 – 20:00')
# flag_no_duplicate_activities <- 1
# load(here::here("data/clean_sport_schedule.rda"))
# optimize_output <- optimize_schedule(clean_sport_schedule, date, activity, time, calburn, weight,flag_no_duplicate_activities)
# optim_plot <- optimize_output$table_result
# pie_optim(optim_plot) #call function

pie_optim <- function(optim_plot){
  library(plotly)
  library(ggplot2)
  library(dplyr)
  
  # Create Data
  data <- data.frame(
    group=optim_plot$Activity,
    value_burn=optim_plot$calburn,
    value_duration=optim_plot$Duration_min
  )
  
  data <- data %>% 
    arrange(desc(group)) %>%
    mutate(propb = value_burn / sum(data$value_burn) *100) %>%
    mutate(yposb = cumsum(propb)- 0.5*propb) 
  
  data <- data %>% 
    arrange(desc(group)) %>%
    mutate(propd = value_duration / sum(data$value_duration) *100) %>%
    mutate(yposd = cumsum(propd)- 0.5*propd)
  
# Calburn chart
  calburnplot <- plot_ly(data, labels = ~group, values = ~value_burn, type = 'pie')
  calburnplot <- calburnplot %>% 
    layout(title = '<b> Calorie Burn <b>',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           legend = list(orientation = 'h'))
  
# Duration chart
  durationplot <- plot_ly(data, labels = ~group, values = ~value_duration, type = 'pie')
  durationplot <- durationplot %>% 
    layout(title = '<b> Exercise Duration (min) <b>',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           legend = list(orientation = 'h'))

  return(list(calburnplot, durationplot))
}