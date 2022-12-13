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
  usethis::use_data(sport_schedule, overwrite = TRUE)
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
  usethis::use_data(met_values, overwrite = TRUE)
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
  library(readxl)
  library(here)
  
  # Import data
  #load(here::here("data/sport_schedule.rda"))
  #load(here::here("data/met_values.rda"))
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
#' date <- c('2022-12-02')
#' activity <- c('Aquagym', 'Zumba', 'Pilates', 'Agrès',
#'              'Tai ji quan / Tous niveaux', 
#'              'Musculation connectée / 1. Introduction',
#'              'Cirque', 'Aviron / Débutants', 'Salsa cubaine / Débutants')
#' weight <- 50
#' time <- c('07:00 – 08:00', '08:00 – 09:00', '12:00 – 13:00', '13:00 – 14:00',
#'          '17:00 – 18:00', '18:00 – 19:00', '19:00 – 20:00')
#' cleanschedule<- get_cleanschedule_met(data= clean_sport_schedule) 
#' # optimize_output <- optimize_schedule(cleanschedule, date, activity, time, calburn, weight) 
#' # optimize_output[1] # 1 if successful and 0 if fail
#' # sum(optimize_output$table_result$calburn) # 753.375
optimize_schedule <- function(cleanschedule, date, activity, time, calburn, weight) {
  # intall.package("lpSolve")
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

####################### SHINY ############################################
library(shiny)
library(shinythemes)
library(dplyr)

sport_schedule <- webscrape_sports()
met_values     <- webscrape_MET()
cleanschedule <- get_cleanschedule_met(sport_schedule,met_values)

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Sports Unil Plan"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("text"),
      
      selectInput("date", 
                  label = "Choose a date you want to workout",
                  choices = unique(cleanschedule$Date),
                  selected = unique(cleanschedule$Date)[1]),
      
      selectInput("time", 
                  label = "Choose time",
                  choices = c('07:00 – 08:00', 
                              '08:00 – 09:00', 
                              '09:00 – 10:00', 
                              '10:00 – 11:00',
                              '11:00 – 12:00', 
                              '12:00 – 13:00', 
                              '13:00 – 14:00',
                              '14:00 – 15:00',
                              '15:00 – 16:00',
                              '16:00 – 17:00',
                              '18:00 – 19:00',
                              '19:00 – 20:00',
                              '20:00 – 21:00',
                              '21:00 – 22:00',
                              '22:00 – 23:00'),
                  selected = NULL,
                  multiple = TRUE),
      
      selectInput("activity", 
                  label = "Choose activity",
                  choices = unique(filter(cleanschedule, Date == unique(cleanschedule$Date)[1])$Activity),
                  multiple = TRUE),
      
      numericInput("calburn",
                   label = "Calories you want to burn today (calories)",
                   value = "300"),
      
      numericInput("weight",
                   label = "Your weight (kg)",
                   value = "50"),
      
      actionButton("opt",
                   label = "Go!")
    ),
    
    mainPanel(
      tabsetPanel( type = "tabs",
                   tabPanel("Optimization", tableOutput("optim_table"),textOutput("optim_result"),plotOutput("piechart")),
                   tabPanel("Activities"  , tableOutput("activity_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    update_date <- input$date
    update_schedule <- filter(cleanschedule, Date == update_date)
    
    update_time <- input$time
    # If time is null, skip this
    if (!is.null(update_time)) {
      update_schedule <- mutate(update_schedule,time = 0)
      # Aggregate the connecting time slots 
      # e.g. if the selected time slots are 7am-8am and 8am-9am, combine them together into a single timeslot  i.e. 7am-9am
      timetemp <- as.data.frame(matrix(as.numeric(gsub("\\:",".",unlist(strsplit(sort(update_time), "–")))),ncol = 2, byrow = TRUE))
      n <- length(update_time)
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
        update_schedule$time <- update_schedule$time | 
          (update_schedule$`Start time` >= selected_time[j,1] &
             update_schedule$`End time` <= selected_time[j,2])
        update_schedule$time <- 1*update_schedule$time
      }
      update_schedule <- update_schedule %>% filter(time == 1) 
    }
    # Update the activity list by the new selected date
    update_activity <- unique(update_schedule)$Activity
    updateSelectInput(session, "activity", choices = update_activity)
  })
  
  optimize_output <- eventReactive(input$opt, {
    outputtemp <- optimize_schedule(cleanschedule, input$date, 
                                    input$activity,input$time, input$calburn,
                                    input$weight)  
  })
  
  output$optim_table <- renderTable({
    data.frame(optimize_output()[2]) %>%
      select(-c(table_result.Date, table_result.time, table_result.METs, table_result.p)) %>% 
      rename( `Start Time` = table_result.Start.time,
             `End Time` = table_result.End.time,
             `Duration (min)` = table_result.Duration_min,
             Activity = table_result.Activity, Location = table_result.Location,
             Carlburn = table_result.calburn)
  })
  
  output$optim_result <- renderText({ 
    if (optimize_output()[1] == 1) { 
      print('The optimization is successful!')
    } else {
      print('No feasible solution found')
    }
  })
  
  output$piechart <- plotOutput(
    optimize_output()[2]
  )
  
  output$activity_table <- renderTable({
    data.frame(optimize_output()[3]) %>%
      select(-c(activity_selected.Date, activity_selected.time, activity_selected.METs, activity_selected.p)) %>% 
      rename(`Start Time` = activity_selected.Start.time,
             `End Time` = activity_selected.End.time,
             `Duration (min)` = activity_selected.Duration_min,
             Activity = activity_selected.Activity, Location = activity_selected.Location,
             Carlburn = activity_selected.calburn)
  })
  
}

shinyApp(ui, server)
