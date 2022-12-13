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
  return(usethis::use_data(sport_schedule, overwrite = TRUE))
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
  return(usethis::use_data(met_values, overwrite = TRUE))
}
