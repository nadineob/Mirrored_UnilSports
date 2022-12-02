
library(rvest)
library(tidyverse)
library(flextable)
library(lubridate)

schedule.html <- read_html("https://sport.unil.ch/?mid=92") # webpage

# A faster way to retrieve the table per day

element <- schedule.html %>% 
  html_element(css = "table.quotidien")
element

sport_schedule <- element %>% 
  html_table()
sport_schedule <- sport_schedule[-1,]

colnames(sport_schedule) <- c("Timetable", "Activity", "Location")

previous.date <- schedule.html %>%
  html_nodes("div.planning-navigation td.back") %>% 
  html_text2()
previous.date <- gsub("\\.", "-", previous.date)
previous.date <- parse_date_time(previous.date, orders = c("ymd", "dmy", "mdy"))
todays.date <- as.Date(previous.date) + 1

sport_schedule.today <-  sport_schedule %>% 
  mutate(Date = todays.date) %>% 
  relocate(Date, .before = Timetable)

# A longer way to retrieve the table per day

timetable <- schedule.html %>%
  html_nodes("td.horaire") %>% 
  html_text2()
head(timetable) 

activity <- schedule.html %>%
  html_nodes("div.planning-content td a") %>% 
  html_text2()
head(activity) 

location <- schedule.html %>%
  html_nodes("td.lieu") %>% 
  html_text2()
head(location) 

previous.date <- schedule.html %>%
  html_nodes("div.planning-navigation td.back") %>% 
  html_text2()
previous.date <- gsub("\\.", "-", previous.date)
previous.date <- parse_date_time(previous.date, orders = c("ymd", "dmy", "mdy"))
todays.date <- as.Date(previous.date) + 1

sport_schedule.today <- tibble(data.frame(
  Timetable = timetable,
  Activity = activity,
  Location = location)) %>% 
  mutate(Date = todays.date) %>% 
  relocate(Date, .before = Timetable)

#----------------------------------------------------------------------------
# Retrieve 8 days of Unil Sports Center

# Retrieve the links for each of the days shown on the website
schedule.html <- read_html("https://sport.unil.ch/?mid=92") # webpage
a_elements <- schedule.html %>% 
  html_nodes(".summary .level2>li>a") %>% 
  html_attr("href")
a_elements <- paste("https://sport.unil.ch/",a_elements, sep = "") 

# Creating two empty dataframes for table
table_new <-data.frame()
sport_schedule <- data.frame()

# Create sports table with 8 dates
for (i in 1:8) {
  html <- read_html(a_elements[i])
  table_new <- rvest::html_element(html, css = "table.quotidien" ) %>% 
    rvest::html_table() %>% 
    slice(-1) %>% # Remove first Row (weird names) 
    rename("Timetable" = "X1", "Activity"= "X2", "Location"="X3")
  previous.date <- html %>%
    html_nodes("div.planning-navigation td.back") %>% # Retrieve the yesterdays date
    html_text2()
  previous.date <- gsub("\\.", "-", previous.date)
  previous.date <- parse_date_time(previous.date, orders = c("ymd", "dmy", "mdy"))
  todays.date <- as.Date(previous.date) + 1 # Retrieve todays date
  table_new <-  table_new %>% 
    mutate(Date = todays.date) %>% 
    relocate(Date, .before = Timetable)
  sport_schedule<- rbind(sport_schedule,table_new) # Create table
}

#-------------------------------------------------------------------------------

# Retrieve 60 days of Unil Sports Center

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


# Create list of the up coming 60 days
for (i in 1:60) {
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

# Create sports table with 61 dates
for (i in 1:61) {
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
  todays.date <- as.Date(previous.date) + 1 # Retrieve todays date
  table_new <-  table_new %>% 
    mutate(Date = todays.date) %>% 
    relocate(Date, .before = Timetable)
  sport_schedule<- rbind(sport_schedule,table_new) # Create table
}

# Create data to use for the functions
usethis::use_data(sport_schedule, overwrite = TRUE) # create data file
