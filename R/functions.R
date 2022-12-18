#' @title Webscraping function for Sports Schedule
#' @description A function that returns the sports table schedule from the Unil Sports Center.
#' @param days The number of days that you want to retrieve from the Unil Sports Center website. The defaut number of days is 7. 
#' @return  A data frame containing the sports schedule of the number of days selected
#' @export
#' @examples
#' webscrap_sports()
webscrape_sports <- function(days = 7) {
  library(rvest)
  library(tidyverse)
  library(lubridate)

  
  if (is.numeric(days) == F) { 
    stop("'days' must be numeric")
  }

  schedule.html <- read_html("https://sport.unil.ch/?mid=92") 
  a_elements <- schedule.html %>% 
    html_nodes(".summary .level2>li>a") %>% 
    html_attr("href")
  a_elements <- paste("https://sport.unil.ch/",a_elements, sep = "") 
  

  new_list <- list()
  
  
  new_list <- append(new_list,a_elements[1])
  

  days <- days -1
  

  for (i in 1:days) {
    if (i == 1) {
      schedule.html <- read_html("https://sport.unil.ch/?mid=92") 
      a_element <- schedule.html %>% 
        html_nodes(".planning-navigation .next>a") %>% 
        html_attr("href")
      new.html <- paste("https://sport.unil.ch/",a_element, sep = "") 
      new_list <- append(new_list,new.html)
    } else {
      schedule.html <- read_html(new.html) 
      a_element <- schedule.html %>% 
        html_nodes(".planning-navigation .next>a") %>% 
        html_attr("href")
      new.html <- paste("https://sport.unil.ch/",a_element, sep = "") 
      new_list <- append(new_list,new.html)
    }
  }
  
 
  new_vector <- unlist(new_list)
  

  table_new <-data.frame()
  sport_schedule <- data.frame()
  

  days <- days +1
  
 
  for (i in 1:days) {
    html <- read_html(new_vector[i])
    table_new <- rvest::html_element(html, css = "table.quotidien" ) %>% 
      rvest::html_table() %>% 
      slice(-1) %>% 
      rename("Timetable" = "X1", "Activity"= "X2", "Location"="X3")
    previous.date <- html %>%
      html_nodes("div.planning-navigation td.back") %>% 
      html_text2()
    previous.date <- gsub("\\.", "-", previous.date)
    previous.date <- parse_date_time(previous.date, orders = c("ymd", "dmy", "mdy"))
    todays.date <- as.Date(previous.date) + 1
    table_new <-  table_new %>% 
      mutate(Date = todays.date) %>% 
      relocate(Date, .before = Timetable)
    sport_schedule<- rbind(sport_schedule,table_new)
  }
  return(sport_schedule)
}




#' @title Webscraping function for MET Values
#' @description A function that returns the MET values table of more than 800 activities webscrapped from the golf.procon.org website.
#' @return  A data frame containing the MET values per sport activity.
#' @export
#' @examples
#' webscrape_MET()
webscrape_MET <- function() {

  library(rvest)
  library(tidyverse)

  
  met_values.html <- read_html("https://golf.procon.org/met-values-for-800-activities/") 
  
  
  element <- met_values.html %>% 
    html_element(css = "tbody.row-hover")
  element
  
  met_values <- element %>% 
    html_table() %>% 
    rename("Activity" = "X1", "Specific Motion"= "X2", "METs"="X3")

  return(met_values)
}




#' @title Wrangling Function
#' @description 
#'     A function that cleans the output (data rame) extracted from the webscrape_sports function. 
#'     This function maps the MET values into the data in order to calculate the calorie burn amount per activity.  
#' @return  A data frame containing the activities per day with their correspondingly Met values.
#' @export
#' @examples
#' get_cleanschedule_met()
get_cleanschedule_met <- function(sport_schedule,met_values) {
  library(dplyr)
  library(here)
  
  mapping
  
  
  unique(sport_schedule$Timetable)
  sport_schedule <- sport_schedule %>% filter(!Timetable == "tout le jour")

  sport_schedule <- sport_schedule %>% filter(!Activity == "Sport libre")
  

  sport_schedule <- sport_schedule %>% 
    left_join(mapping, by = "Activity") %>%
    left_join(select(met_values,-Activity), by = "Specific Motion")
  
  timetemp <- as.data.frame(matrix(as.numeric(gsub("\\:",".",unlist(strsplit(sport_schedule$Timetable, "\u2013")))),
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
#'     Function that evaluates the calorie burn per activity and time according to the parameters entered.  
#'     This optimization function calculates the calaory burn subject to 3 constraints: 
#'     1. The total calorie burn must exceed the target calorie Equation: sum(x_i*cal_i) >= calburn where cal_i is the calorie burn of activity i and calburn is the target calorie.
#'     2. No overlapping time slots. The optimizer won't select 2 or more activities that occur at the same time. For example, if activity A starts at 8.00 and ends at 9.00 and activity B starts at 8.45 and ends at 9.15, they cannot be selected together (i.e. only one of them can be selected) Equation: x_a + x_b + x_c +... <= 1 for all overlapping time intervals if activity a, b, c,... have overlapping time slots. 
#'     3. Do not select the same activity.The same activity cannot be selected. For example, if there are several Football sessions, only 1 Football session can be selected. Equation: x_i + x_j + x_k + ... <= 1 for all duplicate activities if activity i, j, k,... are the same activity
#' @param clean_sport_schedule The data frame output from the get_cleanschedule_met. The output of this function should not be modified, so this function can apply the integer optimization technique properly.
#' @param date the date that the user wants to search for activities in.
#' @param activity type of sports activity the user wants to attend. 
#' @param time timing the the user is available in.
#' @param calburn the minimum number of calories the user wants to burn.
#' @param weight the actual weight in kg of the user.
#' @param flag_no_duplicate_activities decides whether to show duplicates in the output or not. If  flag_no_duplicate_activities = TRUE, there will be no duplicate activities in the output.
#' @return returns a list of 5 items, the optim_result, table_result (which has all the details of the best activity/ies, activity_selected, totalcal, totalduration)   
#' @export
#' @examples
#' calburn <- 500
#' date <- c('2022-12-14')
#' activity <- c('Aquagym', 'Zumba', 'Pilates', 
#'              'Tai ji quan / Tous niveaux',
#'              'Cirque')
#' weight <- 50
#' time <- c('07:00 \u2013 08:00', '08:00 \u2013 09:00', '12:00 \u2013 13:00', '13:00 \u2013 14:00',
#'          '17:00 \u2013 18:00', '18:00 \u2013 19:00', '19:00 \u2013 20:00')
#' flag_no_duplicate_activities <- 1
#' load(here::here("data/clean_sport_schedule.rda"))
#' optimize_output <- optimize_schedule(clean_sport_schedule, date, activity, time, calburn, weight,flag_no_duplicate_activities)
optimize_schedule <- function(clean_sport_schedule, date, activity, time, calburn, weight, flag_no_duplicate_activities = 0) {
  library(lpSolve)
  library(data.table)
  library(dplyr)
  library(rlang)

  
  cleanscheduletemp <- clean_sport_schedule %>% 
    filter(Date == date) %>%
    filter(Activity %in% activity)
  

  cleanscheduletemp <- cleanscheduletemp %>% mutate(time = 0)
  cleanscheduletemp <- cleanscheduletemp[, c(1,2,3,9,4,5,6,7,8)]
  
  timetemp <- as.data.frame(matrix(as.numeric(gsub("\\:",".",unlist(strsplit(sort(time), "\u2013")))),
                                   ncol = 2, byrow = TRUE))
  

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
  

  k <- nrow(selected_time)
  for (j in 1:k){
    cleanscheduletemp$time <- cleanscheduletemp$time | 
      (cleanscheduletemp$`Start time` >= selected_time[j,1] &
         cleanscheduletemp$`End time` <= selected_time[j,2])
    cleanscheduletemp$time <- 1*cleanscheduletemp$time
  }
  table_opt <- cleanscheduletemp %>% filter(time == 1) 
  
  
  n_activity <- nrow(table_opt)
  
  table_opt <- table_opt %>% mutate(calburn = p*weight)
  

  table_opt_datatable <- as.data.table(table_opt)
  table_opt_datatable$`Start time` <- table_opt_datatable$`Start time`+0.01 
  setkey(table_opt_datatable, 'Start time', 'End time')
  overlap_ix <- foverlaps(table_opt_datatable, table_opt_datatable, type="any", which=TRUE)
  overlap_ix <- overlap_ix[as.logical(!(overlap_ix[,1] == overlap_ix[,2])),]
  overlap_ix <- as.data.frame(overlap_ix)
  checkoverlap = !is.na(overlap_ix[1,1]) 
  
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
      
      flag_no_duplicate_activities <- 0
    }
  }
  
  
  f.obj <- c(rep(1, n_activity))
  
  
  f.con <- matrix(table_opt$calburn, nrow = 1)
  f.dir <- c(">=")
  f.rhs <- c(calburn)
  
  
  if (checkoverlap) {
    f.con <- rbind(f.con,overlap_constraints)
    f.dir <- c(f.dir,dir_overlap)
    f.rhs <- c(f.rhs, rhs_overlap)
  }
  
  
  if (flag_no_duplicate_activities) {
    f.con <- rbind(f.con,duplicate_constraints)
    f.dir <- c(f.dir,dir_dup)
    f.rhs <- c(f.rhs, rhs_dup)
  }
  
  
  optim_output <- lp("min", f.obj, f.con, f.dir, f.rhs, int.vec = 1:n_activity, all.bin = TRUE)
  solution <- optim_output$solution
  if (optim_output$status == 0) { 
    optim_result = 1
    cat(sprintf('The optimization is successful\n'))
  } else {

    f.obj <- matrix(table_opt$calburn, nrow = 1)
    f.dir[1] <- "<"
    optim_output <- lp("max", f.obj, f.con, f.dir, f.rhs, int.vec = 1:n_activity, all.bin = TRUE)
    solution <- optim_output$solution
    if (optim_output$status == 0) {
      optim_result = 2 
      cat(sprintf('The target calorie burn can not be met\n'))
    }
    else {
      optim_result = 0 
      cat(sprintf('No feasible solution found\n'))
    }  
  }
  table_result = table_opt[as.logical(solution),]
  return(list(optim_result = optim_result, table_result = table_result, activity_selected = table_opt, totalcal = sum(table_result$calburn), totalduration = sum(table_result$Duration_min)))
  
}




#' @title piechart Function
#' @description a function that prints a pie chart reflecting the output of the optimization function
#' @param optim_plot the output that is obtained in the table_result section of the optimize_schedule function output. 
#' @return  a pie chart of the table_result
#' @export
#' @examples
#' calburn <- 500
#' date <- c('2022-12-14')
#' activity <- c('Aquagym', 'Zumba', 'Pilates',
#'              'Tai ji quan / Tous niveaux',
#'              'Cirque')
#' weight <- 50
#' time <- c('07:00 \u2013 08:00', '08:00 \u2013 09:00', '12:00 \u2013 13:00', '13:00 \u2013 14:00',
#'          '17:00 \u2013 18:00', '18:00 \u2013 19:00', '19:00 \u2013 20:00')
#' flag_no_duplicate_activities <- 1
#' load(here::here("data/clean_sport_schedule.rda"))
#' optimize_output <- optimize_schedule(clean_sport_schedule, date, activity, time, calburn, weight,flag_no_duplicate_activities)
#' optim_plot <- optimize_output$table_result
#' pie_optim(optim_plot) #call function
pie_optim <- function(optim_plot){
  library(plotly)
  library(ggplot2)
  library(dplyr)

  
  
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
  

  calburnplot <- plot_ly(data, labels = ~group, values = ~value_burn, type = 'pie')
  calburnplot <- calburnplot %>% 
    layout(title = '<b> Calorie Burn <b>',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           legend = list(orientation = 'h'))
  

  durationplot <- plot_ly(data, labels = ~group, values = ~value_duration, type = 'pie')
  durationplot <- durationplot %>% 
    layout(title = '<b> Exercise Duration (min) <b>',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           legend = list(orientation = 'h'))

  return(list(calburnplot, durationplot))
}






#' @title User Interface function for UnilSports Shiny Application
#' @description This is a function that builds a user interface for the UnilSports shiny application. This function is then used as the first parameter of the UnilSports_gui function. 
#' @param clean_sport_schedule The data frame output from the get_cleanschedule_met. The output of this function should not be modified, so this function can apply the integer optimization technique properly.
#' @return  a shiny web application dashboard that builds a user interface for the UnilSports shiny application. This function is then used as one of  the parameters of the  UnilSports_gui function. 
#' @export
#' @examples
#' build_ui(clean_sport_schedule)
build_ui <- function(clean_sport_schedule) {
  library(shiny)
  return(shiny::navbarPage(shiny::strong("Sports Unil Plan"), 
             theme = bslib::bs_theme(bootswatch = "united", 
                                     base_font = sass::font_google("Montserrat")),
             
             shiny::sidebarLayout(
               sidebarPanel(
                 
                 selectInput("date", 
                             label = strong("Choose a date you want to workout"),
                             choices = unique(clean_sport_schedule$Date),
                             selected = unique(clean_sport_schedule$Date)[1]),
                 
                 selectInput("time", 
                             label = strong("Choose time"),
                             choices = c('07:00 \u2013 08:00', 
                                         '08:00 \u2013 09:00', 
                                         '09:00 \u2013 10:00', 
                                         '10:00 \u2013 11:00',
                                         '11:00 \u2013 12:00', 
                                         '12:00 \u2013 13:00', 
                                         '13:00 \u2013 14:00',
                                         '14:00 \u2013 15:00',
                                         '15:00 \u2013 16:00',
                                         '16:00 \u2013 17:00',
                                         '17:00 \u2013 18:00',
                                         '18:00 \u2013 19:00',
                                         '19:00 \u2013 20:00',
                                         '20:00 \u2013 21:00',
                                         '21:00 \u2013 22:00',
                                         '22:00 \u2013 23:00'),
                             selected = NULL,
                             multiple = TRUE),
                 
                 selectInput("activity", 
                             label = strong("Choose activity"),
                             choices = unique(filter(clean_sport_schedule, Date == unique(clean_sport_schedule$Date)[1])$Activity),
                             multiple = TRUE),
                 
                 numericInput("calburn",
                              label = strong("Calories you want to burn today (calories)"),
                              value = "1500",
                              step = 50),
                 
                 numericInput("weight",
                              label = strong("Your weight (kg)"),
                              value = "50"),
                 checkboxInput("no_dup", "Do not choose the same activity", FALSE),
                 actionButton("opt",
                              label = strong("  Go!"),
                              icon = icon("dumbbell"))
               ),
               
               mainPanel( 
                 imageOutput("image", height = "50px", width = "auto"), 
                 tabsetPanel( type = "tabs",
                              tabPanel("Optimization",
                                       helpText("Optimization Result"), 
                                       tableOutput("optim_table"),
                                       textOutput("optim_result"),
                                       fluidRow(
                                         column(6, br(), br(),
                                                plotlyOutput("calburnplot",
                                                             width = "100%",
                                                             height = "350px")),
                                         column(6,br(), br(),
                                                plotlyOutput("durationplot",
                                                             width = "100%",
                                                             height = "350px")))
                              ),
                              tabPanel("Available Activities"  , 
                                       helpText("List of available activities with respect to the criteria"), 
                                       tableOutput("activity_table"))
                 )
               )
             )
  ))
}






#' @title Server function for UnilSports Shiny Application
#' @description This is a function that builds a server for the UnilSports shiny application. This function is then used as the secoond parameter in the  UnilSports_gui function. 
#' @param clean_sport_schedule The data frame output from the get_cleanschedule_met. The output of this function should not be modified, so this function can apply the integer optimization technique properly.
#' @return  a shiny server output 
#' @export
#' @examples
#' build_server(clean_sport_schedule)
build_server <- function(clean_sport_schedule) {
  return(function(input, output, session) {
    
    observe({
      update_date <- input$date
      update_schedule <- filter(clean_sport_schedule, Date == update_date)
      
      update_time <- input$time

      if (!is.null(update_time)) {
        update_schedule <- mutate(update_schedule,time = 0)
        timetemp <- as.data.frame(matrix(as.numeric(gsub("\\:",".",unlist(strsplit(sort(update_time), "\u2013")))),ncol = 2, byrow = TRUE))
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
        
        k <- nrow(selected_time)
        for (j in 1:k){
          update_schedule$time <- update_schedule$time | 
            (update_schedule$`Start time` >= selected_time[j,1] &
               update_schedule$`End time` <= selected_time[j,2])
          update_schedule$time <- 1*update_schedule$time
        }
        update_schedule <- update_schedule %>% filter(time == 1) 
      }
      update_activity <- unique(update_schedule)$Activity
      updateSelectInput(session, "activity", choices = update_activity)
    })
    
    optimize_output <- eventReactive(input$opt, {
      outputtemp <- optimize_schedule(clean_sport_schedule, input$date, 
                                      input$activity,input$time, input$calburn,
                                      input$weight,as.numeric(input$no_dup))  
    })
    
    output$optim_table <- renderTable(hover = T,{
      
      data.frame(optimize_output()[2]) %>%
        select(-c(table_result.Date, table_result.time, table_result.METs, table_result.p)) %>% 
        rename( `Start Time` = table_result.Start.time,
                `End Time` = table_result.End.time,
                `Duration (min)` = table_result.Duration_min,
                Activity = table_result.Activity, 
                Location = table_result.Location,
                Calburn = table_result.calburn)
    })
    
    output$optim_result <- renderText({ 
      if (optimize_output()[1] == 1) { 
        sprintf('The optimization is successful! Total Exercise Duration: %.0f min, Total Calorie Burn: %.0f', optimize_output()[5], optimize_output()[4])
      } 
      else if (optimize_output()[1] == 2) {
        sprintf('Cannot reach the calorie burn target. The optimizer returns the solution with the highest possible calorie burn. Total Exercise Duration: %.0f min, Total Calorie Burn: %.0f', optimize_output()[5], optimize_output()[4])
      }
      else {
        sprintf(cat('No feasible solution found'))
      }
    })
    
    output$calburnplot <- renderPlotly({
      optim_plot <- optimize_output()$table_result
      p <- pie_optim(optim_plot)
      p[[1]]
    })
    
    output$durationplot <- renderPlotly({
      optim_plot <- optimize_output()$table_result
      p <- pie_optim(optim_plot)
      p[[2]]
    })
    
    output$activity_table <- renderTable({
      data.frame(optimize_output()[3]) %>%
        select(-c(activity_selected.Date, activity_selected.time, activity_selected.METs, activity_selected.p)) %>% 
        rename(`Start Time` = activity_selected.Start.time,
               `End Time` = activity_selected.End.time,
               `Duration (min)` = activity_selected.Duration_min,
               Activity = activity_selected.Activity, 
               Location = activity_selected.Location,
               Calburn = activity_selected.calburn)
    })
    
    output$image <- renderImage({
      list(src= "image.png", align= "right", height="100%", width="auto", 
           deleteFile=FALSE)
    })
    
  })
}






#' @title Shiny Application function for UnilSports package
#' @description This is a function that provides a dashboard for users to input their desired time tables, classes, calories burned to get a set of classes that adhere to the respective requirements
#' @param clean_sport_schedule The data frame output from the get_cleanschedule_met. The output of this function should not be modified, so this function can apply the integer optimization technique properly.
#' @return  a shiny web application dashboard that provides the user with the available sports classes based on certain parameters entered by the user.
#' @export
#' @examples
#' clean_sport_schedule <- get_cleanschedule_met(sport_schedule,met_values)
UnilSports_gui <- function(clean_sport_schedule) {
  
  library(shiny)
  library(shinythemes)
  library(dplyr)
  library(plotly)
  library(bslib)
  
  
  run_shiny <- shinyApp(build_ui(clean_sport_schedule), build_server(clean_sport_schedule))
  return(run_shiny)
}









#' @title Start Function for Shiny Application 
#' @description This is a function that automatically runs the shiny function created and opens the dashboard for the user
#' @return  A dashboard for the user to interact with
#' @export
#' @examples
#' startApp()
startApp <- function() {

  sport_schedule <- webscrape_sports()
  met_values     <- webscrape_MET()
  clean_sport_schedule <- get_cleanschedule_met(sport_schedule,met_values)
  UnilSports_gui(clean_sport_schedule)
}
