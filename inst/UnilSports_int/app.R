####################### SHINY ############################################
library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(bslib)

source("R/functions.R")
sport_schedule <- webscrape_sports()
met_values     <- webscrape_MET()
load(here::here("data/mapping.rda"))
clean_sport_schedule <- get_cleanschedule_met(sport_schedule,met_values,mapping)

ui <- navbarPage(strong("Sports Unil Plan"), 
                 theme = bslib::bs_theme(bootswatch = "united", 
                                         base_font = font_google("Montserrat")),
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput("date", 
                                 label = strong("Choose a date you want to workout"),
                                 choices = unique(clean_sport_schedule$Date),
                                 selected = unique(clean_sport_schedule$Date)[1]),
                     
                     selectInput("time", 
                                 label = strong("Choose time"),
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
                                             '17:00 – 18:00',
                                             '18:00 – 19:00',
                                             '19:00 – 20:00',
                                             '20:00 – 21:00',
                                             '21:00 – 22:00',
                                             '22:00 – 23:00'),
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
)

server <- function(input, output, session) {
  
  observe({
    update_date <- input$date
    update_schedule <- filter(clean_sport_schedule, Date == update_date)
    
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
  
}

shinyApp(ui, server)