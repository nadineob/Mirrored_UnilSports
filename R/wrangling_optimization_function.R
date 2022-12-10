#############################Wrangling Function#################################
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

############################Optimization Function###############################
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
  
  timetemp <- as.data.frame(matrix(as.numeric(gsub("\\:",".",unlist(strsplit(time, "–")))),
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
      if(i==n){
        selected_time <- rbind(selected_time, output_temp)
      }
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
  return(list(optim_result = optim_result, table_result = table_opt[as.logical(optim_output$solution),]))
  
}

# Example 
# Define inputs
calburn <- 500
date <- c('2022-12-02')
activity <- c('Aquagym', 'Zumba', 'Pilates', 'Agrès',
              'Tai ji quan / Tous niveaux', 
              'Musculation connectée / 1. Introduction',
              'Cirque', 'Aviron / Débutants', 'Salsa cubaine / Débutants')

weight <- 50
time <- c('07:00 – 08:00', '08:00 – 09:00', '12:00 – 13:00', '13:00 – 14:00',
          '17:00 – 18:00', '18:00 – 19:00', '19:00 – 20:00')

cleanschedule<- get_cleanschedule_met()
optimize_output <- optimize_schedule(cleanschedule, date, activity, time, calburn, weight) 
optimize_output[1] # 1 if successful and 0 if fail
sum(optimize_output$table_result$calburn) # 753.375
