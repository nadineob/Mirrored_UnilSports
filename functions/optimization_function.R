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

# Filter Date and Activity from cleanschedule
cleanscheduletemp <- cleanschedule %>% 
  filter(Date == date) %>%
  filter(Activity %in% activity)

# Prepare time to be ready to filter 
cleanscheduletemp <- cleanscheduletemp %>% mutate(time = 0)
cleanscheduletemp <- cleanscheduletemp[, c(1,2,3,9,4,5,6,7,8)]

timetemp <- as.data.frame(matrix(as.numeric(gsub("\\:",".",unlist(strsplit(time, "–")))),
                                 ncol = 2, byrow = TRUE))

n <- length(time)
output <- data.frame()
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
      output <- rbind(output, output_temp)
      output_temp <- timetemp[i,]
    }
    if(i==n){
      output <- rbind(output, output_temp)
    }
  }
}

k <- nrow(output)
for (j in 1:k){
  cleanscheduletemp$time <- cleanscheduletemp$time | 
    (cleanscheduletemp$`Start time` >= output[j,1] &
    cleanscheduletemp$`End time` <= output[j,2])
  cleanscheduletemp$time <- 1*cleanscheduletemp$time
}

# Filter time from cleanscheduletemp
table_opt <- cleanscheduletemp %>% filter(time == 1)

# Optimization
# intall.package("lpSolve")
library(lpSolve)

# Set coefficients of the objective function
n_activity <- nrow(table_opt)
f.obj <- c(rep(1, n_activity))

# Find calories burn per activity
table_opt <- table_opt %>% mutate(calburn = p*weight)
# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negative constraint; it is automatically assumed
f.con <- matrix(table_opt$calburn, nrow = 1)

# Set unequality signs
f.dir <- c(">=")

# Set right hand side coefficients
f.rhs <- calburn

# Final value (z)
c <- lp("min", f.obj, f.con, f.dir, f.rhs, int.vec = 1:n_activity, all.bin = TRUE)
lp("min", f.obj, f.con, f.dir, f.rhs)$solution
