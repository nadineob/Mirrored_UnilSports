library(rvest)
library(tidyverse)

met_values.html <- read_html("https://golf.procon.org/met-values-for-800-activities/") # webpage

# A faster way to retrieve the table per day

element <- met_values.html %>% 
  html_element(css = "tbody.row-hover")
element

met_values <- element %>% 
  html_table() %>% 
  rename("Activity" = "X1", "Specific Motion"= "X2", "METs"="X3")

# Create data to use for the functions
usethis::use_data(met_values, overwrite = TRUE) # create data file
