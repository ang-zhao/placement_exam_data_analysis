library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)

##################################
###### Data processing ###########
##################################

# Scrape Wikipedia page
url = "https://en.wikipedia.org/wiki/List_of_natural_disasters_by_death_toll"
page = read_html(url) # read HTML
tables = page %>% html_table(fill = TRUE) # extract tables
tab20 = tables[[2]] #20th century table
tab21 = tables[[3]] #21st century table

# Add Year column to the 20th century disasters
tab20$Year = as.integer(sapply(tab20$Event,str_extract, "\\d+"))

# Add Type column to the 20th century disasters
event_to_type <- function(event) {
  if (str_detect(event, regex("flood", ignore_case = T))) {
    return("Flood") 
  } else if (str_detect(event, regex("earthquake", ignore_case = T))) {
    return("Earthquake") 
  } else if (str_detect(event, regex("cyclone", ignore_case = T))) {
    return("Tropical cyclone") 
  } else return(NA)
}
tab20$Type = sapply(tab20$Event, event_to_type)

# Combine tables
disasters = bind_rows(tab20, tab21)

# A function to convert death toll to number
convert_dt <- function(dt, hdt) {
  dt = str_replace_all(dt, ",", "") #remove commas from death toll, e.g. 2,400
  hdt = str_replace_all(hdt, ",", "")
  
  if (is.na(dt)) { #check if there is only an upper bound
    return(as.numeric(str_extract(hdt, "\\d+")))
  } else if(str_detect(dt, "-") || str_detect(dt, "–")) { #check if death toll is a range, i.e. contains "-"
    range = as.numeric(str_extract_all(dt, "\\d+")[[1]]) #extract mean
    return(mean(range))
  } else if(str_detect(dt, "\\+")) { #check if death toll is a lower bound, i.e. contains "+"
    return(as.numeric(str_extract(dt, "\\d+"))) #extract number
  } else {
    return(as.numeric(str_extract(dt, "\\d+")))
  }
}

# Apply convert_dt() to Death toll column
disasters$`Death toll` = mapply(convert_dt, disasters$`Death toll`, disasters$`Death toll (Highest estimate)`)

##################################
###### Plotting ###########
##################################

disasters %>% ggplot(aes(x = as.Date(Year), y = `Death toll`, color = Type)) +
  geom_point() +
  labs(title = "Death toll by year and type of disaster",
       x = "Year",
       y = "Death Toll") +
  theme_minimal()
