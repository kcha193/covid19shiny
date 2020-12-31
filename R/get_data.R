
library(tidyverse)


global_cases <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

global_deaths <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") 
  
global_recovered <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")


# Get the last date of the data
last_date <- as.Date(global_cases %>% names() %>% last(), "%m/%d/%y")

# Sorted the country names by the highest to the lowest
global_cases_country <- 
  global_cases %>%
    rename(State =  'Province/State',
           Country = 'Country/Region') %>%
    pivot_longer(!State:Long, names_to = "Date", values_to = "Count") %>%
    mutate(Date = as.Date(Date, "%m/%d/%y"))  %>%
    filter(Date  == last_date)  %>%
    group_by(Country, Date) %>%
    summarise(Count = sum(Count), .groups = "drop") %>% 
    arrange(-Count) 

# Select the top six countries with China, Australia and NZ
country_selected <- unique(c(global_cases_country$Country[1:6], 
                             "China",  "Australia", "New Zealand"))

# Save the country_list for the dropdown menu for selecting
country_list <- c(country_selected, 
                  setdiff(global_cases_country$Country, country_selected))