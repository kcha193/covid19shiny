
library(tidyverse)


global_cases <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

global_deaths <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") 
  
global_recovered <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")



# Get the last date of the data
date_range <- 
  global_cases %>% names() %>% lubridate::mdy() %>% 
  suppressWarnings() %>% range(na.rm = TRUE)

last_date <- date_range[2]

