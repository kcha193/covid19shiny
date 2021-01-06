


library(tidyverse)


req <- httr::GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")
httr::stop_for_status(req)
filelist_raw <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)

filelist_us <- 
  grep("csse_covid_19_data/csse_covid_19_daily_reports_us/([0-9]+)",
     filelist_raw, value = TRUE)

filelist <- 
  grep("csse_covid_19_data/csse_covid_19_daily_reports/([0-9]+)",
     filelist_raw, value = TRUE)

dates <-
  lubridate::mdy(gsub(".csv$", "", basename(filelist)))


latest_daily_cases <-
  read_csv(
    paste0(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
      filelist[which(dates == max(dates, na.rm = TRUE))]
    )
  )


latest_daily_cases_total <-
  latest_daily_cases %>%
  select(Country_Region, Confirmed:Active) %>%
  rename(Country = Country_Region) %>%
  filter(Country != "US") %>% 
  pivot_longer(Confirmed:Active, names_to = "Type", values_to = "Count") %>%
  group_by(Country, Type) %>%
  summarise(Count_lastest = sum(Count, na.rm = TRUE), .groups = "drop")

latest_daily_cases_us <-
  read_csv(
    paste0(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
      filelist_us[which(dates == max(dates, na.rm = TRUE))]
    )
  )

latest_daily_cases_total <-
  latest_daily_cases_us %>%
  select(Country_Region, Confirmed:Active) %>%
  rename(Country = Country_Region) %>%
  pivot_longer(Confirmed:Active, names_to = "Type", values_to = "Count") %>%
  group_by(Country, Type) %>%
  summarise(Count_lastest = sum(Count, na.rm = TRUE), .groups = "drop") %>% 
  bind_rows(latest_daily_cases_total)


previous_daily_cases <-
  read_csv(
    paste0(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
      filelist[which(dates == sort(dates, decreasing = TRUE)[2])]
    )
  )

previous_daily_cases_total <-
  previous_daily_cases %>%
  select(Country_Region, Confirmed:Active) %>%
  rename(Country = Country_Region) %>%
  filter(Country != "US") %>% 
  pivot_longer(Confirmed:Active, names_to = "Type", values_to = "Count") %>%
  group_by(Country, Type) %>%
  summarise(Count_previous = sum(Count, na.rm = TRUE), .groups = "drop")


previous_daily_cases_us <-
  read_csv(
    paste0(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
      filelist_us[which(dates == sort(dates, decreasing = TRUE)[2])]
    )
  )

previous_daily_cases_total <-
  previous_daily_cases_us %>%
  select(Country_Region, Confirmed:Active) %>%
  rename(Country = Country_Region) %>%
  pivot_longer(Confirmed:Active, names_to = "Type", values_to = "Count") %>%
  group_by(Country, Type) %>%
  summarise(Count_previous = sum(Count, na.rm = TRUE), .groups = "drop")%>% 
  bind_rows(previous_daily_cases_total)


daily_cases_total <- 
  latest_daily_cases_total %>% 
  left_join(previous_daily_cases_total) %>% 
  mutate(Count_change = Count_lastest - Count_previous)



# Sorted the country names by the highest to the lowest
global_cases_country <- 
  daily_cases_total %>%
  filter(Type  == "Confirmed") %>% 
  arrange(-Count_lastest) 

# Select the top six countries with China, Australia and NZ
country_selected <- unique(c(global_cases_country$Country[1:6], 
                             "China",  "Australia", "New Zealand"))

# Save the country_list for the dropdown menu for selecting
country_list <- c(country_selected, 
                  setdiff(global_cases_country$Country, country_selected))
