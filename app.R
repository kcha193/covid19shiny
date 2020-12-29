

library(shiny)
library(shinydashboard)
library(highcharter)
library(purrr)


last_date <- as.Date(global_cases %>% 
                        names() %>% 
                        last(), "%m/%d/%y")

global_cases %>%
        rename(State =  'Province/State',
               Country = 'Country/Region') %>%
        gather("Date",  "Count",-State,-Country,-Lat,-Long) %>%
        mutate(Date = as.Date(Date, "%m/%d/%y"))  %>%
        filter(Date  == last_date)  %>%
        group_by(Country, Date) %>%
        summarise(Count = sum(Count)) %>% 
  arrange(-Count) %>% pull(Country ) -> country_list

country_selected <- unique(c(country_list[1:6], "China",  "Australia",
                         "New Zealand"))

country_list <- c(country_selected, country_list[country_list!=country_selected])


ui <- dashboardPage( skin = "black",
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    h4("Johns Hopkins CSSE: Last updated ",  
       format(last_date, "%d %B %Y")),
    sliderInput("date_range",
                "Dates:",
                min = as.Date("2020-01-22"),
                max = last_date,
                value = c(last_date - 30,last_date),
                timeFormat="%Y-%m-%d", animate = FALSE),
    h5("Line plots of global data:"),
    selectInput("type", "Type", 
                choices =c("Total", "Death", "Recovered"),
                selected  =  "Total"),
    selectizeInput("country", "Country (up to 9)", 
                choices = country_list,
                selected  =  country_selected,
                multiple = TRUE,
            options = list(maxItems = 9)),
    
   
    h4("Note:"),
    p(
      "The global data came from Johns Hopkins CSSE in",
      a("here",
        href = "https://github.com/CSSEGISandData/COVID-19"),
      "."
    ),
    
    box(
      h4("Contact:"),
      h5(a("Kevin Chang", href = "mailto:kevin.ct.chang@gmail.com")),
      p(        "Source code can be founded in ",
        a("here", 
          href = "https://github.com/kcha193/covid19nz")
      ),
      width = 12,
      background = "black"
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    column(
      width = 12,
    tabBox(
        tabPanel("Daily data from Johns Hopkins CSSE",
      box(
        title = "Global",
        valueBoxOutput("global_Total", 3),
        valueBoxOutput("global_Death", 3),
        valueBoxOutput("global_Recovered", 3),
        valueBoxOutput("global_Active", 3),
    
        width = 12
      ),
       box(
        title = "USA",
        valueBoxOutput("US_Total", 3),
        valueBoxOutput("US_Death", 3),
        valueBoxOutput("US_Recovered", 3),
        valueBoxOutput("US_Active", 3),
        width = 12
      ),
      
       box(
        title = "UK",
        valueBoxOutput("UK_Total", 3),
        valueBoxOutput("UK_Death", 3),
        valueBoxOutput("UK_Recovered", 3),
        valueBoxOutput("UK_Active", 3),
        width = 12
      )),
      
        tabPanel(
          "Treemap",
          highchartOutput("treemap_global",  height = "680px")
        ),
        tabPanel(
          "Line plot",
          highchartOutput("line_plot_global",  height = "680px")
        ),
        tabPanel(
          "Barcharts",
          htmlOutput("bar_plot_global",  height = "680px")
        ),
        width = 12,
        height = 700
      )
    )
    )
)




server <- function(input, output, session) {
  
  output$global_Total <- renderValueBox({
   
    latest <- 
    global_cases %>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_cases%>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") cases globally"),
      
      color = "red"
    )
 
  })
  
  output$global_Active <- renderValueBox({
    
    latest <- 
      global_cases%>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count) - 
      global_recovered%>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_cases%>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count) - 
      global_recovered%>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") active cases globally"),
      
      color = "yellow"
    )
    
  })
  
  output$global_Death <- renderValueBox({
    
   
    
    latest <- 
      global_deaths%>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_deaths%>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") deaths globally"),
      
      color = "blue"
    ) })
  
  output$global_Recovered <- renderValueBox({
    
    latest <- 
      global_recovered%>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_recovered%>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") recovered globally"),
      
      color = "green"
    )
    
  })
  
  output$US_Total <- renderValueBox({
   
    latest <- 
    global_cases %>% 
      filter(`Country/Region` == "US") %>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_cases %>% 
      filter(`Country/Region` == "US") %>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") cases globally"),
      
      color = "red"
    )
 
  })
  
  output$US_Active <- renderValueBox({
    
    latest <- 
      global_cases%>% 
      filter(`Country/Region` == "US") %>%  
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count) - 
      global_recovered%>% 
      filter(`Country/Region` == "US") %>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_cases%>% 
      filter(`Country/Region` == "US") %>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count) - 
      global_recovered%>% 
      filter(`Country/Region` == "US") %>%  
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") active cases globally"),
      
      color = "yellow"
    )
    
  })
  
  output$US_Death <- renderValueBox({
    
    latest <- 
      global_deaths%>% 
      filter(`Country/Region` == "US") %>%  
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_deaths %>% 
      filter(`Country/Region` == "US") %>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") deaths globally"),
      
      color = "blue"
    ) })
  
  output$US_Recovered <- renderValueBox({
    
    latest <- 
      global_recovered %>% 
      filter(`Country/Region` == "US") %>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_recovered %>% 
      filter(`Country/Region` == "US") %>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") recovered globally"),
      
      color = "green"
    )
    
  })  

  output$UK_Total <- renderValueBox({
   
    latest <- 
    global_cases %>% 
      filter(`Country/Region` == "United Kingdom") %>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_cases %>% 
      filter(`Country/Region` == "United Kingdom") %>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") cases globally"),
      
      color = "red"
    )
 
  })
  
  output$UK_Active <- renderValueBox({
    
    latest <- 
      global_cases%>% 
      filter(`Country/Region` == "United Kingdom") %>%  
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count) - 
      global_recovered%>% 
      filter(`Country/Region` == "United Kingdom") %>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_cases%>% 
      filter(`Country/Region` == "United Kingdom") %>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count) - 
      global_recovered%>% 
      filter(`Country/Region` == "United Kingdom") %>%  
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") active cases globally"),
      
      color = "yellow"
    )
    
  })
  
  output$UK_Death <- renderValueBox({
    
    latest <- 
      global_deaths%>% 
      filter(`Country/Region` == "United Kingdom") %>%  
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_deaths %>% 
      filter(`Country/Region` == "United Kingdom") %>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") deaths globally"),
      
      color = "blue"
    ) })
  
  output$UK_Recovered <- renderValueBox({
    
    latest <- 
      global_recovered %>% 
      filter(`Country/Region` == "United Kingdom") %>% 
      rename(  Count = last_col()) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    yesterday <- 
      global_recovered %>% 
      filter(`Country/Region` == "United Kingdom") %>% 
      rename(  Count = last_col(1)) %>% 
      summarise( Count = sum(Count)) %>% 
      pull(Count)
    
    change <- latest - yesterday
    
    valueBox(
      scales::comma(latest), 
      subtitle =
        paste0(" (", 
               ifelse(change >= 0, "+", "-"), 
               scales::comma(abs(change)), ") recovered globally"),
      
      color = "green"
    )
    
  })  
  
  global_data_final <-
    reactive({
      global_data <-
        if (input$type == "Total") {
          global_cases
        } else if (input$type == "Death") {
          global_deaths
        } else {
          global_recovered
        }
      
      global_data_final <-
        global_data %>%
        rename(State =  'Province/State',
               Country = 'Country/Region') %>%
        gather("Date",  "Count",-State,-Country,-Lat,-Long) %>%
        mutate(Date = as.Date(Date, "%m/%d/%y"))  %>%
        filter(Date <= input$date_range[2] ,
               Date >= input$date_range[1])  %>%
        group_by(Country, Date) %>%
        summarise(Count = sum(Count)) %>%
        filter(Country %in% input$country)
      
      return(global_data_final)
    })
 
  
  
  output$treemap_global <- 
    renderHighchart({
      
       global_data <-
        if (input$type == "Total") {
          global_cases
        } else if (input$type == "Death") {
          global_deaths
        } else {
          global_recovered
        }
       
      global_data %>%
        rename(State =  'Province/State',
               Country = 'Country/Region') %>%
        gather("Date",  "Count",-State,-Country,-Lat,-Long) %>%
        mutate(Date = as.Date(Date, "%m/%d/%y"))  %>%
        filter(Date <= input$date_range[2] ,
               Date >= input$date_range[1])  %>%
        group_by(Country, Date) %>%
        summarise(Count = sum(Count)) %>%
        filter(Date == input$date_range[2])  %>% 
      hchart(type = "treemap",
             hcaes(value = Count, x = Country, color = Count)) 

      
    }) 
  
  output$line_plot_global <- 
    renderHighchart({
       
      hchart(global_data_final() ,
             "line",
             hcaes(x = Date, y = Count, group = Country))  %>%
        hc_yAxis(type = 'logarithmic',
                 title = list(text = "Total case counts in logarithmic scale"))
      
      
    })
  
  
  output$bar_plot_global <- 
    renderUI({
      
      global_data_final <- global_data_final()

      global_data_final <- 
        global_data_final %>% 
        mutate(Date = Date +1) %>%
        rename(Count_prev = Count) %>% 
        left_join(global_data_final) %>% 
        mutate(Count_daily = Count - Count_prev)
        
      
      h <- 
        map(input$country, function(x) {
        global_data_final %>%
          filter(Country == x) %>%
          hchart("column", hcaes(x = Date, y = Count_daily), showInLegend = FALSE) %>%
          hc_add_theme(hc_theme_smpl()) %>%
          hc_title(text = x) %>%
          hc_yAxis(title = list(text = ""))
      }) %>%
        hw_grid(rowheight = 200, ncol = 3) 
      
      return(h)
      
    })
}


shinyApp(ui = ui, server = server)

