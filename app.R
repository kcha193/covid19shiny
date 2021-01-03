

library(shiny)
library(shinydashboard)
library(highcharter)

# User interface
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "COVID-19"),
  dashboardSidebar(
    h4("Johns Hopkins CSSE: Last updated ",  
       format(last_date, "%d %B %Y")),
    sliderInput("date_range",
                "Choose date ranges:",
                min = date_range[1],
                max = last_date,
                value = c(last_date - months(6),last_date),
                timeFormat="%Y-%m-%d", animate = FALSE),
    h5("Choose different type of data for display"),
    selectInput("type", "Type", 
                choices =c("Active", "Confirmed", "Deaths", "Recovered"),
                selected  =  "Active"),
    selectizeInput("country", "Choose countries (up to 9)", 
                   choices = country_list,
                   selected  =  country_selected,
                   multiple = TRUE,
                   options = list(maxItems = 9)),
    checkboxInput("log", "Logarithmic scale"),
    h4("Note:"),
    p("The global data came from Johns Hopkins CSSE in",
      a("here", href = "https://github.com/CSSEGISandData/COVID-19"), "."),
    
    box(
      h4("Contact:"),
      h5(a("Kevin Chang", href = "mailto:kevin.ct.chang@gmail.com")),
      p("Source code can be founded in ",
        a("here", href = "https://github.com/kcha193/covid19shiny")),
      width = 12, background = "black"
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    column(width = 12,
           tabBox(
             tabPanel("Daily data from Johns Hopkins CSSE",
                      value_box_UI("global"),
                      value_box_UI("us", country = "USA"),
                      value_box_UI("uk", country = "UK"),
                      value_box_UI("nz", country = "NZ")
             ),
             tabPanel("Treemap Chart (Latest Total)", 
                      highchartOutput("treemap", height = "680px")
             ),
             tabPanel("Line plot (Daily Total)",
                      highchartOutput("line_plot", height = "680px")
             ),
             tabPanel("Barcharts (Daily increment)",
                      htmlOutput("bar_plot",  height = "680px")
             ),
             width = 12, height = 700
           )
    )
  )
)




# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Value boxes -------------------------------------------------------------
  
  value_box_server("global")
  
  value_box_server("us", country = "US")
  
  value_box_server("uk", country = "United Kingdom")
  
  value_box_server("nz", country = "New Zealand")
  
  
# Treemap Chart for different countries -----------------------------------------------------------

  output$treemap <- 
    renderHighchart({
     
      daily_cases_total %>% 
        filter(Type == input$type)  %>% 
        hchart(type = "treemap",
               hcaes(value = Count_lastest, x = Country,
                     color = Count_lastest)) 
    }) 

  
    # Data for the overall plots ----------------------------------------------
  
  global_data_final <-
    reactive({
      
      global_data <-
        if (input$type == "Confirmed") {
          global_cases
        } else if (input$type == "Deaths") {
          global_deaths
        } else if (input$type == "Recovered") {
          global_recovered
        } else {
          bind_rows(Confirmed = global_cases, 
                    Deaths = global_deaths, 
                    Recovered = global_recovered,
                    .id = "Type")
        }
      
      if(input$type == "Active"){
        global_data_final <-
          global_data %>%
          rename(State =  'Province/State',
                 Country = 'Country/Region') %>%
          gather("Date",  "Count", -Type, -State,-Country,-Lat,-Long) %>%
          mutate(Date = as.Date(Date, "%m/%d/%y"))  %>%
          filter(Date <= input$date_range[2] ,
                 Date >= input$date_range[1])  %>%
          group_by(Type, Country, Date) %>%
          summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
          filter(Country %in% input$country)  %>% 
          spread(key = "Type", value = "Count") %>% 
          mutate(Count =  Confirmed - Deaths - Recovered) %>%
          select(Country, Date , Count)
        
      } else {
        global_data_final <-
          global_data %>%
          rename(State =  'Province/State',
                 Country = 'Country/Region') %>%
          gather("Date",  "Count",-State,-Country,-Lat,-Long) %>%
          mutate(Date = as.Date(Date, "%m/%d/%y"))  %>%
          filter(Date <= input$date_range[2] ,
                 Date >= input$date_range[1])  %>%
          group_by(Country, Date) %>%
          summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
          filter(Country %in% input$country)  
      }
      
     
      
      return(global_data_final)
    })
# Line plot showing the time series for different countries ----------------------------------------
  
  output$line_plot <- 
    renderHighchart({
      h <- 
        hchart(global_data_final() ,
             "line",
             hcaes(x = Date, y = Count, group = Country)) 
    
      if(input$log){
        
        h <- 
          h %>% 
          hc_yAxis(type = 'logarithmic',
                   title = list(text = "Total case counts in logarithmic scale"))
      }
      
      return(h)
    })
  

# Barchart for different countries ----------------------------------------

  output$bar_plot <- 
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
      
       
      if(input$log){
        
        h <- 
          map(input$country, function(x) {
            global_data_final %>%
              filter(Country == x) %>%
              hchart("column", hcaes(x = Date, y = Count_daily), showInLegend = FALSE) %>%
              hc_add_theme(hc_theme_smpl()) %>%
              hc_title(text = x) %>% 
              hc_yAxis(type = 'logarithmic') 
          }) %>%
          hw_grid(rowheight = 200, ncol = 3) 
        
      }
      
      return(h)
    })
}


shinyApp(ui = ui, server = server)

