

value_box_UI <- function(id, country = "Global") {
  ns <- NS(id)
  
  box(
    title = country,
    valueBoxOutput(ns("N_Total"), 3),
    valueBoxOutput(ns("N_Death"), 3),
    valueBoxOutput(ns("N_Recovered"), 3),
    valueBoxOutput(ns("N_Active"), 3),
    width = 12
  )
}



value_box_server <- function(id, country = "Global") {
  
  moduleServer(id, function(input, output, session) {
    
    if(country != "Global"){
      
      global_cases <- 
        global_cases %>% 
        filter(`Country/Region` == country)
      
      global_recovered <- 
        global_recovered %>% 
        filter(`Country/Region` == country)
      
      global_deaths <- 
        global_deaths %>% 
        filter(`Country/Region` == country)
      
       textbox <- paste0("in ", country)
    } else {
      
      textbox <- "globally"
    }
    
    
    output$N_Total <- renderValueBox({
  
      latest <-
        global_cases %>%
        rename(Count = last_col()) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count)
      
      yesterday <-
        global_cases %>%
        rename(Count = last_col(1)) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count)
      
      change <- latest - yesterday
      
      valueBox(
        scales::comma(latest), 
        subtitle =
          paste0(" (", 
                 ifelse(change >= 0, "+", "-"), 
                 scales::comma(abs(change)), ") cases ", textbox),
        
        color = "red"
      )
      
    })
    
    output$N_Active <- renderValueBox({
      
      latest <-
        global_cases %>%
        rename(Count = last_col()) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count) -
        global_recovered %>%
        rename(Count = last_col()) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count)
      
      yesterday <-
        global_cases %>%
        rename(Count = last_col(1)) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count) -
        global_recovered %>%
        rename(Count = last_col(1)) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count)
      
      change <- latest - yesterday
      
      valueBox(
        scales::comma(latest), 
        subtitle =
          paste0(" (", 
                 ifelse(change >= 0, "+", "-"), 
                 scales::comma(abs(change)), ") active cases ", textbox),
        
        color = "yellow"
      )
      
    })
    
    output$N_Death <- renderValueBox({
      
      latest <-
        global_deaths %>%
        rename(Count = last_col()) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count)
      
      yesterday <-
        global_deaths %>%
        rename(Count = last_col(1)) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count)
      
      change <- latest - yesterday
      
      valueBox(
        scales::comma(latest), 
        subtitle =
          paste0(" (", 
                 ifelse(change >= 0, "+", "-"), 
                 scales::comma(abs(change)), ") deaths ", textbox),
        
        color = "blue"
      ) })
    
    output$N_Recovered <- renderValueBox({
      
      latest <-
        global_recovered %>%
        rename(Count = last_col()) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count)
      
      yesterday <-
        global_recovered %>%
        rename(Count = last_col(1)) %>%
        summarise(Count = sum(Count)) %>%
        pull(Count)
      
      change <- latest - yesterday
      
      valueBox(
        scales::comma(latest), 
        subtitle =
          paste0(" (", 
                 ifelse(change >= 0, "+", "-"), 
                 scales::comma(abs(change)), ") recovered ", textbox),
        
        color = "green"
      )
      
    })
    
  })
  
}