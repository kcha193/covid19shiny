



value_box_UI <- function(id, country = "Global") {
  ns <- NS(id)
  
  box(
    title = country,
    valueBoxOutput(ns("N_Confirmed"), 3),
    valueBoxOutput(ns("N_Death"), 3),
    valueBoxOutput(ns("N_Recovered"), 3),
    valueBoxOutput(ns("N_Active"), 3),
    width = 12
  )
}

value_box_UI <- function(id, country = "Global") {
  ns <- NS(id)
  
  box(
    title = country,
    valueBoxOutput(ns("N_Confirmed"), 3),
    valueBoxOutput(ns("N_Death"), 3),
    valueBoxOutput(ns("N_Recovered"), 3),
    valueBoxOutput(ns("N_Active"), 3),
    width = 12
  )
}



value_box_server <- function(id, country = "Global") {
  
  moduleServer(id, function(input, output, session) {
    
    if(country != "Global"){
      
      daily_cases_total <-
        daily_cases_total %>% 
        filter(Country == country)
        
       textbox <- paste0("in ", country)
    } else {
      
      daily_cases_total <-
        daily_cases_total %>% 
        group_by(Type) %>% 
        summarise(Count_lastest = sum(Count_lastest, na.rm = TRUE),
                  Count_change = sum(Count_change, na.rm = TRUE),
                  .groups = "drop")
      
      
      textbox <- "globally"
    }
    
    
    output$N_Confirmed <- renderValueBox({
  
      
      latest <-
        daily_cases_total %>% 
        filter(Type == "Confirmed") %>%
        pull(Count_lastest)

      change <-
        daily_cases_total %>% 
        filter(Type == "Confirmed") %>%
        pull(Count_change)
      
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
        daily_cases_total %>% 
        filter(Type == "Active") %>%
        pull(Count_lastest)

      change <-
        daily_cases_total %>% 
        filter(Type == "Active") %>%
        pull(Count_change)
      
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
        daily_cases_total %>% 
        filter(Type == "Deaths") %>%
        pull(Count_lastest)

      change <-
        daily_cases_total %>% 
        filter(Type == "Deaths") %>%
        pull(Count_change)
      
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
        daily_cases_total %>% 
        filter(Type == "Recovered") %>%
        pull(Count_lastest)

      change <-
        daily_cases_total %>% 
        filter(Type == "Recovered") %>%
        pull(Count_change)

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