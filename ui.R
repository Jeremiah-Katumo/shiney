library(shiny)
source("variables.R", local = TRUE)

ui <- dashboardPage
  title = "www.katumo.com",
                    
  dashboardHeader(title = "Road Accidents Dashboard", titleWidth = 200),
  
  dashboardSidebar(
    selectInput(
      inputId = "year",
      label = "Year:",
      
    )
  )
                    
                    
                    
                    
                    
)