library(shiny)
source("variables.R", local = TRUE)

ui <- dashboardPage(
  title = "www.silas.com",
                    
  dashboardHeader(title = "Road Accidents Dashboard", titleWidth = 200),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput(
        inputId = "year",
        label = "Year:",
        choices = year_list,
        selected = 99,
        size = 5,
        selectize = FALSE
      ),
      selectInput(
        inputId = "accident severity",
        label = "Accident Severity:",
        choices = severity_list,
        selected = "Fatal",
        size = 3,
        selectize = FALSE
      ),
      selectInput(
        inputId = "weather condition",
        label = "Weather Condition:",
        choices = weather_list,
        selected = "Fine",
        size = 5,
        selectize = FALSE
      ),
      selectInput(
        inputId = "vehicles",
        label = "Vehicle:",
        choices = vehicle_list,
        selected = "Motorcycle",
        size = 6,
        selectize = FALSE
      ),
      actionLink("remove", "Remove details tabs"),
      menuItem("Source Code", icon = icon("github"), href = "https://github.com/Silas-Ochieng"),
      menuItem("About Me", icon = icon("linkedin"), href = "https://www.linkedin.com/in/silas-ochieng"),
      menuItem("Video", icon = icon("youtube"), href = "")
    )
  ),
  
  
  dashboardBody(
    tabsetPanel(id = "tabs",
                tabPanel(
                  title = "Main Dashboard",
                  value = "page1",
                  fluidRow(
                    valueBoxOutput("total_accidents"),
                    valueBoxOutput("total_casualties")
                  ),
                  fluidRow(
                    valueBoxOutput("fatal_casualties"),
                    valueBoxOutput("serious_casualties"),
                    valueBoxOutput("slight_casualties")
                  )
                )
    )
  )


)