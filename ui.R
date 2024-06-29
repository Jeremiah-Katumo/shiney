library(shiny)
source("variables.R", local = TRUE)
source("plots.R", local = TRUE)

ui <- dashboardPage(
  title = "www.katush.com",
                    
  dashboardHeader(title = "Road Accidents Dashboard", titleWidth = 200),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput(
        inputId = "current_year",
        label = "Current Year:",
        choices = year_list,
        selected = 99,
        size = 4,
        selectize = FALSE
      ),
      selectInput(
        inputId = "previous_year",
        label = "Previous Year:",
        choices = year_list,
        selected = 99,
        size = 4,
        selectize = FALSE
      ),
      selectInput(
        inputId = "weather_condition",
        label = "Weather Condition:",
        choices = weather_list,
        selected = "Fine",
        size = 5,
        selectize = FALSE
      ),
      selectInput(
        inputId = "vehicle_type",
        label = "Vehicle:",
        choices = vehicle_list,
        selected = "Motorcycle",
        size = 6,
        selectize = FALSE
      ),
      actionLink("remove", "Remove details tabs"),
      menuItem("Source Code", icon = icon("github"), href = "https://github.com/Jeremiah-Katumo"),
      menuItem("About Me", icon = icon("linkedin"), href = "https://www.linkedin.com/in/Jeremiah-Katumo"),
      menuItem("Video", icon = icon("youtube"), href = "")
    )
  ),
  
  
  dashboardBody(
    tabsetPanel(id = "tabs",
                tabPanel(
                  title = "Main Dashboard",
                  value = "page1",
                  fluidRow(
                    valueBoxOutput("total_accidents", width = 4),
                    valueBoxOutput("total_casualties", width = 4),
                    valueBoxOutput("percentages", width = 4)
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


