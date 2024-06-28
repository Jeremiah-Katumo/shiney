library(shiny)

server <- function(input, output, session) {
  tab_list <- NULL
  showNotification("Created by Katush", duration = NULL, type = "message")
  
  base_accidents <- reactive({
    response <- accident_data %>%
      mutate(`Modified Date` = `Accident Date`) %>%
      separate(`Modified Date`, into = c("Day", "Month", "Year")) %>%
      mutate(Weather_category = case_when(
        Weather_Conditions %in% c("Fine + high winds", "Fine no high winds") ~ "Fine",
        Weather_Conditions == "Fog or mist" ~ "Fog",
        Weather_Conditions %in% c("Raining + high winds", "Raining no high winds") ~ "Raining",
        str_detect(Weather_Conditions, "^Snowing") ~ "Snowing",
        TRUE ~ "Others"
      )) %>%
      mutate(`Modified Vehicle_Type` = case_when(
        grepl("^Motorcycle", Vehicle_Type) ~ "Motorcycle",
        grepl("^Goods", Vehicle_Type) ~ "Transit",
        grepl("^(Car|Taxi|Van)", Vehicle_Type) ~ "Cars",
        grepl("^Agricultural", Vehicle_Type) ~ "Agricultural",
        grepl("^(Bus|Minibus)", Vehicle_Type) ~ "Bus",
        grepl("^(Other|Pedal|Ridden)", Vehicle_Type) ~ "Others",
        TRUE ~ "Others"  # Catch-all for any values that don't match the patterns
      ))
  })
  
  base_filters <- reactive({
    data <- base_accidents()
    
    # Filter by vehicle type
    if (input$vehicle_type != "Motorcycle") {
      data <- data[data$`Modified Vehicle_Type` == input$vehicle_type, ]
    }
    
    # Filter by weather condition
    if (input$weather_condition != "Fine") {
      data <- data[data$`Weather_category` == input$weather_condition, ]
    }
    
    if (input$accident_severity != "Fatal") {
      data <- data[data$Accident_Severity == input$weather_condition, ]
    }
    
    # Filter by date range
    # data <- data[data$Date >= input$date_range[1] & data$Date <= input$date_range[2], ]
    
    # Filter by year
    if (input$current_year != "All Years") {
      data <- data[data$Year == input$current_year, ]
    } else {
      data <- data[data$Year %in% c(2019, 2020, 2021, 2022), ]
    }
    
    if (input$previous_year != "All Years") {
      data <- data[data$Year == input$previous_year, ]
    } else {
      data <- data[data$Year %in% c(2019, 2020, 2021, 2022), ]
    }
    
    data
  })
  
  
  output$total_accidents <- renderValueBox({
    base_filters() %>%
      count() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = "orange", subtitle = "Total Accidents")
  })
  
  # output$total_casualties <- renderValueBox({
  #   base_filters() %>%
  #     pull(Number_of_Casualties) %>%
  #     sum() %>%
  #     as.integer() %>%
  #     prettyNum(big.mark = ",") %>%
  #     valueBox(icon = icon("chart-bar"), color = "navy", subtitle = "Total Casualties")
  # })
  # Calculate and display total casualties with percentage change
  output$total_casualties <- renderValueBox({
    # Calculate total casualties for the selected year
    total_current_year <- base_filters() %>%
      filter(Year == input$current_year) %>%
      summarise(total_casualties = sum(Number_of_Casualties)) %>%
      pull(total_casualties)

    # Calculate total casualties for the previous year
    if (input$previous_year != "All Years") {
      total_previous_year <- base_filters() %>%
        filter(Year == as.integer(input$previous_year)) %>%
        summarise(total_casualties = sum(Number_of_Casualties)) %>%
        pull(total_casualties)

      # Calculate percentage change
      if (total_previous_year != 0) {
        percentage_change <- ((total_current_year - total_previous_year) / total_previous_year) * 100
      } else {
        percentage_change <- NA
      }

      # Format percentage change
      if (!is.na(percentage_change)) {
        percentage_change <- paste0(round(percentage_change, 1), "%")
      } else {
        percentage_change <- NA
      }
    } else {
      percentage_change <- NA
    }

    # Format total casualties
    total_current_year <- prettyNum(total_current_year, big.mark = ",")

    valueBox(
      value = total_current_year,
      subtitle = "Total Casualties",
      footer = percentage_change,
      icon = icon("chart-bar"),
      color = "navy"
    )
  })
  
  
  output$fatal_casualties <- renderValueBox({
    base_accidents() %>%
      filter(Accident_Severity == "Fatal") %>%
      select(Number_of_Casualties) %>%
      sum() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = "red", subtitle = "Fatal Casualties")
  })
  
  output$serious_casualties <- renderValueBox({
    base_accidents() %>%
      filter(Accident_Severity == "Serious") %>%
      select(Number_of_Casualties) %>%
      sum() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = "lime", subtitle = "Serious Casualties")
  })
  
  output$slight_casualties <- renderValueBox({
    base_accidents() %>%
      filter(Accident_Severity == "Slight") %>%
      select(Number_of_Casualties) %>%
      sum() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = "yellow", subtitle = "Slight Casualties")
  })
}


