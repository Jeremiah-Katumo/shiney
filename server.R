
server <- function(input, output, session) {
  showNotification("Created by Katush", duration = NULL, type = "message")
  
  base_accidents <- reactive({
    accident_data_prep
  })
  
  base_filters <- reactive({
    data <- base_accidents()
    
    # Filter by vehicle type
    if (input$vehicle_type != "Motorcycle") {
      data <- data[data$`Modified Vehicle_Type` == input$vehicle_type, ]
    }
    
    # Filter by weather condition
    if (input$weather_condition != "Fine") {
      data <- data[data$Weather_category == input$weather_condition, ]
    }
    
    if (input$accident_severity != "Fatal") {
      data <- data[data$Accident_Severity == input$accident_severity, ]
    }
    
    # Calculate metrics for the current year and the previous year
    current_year_data <- data[data$Year == input$current_year, ]
    previous_year_data <- data[data$Year == input$previous_year, ]
    
    current_fatal <- current_year_data %>% filter(Accident_Severity == "Fatal")
    current_serious <- current_year_data %>% filter(Accident_Severity == "Serious")
    current_slight <- current_year_data %>% filter(Accident_Severity == "Slight")
    previous_fatal <- previous_year_data %>% filter(Accident_Severity == "Fatal")
    previous_serious <- previous_year_data %>% filter(Accident_Severity == "Serious")
    previous_slight <- previous_year_data %>% filter(Accident_Severity == "Slight")
    
    list(
      data,
      current_year = current_year_data,
      previous_year = previous_year_data,
      change = nrow(current_year_data) - nrow(previous_year_data),
      casualties_change = sum(current_year_data$Number_of_Casualties) - sum(previous_year_data$Number_of_Casualties),
      fatal_change = sum(current_fatal$Number_of_Casualties) - sum(previous_fatal$Number_of_Casualties),
      serious_change = sum(current_serious$Number_of_Casualties) - sum(previous_serious$Number_of_Casualties),
      slight_change = sum(current_slight$Number_of_Casualties) - sum(previous_slight$Number_of_Casualties)
    )
  })
  
  
  output$total_accidents <- renderValueBox({
    change <- base_filters()$change
    print(paste("Total accidents change:", change))  # Debugging
    change %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = ifelse(change >= 0, "green", "red"), subtitle = "Change in Total Accidents")
  })
  
  output$total_casualties <- renderValueBox({
    casualties_change <- base_filters()$casualties_change
    print(paste("Total casualties change:", casualties_change))  # Debugging
    casualties_change %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = ifelse(casualties_change >= 0, "green", "red"), subtitle = "Change in Total Casualties")
  })
  
  output$percentages <- renderValueBox({
    total_current_year <- sum(base_filters()$current_year$Number_of_Casualties)
    total_previous_year <- sum(base_filters()$previous_year$Number_of_Casualties)
    
    percentage_change <- if (total_previous_year != 0) {
      ((total_current_year - total_previous_year) / total_previous_year) * 100
    } else {
      NA
    }
    
    percentage_change <- if (!is.na(percentage_change)) {
      paste0(round(percentage_change, 1), "%")
    } else {
      NA
    }
    
    print(paste("Percentage change:", percentage_change))  # Debugging
    percentage_change <- prettyNum(percentage_change)
    valueBox(
      value = percentage_change,
      subtitle = "Percentage Change",
      icon = icon("bar-chart"),
      color = ifelse(percentage_change >= 0, "green", "red")
    )
  })
  
  output$fatal_casualties <- renderValueBox({
    fatal_change <- base_filters()$fatal_change
    print(paste("Fatal casualties change:", fatal_change))  # Debugging
    fatal_change %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = ifelse(fatal_change >= 0, "green", "red"), subtitle = "Change in Fatal Casualties")
  })
  output$serious_casualties <- renderValueBox({
    serious_change <- base_filters()$serious_change
    print(paste("Serious casualties change:", serious_change))  # Debugging
    serious_change %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = ifelse(serious_change >= 0, "green", "red"), subtitle = "Change in Serious Casualties")
  })
  output$slight_casualties <- renderValueBox({
    slight_change <- base_filters()$slight_change
    print(paste("Slight casualties change:", slight_change))  # Debugging
    slight_change %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = ifelse(slight_change >= 0, "green", "red"), subtitle = "Change in Slight Casualties")
  })
  
  #### PLOTS ####
  base_plots_data <- reactive({
    data <- base_accidents()
    
    # Filter by vehicle type
    if (input$vehicle_type != "Motorcycle") {
      data <- data[data$`Modified Vehicle_Type` == input$vehicle_type, ]
    }
    
    # Filter by weather condition
    if (input$weather_condition != "Fine") {
      data <- data[data$Weather_category == input$weather_condition, ]
    }
    
    if (input$accident_severity != "Fatal") {
      data <- data[data$Accident_Severity == input$accident_severity, ]
    }
    
    plotsdata <- plots_data_function(
      data = data,
      severeness=input$accident_severity
    )
    
    plotsdata
  })
  
  output$donutChart <- renderPlotly({
    # Create the donut chart
    interactive_donut_chart <- base_plots_data() %>%
      plot_ly(
        labels = ~Weather_category, 
        values = ~total_casualties, 
        type = 'pie',
        textinfo = 'label+percent',
        insidetextorientation = 'radial',
        hole = 0.5
      ) %>%
      layout(
        title = 'Fatal Casualties caused by Weather Conditions',
        showlegend = TRUE,
        legend = list(orientation = 'h'),
        plot_bgcolor = '#f8f9fa',   # Plot background color
        paper_bgcolor = '#ffffff'   # Paper background color
      )
    
    interactive_donut_chart
  })
}
