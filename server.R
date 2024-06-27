library(shiny)

server <- function(input, output, session) {
  tab_list <- NULL
  showNotification("Created by Silas", duration = NULL, type = "message")
  
  base_accidents <- reactive({
    response <- accident_data %>%
      mutate(`Modified Date` = `Accident Date`) %>%
      separate(`Modified Date`, into = c("Day", "Month", "Year")) %>%
      mutate(Weather_category = case_when(
        Weather_Conditions %in% c("Fine + high winds", "Fine no high winds") ~ "Fine",
        Weather_Conditions == "Fog or mist" ~ "Fog",
        Weather_Conditions %in% c("Raining + high winds", "Raining no high winds") ~ "Raining",
        str_detect(Weather_Conditions, "^Snowing") ~ "Snowing",
        FALSE ~ as.character(Weather_Conditions)
      ) ) %>%
      mutate(`Modified Date` = `Accident Date`) %>%
      separate(`Modified Date`, into = c("Day", "Month", "Year"))
  })
  
  
  output$total_accidents <- renderValueBox({
    base_accidents() %>%
      select(`Accident Date`) %>%
      count() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = "orange", subtitle = "Total Accidents")
  })
  
  output$total_casualties <- renderValueBox({
    base_accidents() %>%
      select(Number_of_Casualties) %>%
      sum() %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("chart-bar"), color = "navy", subtitle = "Total Casualties")
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