library(shiny)

server <- function(input, output, session) {
  tab_list <- NULL
  showNotification("Created by Jeremy", duration = NULL, type = "message")
  
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
}