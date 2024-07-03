library(tidyverse)
library(recipes)
library(dplyr)
library(h2o)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)                   # interface to the JavaScript library DataTables
library(r2d3)
library(purrr)                # for function programming tools
library(rlang)



accident_data <- read_csv("D:/Tableau/accident data.csv", show_col_types = FALSE)
View(accident_data)

accident_data_prep <- accident_data %>%
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


plots_data <- accident_data_prep %>%
  filter(Accident_Severity == "Fatal") %>%
  group_by(Weather_category) %>%
  summarise(
    count = n(),
    total_casualties = sum(Number_of_Casualties)
  ) %>%
  mutate(
    percentage = (total_casualties / sum(total_casualties)) * 100,
    ypos = cumsum(percentage) - 0.5 * percentage
  ) %>%
  as.data.frame()
# total_casualties <- as.data.frame(plots_data$total_casualties) 
# total_casualties <- as.data.frame(plots_data[["total_casualties"]])
total_casualties <- plots_data %>% select(total_casualties) %>% as.data.frame()
# plots_data[, 3]
plotsdata <- accident_data_prep %>%
  filter(Accident_Severity == "Fatal") %>%
  group_by(Weather_category) %>%
  summarise(
    count = n(),
  ) %>%
  cbind(total_casualties) %>%
  mutate(
    percentage = (total_casualties / sum(total_casualties)) * 100,
    ypos = cumsum(percentage) - 0.5 * percentage
  ) %>%
  as.data.frame()
plots_data_function <- function(data, severeness) {
  plotsdata <- data %>%
    filter(Accident_Severity == severeness) %>%
    group_by(Weather_category) %>%
    summarise(
      count = n(),
    ) %>%
    cbind(total_casualties) %>%
    mutate(
      percentage = (total_casualties / sum(total_casualties)) * 100,
      ypos = cumsum(percentage) - 0.5 * percentage
    ) %>%
    as.data.frame()
  
  return(plotsdata)
}
plots_data_function(data=accident_data_prep,severeness="Fatal")

################################################################################

new_df <- accident_data %>%
  mutate(`Modified Date` = `Accident Date`) %>%
  separate(`Modified Date`, into = c("Day", "Month", "Year"))

## Date variables lists
year_list <- as.list(c(2019, 2020, 2021, 2022)) %>%
  set_names(c("2019", "2020", "2021", "2022"))
year_list$`All Years` <- 99

month_list <- as.list(1:12) %>%
  set_names(month.name)
month_list$`All Year` <- 99

## Vehicle list
vehicle <- new_df %>%
  mutate(`Modified Vehicle_Type` = case_when(
    grepl("^Motorcycle", Vehicle_Type) ~ "Motorcycle",
    grepl("^Goods", Vehicle_Type) ~ "Transit",
    grepl("^(Car|Taxi|Van)", Vehicle_Type) ~ "Cars",
    grepl("^Agricultural", Vehicle_Type) ~ "Agricultural",
    grepl("^(Bus|Minibus)", Vehicle_Type) ~ "Bus",
    grepl("^(Other|Pedal|Ridden)", Vehicle_Type) ~ "Others",
    TRUE ~ "Others"  # Catch-all for any values that don't match the patterns
  )) %>%
  group_by(`Modified Vehicle_Type`) %>%
  summarize(n = n(), .groups = 'drop')
vehicle_list <- setNames(as.list(vehicle$`Modified Vehicle_Type`), vehicle$`Modified Vehicle_Type`)

## Weather list
weather <- new_df %>%
  mutate(Weather_category = case_when(
    Weather_Conditions %in% c("Fine + high winds", "Fine no high winds") ~ "Fine",
    Weather_Conditions == "Fog or mist" ~ "Fog",
    Weather_Conditions %in% c("Raining + high winds", "Raining no high winds") ~ "Raining",
    str_detect(Weather_Conditions, "^Snowing") ~ "Snowing",
    TRUE ~ "Others"
  ) ) %>%
  group_by(Weather_category) %>%
  summarize(n = n(), .groups = 'drop')
weather_list <- setNames(as.list(weather$Weather_category), weather$Weather_category)

# Accident severity list
severity <- new_df %>%
  group_by(Accident_Severity) %>%
  summarise(n = n(), .groups = 'drop')
severity_list <- setNames(as.list(severity$Accident_Severity), severity$Accident_Severity)



total_accidents <- new_df %>%
  select(`Accident Date`) %>%
  count()

total_casualties <- new_df %>%
  select(Number_of_Casualties) %>%
  sum()

fatal_casualties <- new_df %>%
  filter(Accident_Severity == "Fatal") %>%
  select(Number_of_Casualties) %>%
  sum()

seriouse_casualties <- new_df %>%
  filter(Accident_Severity == "Serious") %>%
  select(Number_of_Casualties) %>%
  sum()

slight_casualties <- new_df %>%
  filter(Accident_Severity == "Slight") %>%
  select(Number_of_Casualties) %>%
  sum()

Year_2019 <- new_df %>%
  group_by(Year) %>%
  filter(Year == 2019) %>%
  summarise(n = n()) %>%
  select(n)

Year_2020 <- new_df %>%
  group_by(Year) %>%
  filter(Year == 2020) %>%
  summarise(n = n()) %>%
  select(n)

Year_2021 <- new_df %>%
  group_by(Year) %>%
  filter(Year == 2021) %>%
  summarise(n = n()) %>%
  select(n)

Year_2022 <- new_df %>%
  group_by(Year) %>%
  filter(Year == 2022) %>%
  summarise(n = n()) %>%
  select(n)

# pattern_motor <- "^Motorcyle"
# motorcycle <- new_df %>% 
#   filter(str_detect(Vehicle_Type, pattern = pattern_motor)) %>%
#   count()

motorcycle_counts <- grep("^Motorcycle", new_df$Vehicle_Type, value = TRUE) %>% 
  table() %>% as.data.frame() %>% select(Freq) %>% sum()

transit_counts <- grep("^Goods", new_df$Vehicle_Type, value = TRUE) %>%
  table() %>% as.data.frame() %>% select(Freq) %>% sum()

cars_counts <- grep("^(Car|Taxi|Van)", new_df$Vehicle_Type, value = TRUE) %>%
  table() %>% as.data.frame() %>% select(Freq) %>% sum()

agricultural_counts <- grep("^Agricultural", new_df$Vehicle_Type, value = TRUE) %>%
  table() %>% as.data.frame() %>% select(Freq) %>% sum()

bus_counts <- grep("^(Bus|Minibus)", new_df$Vehicle_Type, value = TRUE) %>%
  table() %>% as.data.frame() %>% select(Freq) %>% sum()

other_vehicle_counts <- grep("^(Other|Pedal|Ridden)", new_df$Vehicle_Type, value = TRUE) %>%
  table() %>% as.data.frame() %>% select(Freq) %>% sum()


### VISUALIZATIONS
viz_data <- new_df %>%
  mutate(Weather_category = case_when(
    Weather_Conditions %in% c("Fine + high winds", "Fine no high winds") ~ "Fine",
    Weather_Conditions == "Fog or mist" ~ "Fog",
    Weather_Conditions %in% c("Raining + high winds", "Raining no high winds") ~ "Raining",
    str_detect(Weather_Conditions, "^Snowing") ~ "Snowing",
    FALSE ~ as.character(Weather_Conditions)
  ) )


weather_conditions <- viz_data %>% select(Weather_category, Number_of_Casualties, Year) %>%
  group_by(Weather_category) %>%
  summarise(total_casualties = sum(Number_of_Casualties)) %>%
  mutate(percentage = total_casualties / sum(total_casualties) * 100)

accident_severity <- viz_data %>% select(Accident_Severity, Number_of_Casualties) %>%
  group_by(Accident_Severity) %>%
  summarise(total_accidents_severity = sum(Number_of_Casualties)) %>%
  mutate(percentage = total_accidents_severity / sum(total_accidents_severity) * 100)


donut_chart <- ggplot(weather_conditions, aes(x = 2, y = total_casualties, fill = Weather_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) + # Adjust to create a doghnut shape
  theme_void() +  # Remove background, grid, and axis
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Donut Chart Example")




# # Calculate and display total casualties with percentage change
# output$total_casualties <- renderValueBox({
#   data <- base_accidents()
#   
#   # Calculate total casualties for the selected year
#   total_current_year <- data %>%
#     filter(Year == input$current_year) %>%
#     summarise(total_casualties = sum(Number_of_Casualties)) %>%
#     pull(total_casualties)
#   
#   # Calculate total casualties for the previous year
#   if (input$previous_year != "All Years") {
#     previous_year <- input$previous_year
#     total_previous_year <- data %>%
#       filter(Year == previous_year) %>%
#       summarise(total_casualties = sum(Number_of_Casualties)) %>%
#       pull(total_casualties)
#     
#     # Calculate percentage change
#     if (total_previous_year != 0) {
#       percentage_change <- ((total_current_year - total_previous_year) / total_previous_year) * 100
#     } else {
#       percentage_change <- NA
#     }
#     
#     # Format percentage change
#     if (!is.na(percentage_change)) {
#       percentage_change <- paste0(round(percentage_change, 1), "%")
#     } else {
#       percentage_change <- "N/A"
#     }
#   } else {
#     percentage_change <- "N/A"
#   }
#   
#   # Format total casualties
#   total_current_year <- prettyNum(total_current_year, big.mark = ",")
#   
#   valueBox(
#     value = total_current_year, 
#     subtitle = "Total Casualties", 
#     footer = percentage_change,
#     icon = icon("chart-bar"), 
#     color = "navy"
#   )
# })


