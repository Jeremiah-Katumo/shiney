library(tidyverse)
library(recipes)
library(dplyr)
library(h2o)
library(readr)

accident_data <- read_csv("D:/Tableau/accident data.csv")
View(accident_data)

new_df <- accident_data %>%
  mutate(`Modified Date` = `Accident Date`) %>%
  separate(`Modified Date`, into = c("Day", "Month", "Year"))

total_accidents <- new_df %>%
  select(`Accident Date`) %>%
  count()

total_casualties <- new_df %>%
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

accident_severity <- viz_data %>% 


donut_chart <- ggplot(weather_conditions, aes(x = 2, y = total_casualties, fill = Weather_category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) + # Adjust to create a doghnut shape
  theme_void() +  # Remove background, grid, and axis
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Donut Chart Example")






