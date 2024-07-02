library(tidyverse)
library(recipes)
library(h20)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(maps)
library(chloroplethr)

accident_data <- read_csv("D:/Tableau/accident data.csv")
View(accident_data)


# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()


data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(25, 35, 20, 20)
)
# Calculate the percentage
data <- data %>%
  mutate(percentage = value / sum(value) * 100,
         ypos = cumsum(percentage) - 0.5 * percentage) # Calculate position for text
# Create the plot
donut_chart <- ggplot(data, aes(x = 2, y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) + # Adjust to create a donut shape
  theme_void() +  # Remove background, grid, and axis
  theme(legend.position = "right") +
  geom_text(aes(y = ypos, label = paste0(round(percentage, 1), "%")), color = "white") +
  labs(title = "Donut Chart Example")

print(donut_chart)

################################################################################

# Sample dataset
data <- data.frame(
  id = 1:5,
  name = c("Location1", "Location2", "Location3", "Location4", "Location5"),
  latitude = c(37.7749, 34.0522, 40.7128, 41.8781, 47.6062),
  longitude = c(-122.4194, -118.2437, -74.0060, -87.6298, -122.3321)
)
# Convert to spatial data frame
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
# Plot the map
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = data_sf, aes(color = name), size = 3) +
  theme_minimal() +
  labs(title = "Map with Sample Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Locations")
# Customize the map
ggplot(data = world) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = data_sf, aes(color = name), size = 3) +
  theme_minimal() +
  labs(title = "Map with Sample Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Locations") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )



data <- accident_data_prep %>% 
  select(
    Index, Accident_Severity, Latitude, Longitude, Number_of_Casualties, `District Area`
  ) %>% na.omit()
# Convert to spatial data frame
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)
# Get world map
world <- ne_countries(scale = "medium", returnclass = "sf")
# Plot the map
ggplot(data = world) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = data_sf, aes(color = `District Area`), size = 5) +
  theme_minimal() +
  labs(title = "UK Road Accidents Casualties Distributed by Location",
       x = "Longitude",
       y = "Latitude",
       color = "Locations") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )




# Sample dataset
data <- data.frame(
  id = 1:5,
  name = c("Location1", "Location2", "Location3", "Location4", "Location5"),
  latitude = c(37.7749, 34.0522, 40.7128, 41.8781, 47.6062),
  longitude = c(-122.4194, -118.2437, -74.0060, -87.6298, -122.3321)
)
# Define the bounding box for the USA
usa_bbox <- c(left = -125, bottom = 25, right = -65, top = 50)
# Get the map
usa_map <- get_stadiamap(bbox = usa_bbox, zoom = 5, maptype = "stamen_terrain") # stamen_toner_lite

# Define the center and zoom level for the USA
center_us <- c(lon = -95.7129, lat = 37.0902)
# Get the map
usa_map <- get_map(location = center_us, zoom = 4, source = "osm")
# Plot the map
ggmap(usa_map) +
  geom_point(data = data, aes(x = longitude, y = latitude, color = name), size = 3) +
  theme_minimal() +
  labs(title = "Zoomed Map of the USA with Sample Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Locations")
# Customize the map
ggmap(usa_map) +
  geom_point(data = data, aes(x = longitude, y = latitude, color = name), size = 3) +
  theme_minimal() +
  labs(title = "Zoomed Map of the USA with Sample Locations",
       x = "Longitude",
       y = "Latitude",
       color = "Locations") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

accident_data_prep %>% select(`District Area`, Number_of_Casualties) %>%
  state_chloropleth()



showing_key()

ggmap_show_api_key()

ggmap_hide_api_key()

scrub_key(string, with = "xxx")

register_google(
  key,
  account_type,
  client,
  signature,
  second_limit,
  day_limit,
  write = FALSE
)
