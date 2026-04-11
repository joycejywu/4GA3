## Install 
install.packages("ggspatial")
install.packages("prettymapr")
install.packages("opendatatoronto") 

## Load Libraries
library(jsonlite)
library(tidyverse)
library(writexl)
library(sf)
library(ggplot2)
library(ggspatial)
library(prettymapr)
library(RColorBrewer)
library(opendatatoronto)

## Load JSON data 
bike_json <- fromJSON("https://tor.publicbikesystem.net/ube/gbfs/v1/en/station_information.json")

glimpse(bike_json) 

## Load station status 
status_json <- fromJSON("https://tor.publicbikesystem.net/ube/gbfs/v1/en/station_status.json")
status <- status_json$data$stations

glimpse(status)

## Extract Stations Table
stations <- bike_json$data$stations

## Join info + live data 
stations_full <- stations %>%
  left_join(status, by = "station_id")

glimpse(stations_full)

## Inspect Dataset
head(stations)
View(stations)
glimpse(stations)
names(stations)

## Export Raw data 
write_xlsx(stations, "stations_full_raw.xlsx")

## Clean Data
stations_clean <- stations_full %>%
  select(
    station_id,
    name,
    lat,
    lon,
    capacity,
    num_bikes_available,
    num_docks_available,
    groups
  ) %>%
  rename(
    Station_ID = station_id,
    Station_Name = name,
    Latitude = lat,
    Longitude = lon,
    Capacity = capacity,
    Bikes_Available = num_bikes_available,
    Docks_Available = num_docks_available,
    Area = groups
  ) %>%
  arrange(desc(Capacity))

## Fix area from list to text 
stations_clean <- stations_clean %>%
  mutate(Area = map_chr(Area, ~ paste(.x, collapse = ", ")))

## Region/Neighborhood 
stations_clean <- stations_clean %>%
  separate(Area, into = c("Region", "Neighbourhood"), sep = ", ")

## Utilization of Stations 
stations_clean <- stations_clean %>%
  mutate(Utilization = (Bikes_Available / Capacity) * 100)
# Shows how full each station is (% of bikes vs total capacity)

## System balance (net availability)
stations_clean <- stations_clean %>%
  mutate(Net_Availability = Bikes_Available - Docks_Available)

## Timestamp of Data 
stations_clean <- stations_clean %>%
  mutate(Data_Collected = Sys.time())

## Timestamp for File Name 
timestamp <- format(
  max(stations_clean$Data_Collected),
  "%B %d, %Y at %I:%M %p"
)

## Export Clean Data 
write_xlsx(stations_clean, "Toronto_Bikeshare_Stations.xlsx")

## Convert to Spatial Data
stations_sf <- st_as_sf(
  stations_clean,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

## Utilization map with Toronto basemap
station_utilization_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +  
  
  geom_sf(
    data = stations_sf,
    aes(color = Utilization),
    size = 2.8,
    alpha = 1
  ) +
  
  scale_color_viridis_c(
    option = "magma",
    direction = -1,
    begin = 0.2,
    end = 1,
    name = "Utilization (%)"
  ) +
  
  labs(
    title = "Bike Share Station Utilization in Toronto",
    subtitle = paste(
      "Higher % = more bikes relative to capacity | Lower % = fewer bikes",
      "\nData collected on", timestamp
    )
  ) +
  
  theme_void() +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  )

station_utilization_map

## Balance map
station_balance_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +
  
  geom_sf(
    data = stations_sf,
    aes(color = Net_Availability),
    size = 2.8,
    alpha = 1
  ) +
  
  scale_color_gradient2(
    low = "#2166ac",
    mid = "#f7f7f7",
    high = "#b2182b",
    midpoint = 0,
    limits = c(-50, 50),
    breaks = c(-50, -25, 0, 25, 50),
    name = "Net Availability\n(Bikes - Docks)"
  ) +
  
  labs(
    title = "Bike Share System Balance in Toronto",
    subtitle = paste(
      "Blue = low bikes (high demand) | Red = high bikes (low demand)",
      "\nData collected on", timestamp
    )
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  )

station_balance_map

## Export
ggsave(
  filename = paste0("station_utilization_map_", timestamp, ".png"),
  plot = station_utilization_map,
  width = 10,
  height = 7,
  dpi = 300
)
ggsave(
  filename = paste0("system_balance_map_", timestamp, ".png"),
  plot = station_balance_map,
  width = 10,
  height = 7,
  dpi = 300
)

# Adding Neighbourhood Geographies
package <- show_package("fc443770-ef0a-4025-9c2c-2cb558bfab00")

resources <- list_package_resources("fc443770-ef0a-4025-9c2c-2cb558bfab00")

datastore_resources <- resources %>%
  filter(tolower(format) %in% c("csv", "geojson"))

toronto_nbh <- datastore_resources %>%
  slice(1) %>%
  get_resource()

# Toronto boundary 
toronto_boundary <- toronto_nbh %>%
  st_union()

# Toronto Map
Toronto_Map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +
  
  geom_sf(
    data = toronto_nbh,
    fill = NA,
    color = "grey40",
    size = 0.2,
    alpha = 0.5
  ) +
  
  # Toronto boundary (bold outline)
  geom_sf(
    data = toronto_boundary,
    fill = NA,
    color = "black",
    size = 0.8
  ) +
  
  labs(
    title = "Study Area: City of Toronto",
    subtitle = "Boundary derived from neighbourhood polygons"
  ) +
  
  theme_void() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

Toronto_Map

ggsave(
  filename = paste0("Toronto_Map", timestamp, ".png"),
  plot = Toronto_Map,
  width = 10,
  height = 7,
  dpi = 300
)