## Install 
install.packages("ggspatial")
install.packages("prettymapr")

## Load Libraries
library(jsonlite)
library(tidyverse)
library(writexl)
library(sf)
library(ggplot2)
library(ggspatial)
library(prettymapr)

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

## Save as GeoJSON for GIS use
st_write(stations_sf, "Toronto_Bikeshare_Stations.geojson", delete_dsn = TRUE)

## Quick Plot
plot(stations_sf["Capacity"])

## Map with Toronto basemap
station_utilization_map <- ggplot() +
  annotation_map_tile(type = "cartolight", zoom = 11) +  
  geom_sf(
    data = stations_sf,
    aes(color = Utilization),
    size = 1.8,
    alpha = 0.8
  ) +
  scale_color_viridis_c(
    option = "magma",
    name = "Station Utilization (%)"
  ) +
  labs(
    title = "Bike Share Station Utilization in Toronto",
    subtitle = paste(
      "Real-time availability as a percentage of station capacity",
      "\nData collected on", timestamp
    )
  ) +
  theme_void() +  
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

station_utilization_map

## Export
ggsave(
  filename = paste0("station_utilization_map_", timestamp, ".png"),
  plot = station_utilization_map,
  width = 10,
  height = 7,
  dpi = 300
)