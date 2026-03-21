install.packages("tinytex")
tinytex::install_tinytex()
## Load packages
library(tidyverse)
library(sf)
library(ggplot2)
## Load data
bikeshare <- read_csv("bikeshare_stations.csv")

bikeshare_sf <- st_as_sf(
  bikeshare,
  coords = c("lon", "lat"),
  crs = 4326
)
neighborhoods_sf <- st_read("Neighbourhoods.geojson")
## Spatial join
bikeshare_joined <- st_join(bikeshare_sf, neighborhoods_sf)
## Count stations per neighbourhood
stations_count <- bikeshare_joined %>%
  st_drop_geometry() %>%
  group_by(AREA_NAME) %>%
  summarise(num_stations = n())
## Join counts back to polygons
neighborhoods_access <- neighborhoods_sf %>%
  left_join(stations_count, by = "AREA_NAME")
# Project to meters
bikeshare_proj <- st_transform(bikeshare_sf, 32617)
neighborhoods_proj <- st_transform(neighborhoods_access, 32617)

# Create 1 km service area
buffer_area <- st_buffer(bikeshare_proj, dist = 1000)
buffer_union <- st_union(buffer_area)

# Clip neighbourhoods to service area
neighborhoods_clipped <- st_intersection(neighborhoods_proj, buffer_union)

## Create map
map_plot <- ggplot() +
  geom_sf(data = neighborhoods_clipped,
          aes(fill = num_stations),
          color = "grey30",
          size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Bike Share Accessibility (Service Area)",
    subtitle = "Areas within 1 km of Bike Share Stations",
    fill = "Stations"
  ) +
  theme_void()
## Export map
ggsave("bike_accessibility_clipped.png",
       plot = map_plot,
       width = 8, height = 6, dpi = 300)


