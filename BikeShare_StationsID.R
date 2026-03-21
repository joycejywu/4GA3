## Libraries 
library(jsonlite)
library(tidyverse)
library(writexl)
## LOAD JSON
bike_json <- fromJSON("station_information.json")
## Extract stations table
stations <- bike_json$data$stations
## Clearn/organize data
stations_clean <- stations %>%
  select(
    station_id,
    name,
    lat,
    lon,
    capacity
  ) %>%
  rename(
    Station_ID = station_id,
    Station_Name = name,
    Latitude = lat,
    Longitude = lon,
    Capacity = capacity
  ) %>%
  arrange(desc(Capacity))
## Export to Excel 
write_xlsx(stations_clean, "Toronto_Bikeshare_Stations.xlsx")