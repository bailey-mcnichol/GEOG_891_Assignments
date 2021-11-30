

library(tidyverse)
library(leaflet)
library(RColorBrewer)

# Shape file
dams <- sf::read_sf("./data/Dam_or_Other_Blockage_Removed_2012_2017.shp")
stations <- sf::read_sf("./data/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp")

glimpse(dams)
glimpse(stations)

# addMarkers(data = parks, popup = ~AreaName, # Add line data here
#            label = ~AreaName) %>% 
  
  
brewer.pal(n = 6, name = 'Dark2')

pal <- colorFactor(c("#1B9E77", "#D95F02",
                     "#7570B3", "#E7298A",
                     "#66A61E", "#E6AB02"),
                   domain = c("2012", "2013", "2014",
                              "2015", "2016", "2017"))

Base_Map <- providers$Esri.NatGeoWorldMap
Station_Name <- stations$STATION_NA
Dam_Removal <- dams$DamRemoval

## Make water icon
water <- iconList(drop = makeIcon("../data/water.drop.png", 10,10))

## Map
leaflet() %>% 
      setView(lng = -77.678505, lat = 39.801475, zoom = 7) %>%
      addProviderTiles(Base_Map, group = "Base_Map") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addCircleMarkers(data = stations, fill = TRUE, 
                       radius = ~log(Drainage_A),
                       fillColor = "blue",
                       group = "Station_Name",
                       fillOpacity = 0.5,
                       weight = 0) %>%
      addCircleMarkers(data = dams, group = "Dam_Removal",
                       color = ~pal(DamRemoval)) %>%
      #addMarkers(data = coord, lat = Y, lng = X,  icon = water)
  addLayersControl(
    baseGroups = c("Base_Map", "Toner"),
    overlayGroups = c("Station_Name", "Dam_Removal"),
    options = layersControlOptions(collapsed = FALSE))
               
names(dams)
                 

  