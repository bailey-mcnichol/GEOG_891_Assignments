
library(tidyverse)
library(leaflet)

# Set your initial working space (initialize)
m <- leaflet()
m

# Add some tiles (pieces of map)
m <- leaflet() %>%
  addTiles()
m

# Add a marker
m <- leaflet() %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addMarkers(lng = -96.703090, lat = 40.819288, 
             popup="The Burnett Hall GIS Lab") # defines what pops up when click it
m

# Create some data to plot
# start with a data frame
df <- data.frame(
  lat = rnorm(100), # samples from Gaussian dist. with s.d. = 1
  lng = rnorm(100),
  size = runif(100, 5, 20), # samples 100 pts btwn 5-20 from uniform distribution
  color = sample(colors(), 100) # sampling from colors 100x
)
# then add the data frame to a leaflet map
m2 <- leaflet(df) %>% addTiles() # Still haven't added the geometry!

# Look at data
m2$x

# Try to visualize it
# first one
m2 %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
# second one - draws from the uniform distributions within the call, makes all the points red
m2 %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

# Other options for mapping tiles

m <- leaflet() %>% setView(lng = -96.703090, lat = 40.81928, zoom = 14)
m %>% addTiles()

# third party tiles using addProvider() function
m %>% addProviderTiles(providers$Stamen.Toner) # white bckground with black
m %>% addProviderTiles(providers$CartoDB.Positron) # muted colors
m %>% addProviderTiles(providers$CartoDB.DarkMatter) # black with dark grey
m %>% addProviderTiles(providers$Esri.NatGeoWorldMap) # NatGeo


# Read in some data!
# State parks shape file
parks <- sf::read_sf("./data/State_Park_Locations.shp")
# set up the map, zoom out a bit
mp <- leaflet(data = parks) %>% setView(lng = -96.703090, lat = 40.81928, zoom = 10)
# Plot it with the pop-ups/labels being the named the same as the parks
# Pop-up shows up on click, label shows up on mouse hover
mp %>% addTiles() %>%
  addMarkers(popup = ~AreaName, label = ~AreaName)

# Add some lines
streams <- sf::read_sf("./data/Streams_303_d_.shp")
ms <- leaflet(data = streams) %>%
  setView(lng = -96.703090, lat = 40.81928, zoom = 10) %>%
  addTiles() %>%
  addPolylines(., color = "blue",
               popup = ~paste0(Waterbody_, " - ", 
                      Impairment)) # paste0() pastes multiple strings together
ms

# Read in municipal boundaries shape file
bound <- sf::read_sf("./data/Municipal_Boundaries.shp")
names(bound)

# Multiple data layers
# do multiple layers by not passing the first "leaflet()" call a data argument
m.both <- leaflet() %>% # no data here!
  setView(lng = -96.703090, lat = 40.81928, zoom = 10) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(data = bound, popup = ~NAME, fillColor = "navy",
                  color = "darkgrey", weight = 0.8) %>%
   addMarkers(data = parks, popup = ~AreaName, # Add line data here
              label = ~AreaName) %>% 
   addPolylines(data = streams, color = "blue", # Add line data here
                popup = ~paste0(Waterbody_, " - ", Impairment))
    
m.both





