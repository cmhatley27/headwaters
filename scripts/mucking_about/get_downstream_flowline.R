library(tidyverse)
library(sf)
library(nhdplusTools)

#set up point data (snotel location)
point <- data.frame(
  lat = 38.9424,
  lon = -95.1583
)
#find the nearest downstream flowline
index <- get_nldi_index(c(point$lon[1], point$lat[1]))
#create the list object that the 'navigate_nldi' function needs to build downstream flowlines
#using the comid from the index
location <- list('featureSource' = 'comid', featureID = as.character(index$comid[1]))
#build the downstream flowline
try(flowline_down <- navigate_nldi(location, mode = 'DM', distance_km = 100)$DM_flowlines)
#combine all the segments into one multiline
flowline_comb <- st_union(flowline_down)
#convert to sf object for plotting
flowline_sf <- st_as_sf(flowline_comb) 

#convert the original point data to sf object for plotting, and assign lat/long crs
point_sf <- st_as_sf(point, coords = c('lon', 'lat'))
st_crs(point_sf) <- 4269

#plot to see if it did it right
ggplot() +
  geom_sf(data = point_sf, color = 'red') +
  geom_sf(data = flowline_sf)
