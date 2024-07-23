# Read spatial data for mapping -------------------------------------------
require(sf)

dir_states <- file.path('data','shapefiles','states','States_shapefile.shp')
states <- st_read(dir_states) %>%
  filter(State_Code != 'AK', State_Code != 'HI')
rm(dir_states)