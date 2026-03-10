library(tidyverse)
library(sf)
library(nhdplusTools)

#set up data frame where all the flowlines will be stored. One row per USGS gage
#that you want upstream flowlines for.
upstream_flowlines <- data.frame(
  site_no = '06892350', #feed in USGS gage ids for as many sites as you want
  geometry = NA
)

#loop through gages and grab upstream flowlines from NHD
for(gage in 1:nrow(upstream_flowlines)){
  #short pauses are interspersed in here because the functions freak out if 
  #everything happens too fast
  Sys.sleep(0.1)
  #set headwater gage
  headwater <- paste0('USGS-',upstream_flowlines$site_no[gage])
  Sys.sleep(0.1)
  #get headwater info from NHD
  hw_site <- list('featureSource' = 'nwissite', featureID = headwater)
  # try(hw_site_feature <- get_nldi_feature(hw_site))
  # try(hw_site_nhd <- as.data.frame(
  #   subset_nhdplus(comids = as.integer(hw_site_feature$comid), nhdplus_data = 'download',
  #                  status = F)$NHDFlowline_Network
  # ))
  Sys.sleep(0.1)
  #get flowlines upstream
  try(flowline_up <- navigate_nldi(hw_site, mode = 'UT', distance_km = 200)$UT_flowlines)
  Sys.sleep(0.1)
  #union all the individual flowline segments into one big multisegment
  try(flowline_comb <- st_union(flowline_up))
  #if there's ann error just assign NA and skip
  if(is.null(flowline_up)){
    flowline_comb <- NA
  }
  
  upstream_flowlines$geometry[gage] <- flowline_comb 
  
  print(paste0(gage,'/',nrow(upstream_flowlines),' gages done!!'))
}

#convert to sf object
upstream_flowlines_sf <- st_as_sf(upstream_flowlines)

#assign a lat/long crs
st_crs(upstream_flowlines_sf) <- 4269

#plot it to make sure its legit
ggplot() +
  geom_sf(data = upstream_flowlines_sf[1,])
nhdplusTools::
#save as geopackage for compression and loading later
st_write(upstream_flowlines_sf, './data/shapefiles/nhd/hw_upstream_flowlines.gpkg', append = F)