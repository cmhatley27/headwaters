library(tidyverse)
library(dataRetrieval)
library(sf)
library(nhdplusTools)
source('scripts/functions/Theme+Settings.R')
source('scripts/functions/read_gages.R')
source('scripts/functions/read_spatial.R')

# function to fix gage IDs from the list of downstream gages.
id_fixinator <- function(x) {
  x_char <- as.character(x)
  wrong_ids <- str_length(x_char) == 7
  x_char[wrong_ids] <- paste0('0',x_char[wrong_ids])
  
  #ids that slipped through cracks:
  manual_fixes <- c('11055566', '11230695', '211139110', '214291555', '214676115')
  x_char[x_char %in% manual_fixes] <- paste0('0',x_char[x_char %in% manual_fixes])
  return(x_char)
}

# read gages and the lookup table for gage connections
gage_info <- read_gage_info()
nhd_connections <- read_csv('data/shapefiles/nhd/nhd_connections.csv') %>%
  filter(!is.na(provider_id)) %>%
  mutate(across(contains('provider_id'), id_fixinator)) %>%
  filter(!str_detect(provider_id, 'e'))

# filter connections to only show ones where the upstream gage is in our
# headwaters set
connections <- filter(nhd_connections, origin_gage_provider_id %in% gage_info$site_no & updown == 'downmain') %>%
  select(headwater_id = origin_gage_provider_id, downstream_id = provider_id,
         drainage_ratio = origin_to_compared_gage_DA_ratio_NWIS)


headwaters <- connections$headwater_id
downstreams <- connections$downstream_id

out <- data.frame(
  headwater_id = headwaters,
  downstream_id = downstreams,
  length = NA,
  geometry = NA
)
for(conn in 1:nrow(connections)){
  Sys.sleep(0.1)
  # conn <- 11
  #set headwater/downstream connection
  headwater <- paste0('USGS-',headwaters[conn])
  downstream <- paste0('USGS-',downstreams[conn])
  # headwater <- 'USGS-01094400'
  # downstream <- 'USGS-01094500'
  
  Sys.sleep(0.1)
  #get downstream info
  ds_site <- list('featureSource' = 'nwissite', featureID = downstream)
  try(ds_site_feature <- get_nldi_feature(ds_site))
  try(ds_site_nhd <- as.data.frame(
    subset_nhdplus(comids = as.integer(ds_site_feature$comid), nhdplus_data = 'download',
                   status = F)$NHDFlowline_Network
  ))
  
  Sys.sleep(0.1)
  #get headwater info
  hw_site <- list('featureSource' = 'nwissite', featureID = headwater)
  try(hw_site_feature <- get_nldi_feature(hw_site))
  try(hw_site_nhd <- as.data.frame(
    subset_nhdplus(comids = as.integer(hw_site_feature$comid), nhdplus_data = 'download',
                   status = F)$NHDFlowline_Network
  ))
  
  #get flowlines in both directions and take the overlap
  try(flowline_down <- navigate_nldi(hw_site, mode = 'DM', distance_km = 200)$DM_flowlines)
  try(flowline_up <- navigate_nldi(ds_site, mode = 'UT', distance_km = 200)$UT_flowlines)
  flowlines <- filter(flowline_down, nhdplus_comid %in% flowline_up$nhdplus_comid)
  
  flowline_comb <- st_union(flowlines)
  
  out$length[conn] <- st_length(flowline_comb)
  out$geometry[conn] <- flowline_comb 

  print(paste0(conn,'/',nrow(connections),' connections done!!'))
  Sys.sleep(0.1)
}

out_sf <- st_as_sf(out)

write_csv(out_sf, './data/shapefiles/nhd/connection_flowlines.csv')

