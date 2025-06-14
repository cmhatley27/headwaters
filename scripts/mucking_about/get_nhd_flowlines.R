library(tidyverse)
library(dataRetrieval)
library(sf)
library(nhdplusTools)
source('scripts/functions/Theme+Settings.R')
source('scripts/functions/read_gages.R')
source('scripts/functions/read_spatial.R')

# get upstream flowlines ------------------------------------------------
upstream_flowlines <- data.frame(
  site_no = 'xxx', #feed in USGS gage ids
  geometry = NA
)
for(gage in 1:nrow(upstream_flowlines)){
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
  try(flowline_comb <- st_union(flowline_up))
  if(is.null(flowline_up)){
    flowline_comb <- NA
  }
  
  upstream_flowlines$geometry[gage] <- flowline_comb 
  
  print(paste0(gage,'/',nrow(upstream_flowlines),' gages done!!'))
}
upstream_flowlines_sf <- st_as_sf(upstream_flowlines)
st_crs(upstream_flowlines_sf) <- 4269


# save output -------------------------------------------------------------

st_write(upstream_flowlines_sf, './data/shapefiles/nhd/hw_upstream_flowlines.gpkg', append = F)



# Get upstream/downstream connections from John Hammond's table -----------

# Many gages had the leading 0s cut off from their IDs, this function
# pastes them back on.
id_fixinator <- function(x) {
  x_char <- as.character(x)
  wrong_ids <- str_length(x_char) == 7
  x_char[wrong_ids] <- paste0('0',x_char[wrong_ids])
  
  #ids that slipped through cracks:
  manual_fixes <- c('11055566', '11230695', '211139110', '214291555', '214676115')
  x_char[x_char %in% manual_fixes] <- paste0('0',x_char[x_char %in% manual_fixes])
  return(x_char)
}

# read gages and the lookup table (from John) for gage connections
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


# Loop through connections and extract flowline segments ------------------
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
st_crs(out_sf) <- 4269

# save output -------------------------------------------------------------
st_write(out_sf, './data/shapefiles/nhd/connection_flowlines.gpkg', append = F)


# Look for intersections between dams and connection flowlines ------------

#read National Inventory of Dams and flowline data
nid <- read_sf('data/shapefiles/nid/nation.gpkg') %>%
  filter(as.numeric(nidStorage) > 0)

flowlines <- read_sf('data/shapefiles/nhd/connection_flowlines.gpkg')
#convert gages to sf for intersection calculations
gages_sf <- st_as_sf(gages_all_connections, coords = c('dec_long_va','dec_lat_va'), crs = 4269)

#determine how many flowlines have a dam within 10m (assuming that anything
#within this buffer = on the river)
dist_thresh <- 10
units(dist_thresh) <- 'm'
intersections <- st_is_within_distance(flowlines, nid, dist = dist_thresh)

#identify flowlines with a dam intersection
dammed_connections <- (lengths(intersections) > 0)
sum(dammed_connections)

#count number of headwater gags that have at least one non-dammed connection
dam_check <- connections %>%
  mutate(dam = as.numeric(dammed_connections)) %>%
  group_by(headwater_id) %>%
  summarise(dam = min(dam))
nrow(filter(dam_check, dam == 0))

# plot an individual connection to check everything is working
connection_check <- 195
dam_check <- intersections[[connection_check]]
ggplot() +
  geom_sf(data = flowlines[connection_check,]) +
  geom_sf(data = nid[39968,]) +
  geom_sf(data = subset(gages_sf, site_no == flowlines$headwater_id[connection_check]), color = 'blue') +
  geom_sf(data = subset(gages_sf, site_no == flowlines$downstream_id[connection_check]), color = 'red')
nid$nidStorage[dam_check]

which(str_detect(nid$otherNames, 'Bowersock'))
#plot all connections color coded by dam presence
# ggplot() +
#   geom_sf(data = states, inherit.aes = F) +
#   geom_sf(data = mutate(flowlines, dam = dammed_connections), aes(color = dam)) +
#   scale_color_manual(name = 'dammed', limits = c(F,T), values = c('blue','red'))


# plot it up --------------------------------------------------------------
flowlines <- st_read('./data/shapefiles/nhd/connection_flowlines.gpkg')

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = flowlines)


