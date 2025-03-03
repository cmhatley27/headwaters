library(tidyverse)
library(dataRetrieval)
source('scripts/functions/Theme+Settings.R')
source('scripts/functions/read_gages.R')
source('scripts/functions/read_spatial.R')
source('scripts/functions/utility_functions.R')

cut_scale <- function(x){
  cut_scale <- cut(x, c(0,1,max(x)), right = F, labels = c('0', '1+'))
  return(cut_scale)
}

# Get upstream/downstream connections from John Hammond's table and gather all gage info -----------

# Many gages had the leading 0s cut off from their IDs, this function
# pastes them back on.
id_fixinator <- function(x) {
  x_char <- as.character(x)
  wrong_ids <- str_length(x_char) == 7
  x_char[wrong_ids] <- paste0('0',x_char[wrong_ids])
  
  #a few IDs with abnormal lengths slipped through cracks:
  manual_fixes <- c('11055566', '11230695', '211139110', '214291555', '214676115')
  x_char[x_char %in% manual_fixes] <- paste0('0',x_char[x_char %in% manual_fixes])
  return(x_char)
}

# read headwater gages and the lookup table (from John) for gage connections
hw_gage_info <- read_gage_info() %>%
  mutate(type = 'headwater')
nhd_connections <- read_csv('data/shapefiles/nhd/nhd_connections.csv') %>%
  filter(!is.na(provider_id)) %>%
  mutate(across(contains('provider_id'), id_fixinator)) %>%
  filter(!str_detect(provider_id, 'e'))

# filter connections to only show ones where the upstream gage is in our
# headwaters set
connections <- filter(nhd_connections, origin_gage_provider_id %in% hw_gage_info$site_no & updown == 'downmain') %>%
  select(headwater_id = origin_gage_provider_id, downstream_id = provider_id,
         drainage_ratio = origin_to_compared_gage_DA_ratio_NWIS)

# get gage info for all the downstream sites
ds_site_nos <- as.character(unique(connections$downstream_id))
ds_gage_info <- readNWISsite(ds_site_nos) %>%
  select(site_no, station_nm, dec_lat_va, dec_long_va) %>%
  mutate(type = 'downstream')

# combine upstream and downstream gages into one data frame
gages_comb <- rbind(hw_gage_info, ds_gage_info)

# Count number of downstream connections each headwater gage has and attach to
# combined gage list
connection_counts <- as.data.frame(table(connections$headwater_id))
names(connection_counts) <- c('site_no','connections')

gages_comb_all <- gages_comb %>%
  left_join(., connection_counts) %>%
  mutate(connections = ifelse(type == 'headwater' & is.na(connections), 0, connections))

#number of headwater and downstream gages in set
table(gages_comb_all$type)
#number of headwater gages with at least one connection to a downstream gage
length(gages_comb_all$site_no[gages_comb_all$type == 'headwater' & gages_comb_all$connections > 0])


# Plot gages without any additional filters -------------------------------

#just headwater gages
ggplot(subset(gages_comb_all, type != 'downstream'), aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = F) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL) +
  geom_point()

#headwater + downstream gages
ggplot(gages_comb_all, aes(x = dec_long_va, y = dec_lat_va, color = type, size = type, shape = type)) +
  geom_sf(data = states, inherit.aes = F) +
  geom_point() +
  scale_color_manual(limits = c('downstream','headwater'),
                     labels = c('Downstream', 'Headwater'),
                     values = c('royalblue', 'black')) +
  scale_size_manual(limits = c('downstream','headwater'),
                     labels = c('Downstream', 'Headwater'),
                     values = c(0.75, 1.5)) +
  scale_shape_manual(limits = c('downstream','headwater'),
                     labels = c('Downstream', 'Headwater'),
                     values = c(3, 16)) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL) +
  theme(legend.position = 'right')
  # ggtitle('n = 965 gages located within 200km downstream of a headwaters gage')

#number of downstream connections for each headwater gage
ggplot(subset(gages_comb_all, type == 'headwater'), 
       aes(x = dec_long_va, y = dec_lat_va, color = cut_scale(connections), size = cut_scale(connections))) +
  geom_sf(data = states, inherit.aes = F) +
  geom_point() +
  scale_color_manual(limits = c('0','1+'),values = c('red','black'), name = '# Connections') +
  scale_size_manual(limits = c('0','1+'),values = c(1,1.5), name = '# Connections') +
  theme(legend.position = 'right') +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = NULL)
  # ggtitle('456/609 (75%) of headwaters gages are connected to a downstream gage')

#histogram of number of connections per headwater gage
ggplot(subset(gages_comb_all, type == 'headwater'), aes(x = connections)) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0,13,by=1)) +
  xlab('# Connections') +
  ggtitle('Most headwaters gages with downstream connections are connected to multiple downstream gages')


# Establish additional filters on connections --------------------------------

## drainage area ratio threshold -------------------------------------------
#set drainage area ratio threshold (headwater/downstream DA)
da_ratio_thresh <- 0.2

#plot maximum DA ratio for all headwater streams to determine threshold suitability
max_da_check <- connections %>%
  group_by(headwater_id) %>%
  summarize(max_da = max(drainage_ratio, na.rm = T))
ggplot(max_da_check, aes(x = max_da)) +
  geom_histogram() + geom_vline(xintercept = da_ratio_thresh, color = 'red') +
  scale_x_continuous(breaks = seq(0,1.5, by = 0.2), name = 'Max drainage area ratio among downstream connections') +
  ylab('# headwater gages')
  # ggtitle('Maximum drainage area ratio for each headwater gage with a downstream connection')

#number of headwaters gages with at least one connection above the threshold
sum(max_da_check$max_da >= da_ratio_thresh)


## data completeness of downstream gages -----------------------------------
#set threshold for maximum allowable number of NAs in downstream gages
missing_thresh <- 365*3

ds_nwis <- whatNWISdata(
  siteNumber = as.character(ds_site_nos),
  parameterCd = '00060',
  service = 'dv') 

#set time period of interest
start_year <- 1981
end_year <- 2022
start_date_sel <- paste0(start_year,'-10-01')
end_date_sel <- paste0(end_year,'-09-30')
dur <- as.numeric(ymd(end_date_sel)-ymd(start_date_sel)) + 1

#count number of NAs within period of interest
ds_availability <- ds_nwis %>%
  filter(ymd(end_date_sel) <= ymd(end_date),
         ymd(start_date_sel) >= ymd(begin_date)) %>%
  mutate(days_out_of_period = (as.numeric(ymd(end_date) - ymd(end_date_sel))) + (as.numeric(ymd(start_date_sel) - ymd(begin_date))),
         days_in_period = count_nu - days_out_of_period,
         days_missing = dur - days_in_period)

#plot minimum number of NAs in a downstream connection for each headwateer gage to determine threshold suitability
avail_check <- connections %>%
  left_join(select(ds_availability, c(downstream_id = site_no, days_missing))) %>%
  group_by(headwater_id) %>%
  summarize(min_missing = min(days_missing, na.rm = T))
ggplot(avail_check, aes(x = min_missing)) + geom_histogram(binwidth = 365) +
  geom_vline(xintercept = missing_thresh, color = 'red') +
  scale_x_continuous(limits = c(-365/2,365*10), breaks = seq(0,365*10, by = 365),
                     name = 'Min number of NA discharge values among downstream connections (truncated at 10 years)') +
  ylab('# headwater gages')

#number of headwaters gages with at least one connection below the threshold
sum(avail_check$min_missing <= missing_thresh)



## dam presence ------------------------------------------------------------
#read National Inventory of Dams and flowline data
nid <- read_sf('data/shapefiles/nid/nation.gpkg') %>%
  filter(as.numeric(nidStorage) > 0)
flowlines <- read_sf('data/shapefiles/nhd/connection_flowlines.gpkg')
#convert gages to sf for intersection calculations
gages_sf <- st_as_sf(gages_comb_all, coords = c('dec_long_va','dec_lat_va'), crs = 4269)

#determine how many flowlines have a dam within 10m (assuming that anything
#within this buffer = on the river)
dist_thresh <- 10
units(dist_thresh) <- 'm'
intersections <- st_is_within_distance(flowlines, nid, dist = 10)

#identify flowlines with a dam intersection
dammed_connections <- (lengths(intersections) > 0)
sum(dammed_connections)

#count number of headwater gags that have at least one non-dammed connection
dam_check <- connections %>%
  mutate(dam = as.numeric(dammed_connections)) %>%
  group_by(headwater_id) %>%
  summarise(dam = min(dam))
nrow(filter(dam_check, dam == 0))

#plot an individual connection to check everything is working
# connection_check <- 6
# dam_check <- intersections[[connection_check]]
# ggplot() +
#   geom_sf(data = flowlines[connection_check,]) +
#   geom_sf(data = nid[dam_check,]) +
#   geom_sf(data = subset(gages_sf, site_no == flowlines$headwater_id[connection_check]), color = 'blue') +
#   geom_sf(data = subset(gages_sf, site_no == flowlines$downstream_id[connection_check]), color = 'red')
# nid$nidStorage[dam_check]

#plot all connections color coded by dam presence
# ggplot() +
#   geom_sf(data = states, inherit.aes = F) +
#   geom_sf(data = mutate(flowlines, dam = dammed_connections), aes(color = dam)) +
#   scale_color_manual(name = 'dammed', limits = c(F,T), values = c('blue','red'))

# Filter connections -------------------------------------------------------

#drainage area ratio
da_ratio_thresh <- 0.2
sum(max_da_check$max_da >= da_ratio_thresh) #number of headwaters retained

#downstream gage data availability
missing_thresh <- 365*3
good_downstreams <- ds_availability$site_no[ds_availability$days_missing <= missing_thresh]
sum(avail_check$min_missing <= missing_thresh) #number of headwaters retained

#dam presence
nrow(filter(dam_check, dam == 0)) #number of headwaters retained

#create filter flags
connection_checks <- mutate(connections,
                            above_da_thresh = ifelse(drainage_ratio >= da_ratio_thresh, 1, 0),
                            downstream_is_headwater = ifelse(downstream_id %in% hw_gage_info$site_no, 1, 0),
                            good_downstream_availability = ifelse(downstream_id %in% good_downstreams, 1, 0),
                            is_dammed = as.numeric(dammed_connections))

#count number of connections filtered/retained from each filter
table(connection_checks$above_da_thresh)
table(connection_checks$downstream_is_headwater)
table(connection_checks$good_downstream_availability)
table(connection_checks$is_dammed)

#combine selected filters and attach retained connections to full gage list
connection_counts_fil <- as.data.frame(
  table(filter(connection_checks,
               above_da_thresh == 1,
               good_downstream_availability == 1,
               is_dammed == 0
  )$headwater_id)
)
names(connection_counts_fil) <- c('site_no','connections')

gages_comb_fil <- gages_comb %>%
  left_join(., connection_counts_fil) %>%
  mutate(connections = ifelse(type == 'headwater' & is.na(connections), 0, connections))

#number of headwater and downstream gages in set
table(gages_comb_fil$type)
#number of headwater gages with at least one connection to a downstream gage
length(gages_comb_fil$site_no[gages_comb_fil$type == 'headwater' & gages_comb_fil$connections > 0])

# plot retained gages -----------------------------------------------------

#updated number of downstream connections for each headwater gage
ggplot(subset(gages_comb_fil, type == 'headwater'), 
       aes(x = dec_long_va, y = dec_lat_va, color = cut_scale(connections), size = cut_scale(connections))) +
  geom_sf(data = states, inherit.aes = F) +
  geom_point() +
  scale_color_manual(limits = c('0','1+'),values = c('red','black'), name = '# Connections') +
  scale_size_manual(limits = c('0','1+'),values = c(1,1.5), name = '# Connections') +
  theme(legend.position = 'right') +
  xlab('') +
  ylab('')
  # ggtitle('195/609 (32%) of headwaters gages are connected to a downstream gage\nwithin the drainage area ratio threshold')

#just the headwater gages with 1+ connections
ggplot(subset(gages_comb_fil, type == 'headwater' & connections > 0), 
       aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = F) +
  geom_point() +
  theme(legend.position = 'right') +
  xlab('') +
  ylab('')

#histogram of number of connections for each headwater gage
ggplot(subset(gages_comb_fil, type == 'headwater'), aes(x = connections)) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0,13,by=1)) +
  xlab('# Connections')
  # ggtitle('There are much fewer headwaters gages with multiple downstream connections\nafter applying the drainage area ratio threshold')