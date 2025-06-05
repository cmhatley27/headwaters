# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/Theme+Settings.R')
source('scripts/functions/read_gages.R')
source('scripts/functions/read_spatial.R')
source('scripts/functions/utility_functions.R')

all_gages <- read_csv('./data/gagesii/all_gages_summary.csv')
hw_gage_info <- read_gage_info()


# Load in gagesii headwaters/downstream connections -----------------------
# using John Hammond's table

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

#load in table and clean up weird gage IDs
connections <- read_csv('data/gagesii/all_gagesii_connections.csv') %>%
  filter(!is.na(provider_id)) %>%
  mutate(across(contains('provider_id'), id_fixinator)) %>%
  filter(!str_detect(provider_id, 'e')) %>%
  #filter to just the connections where upstream gages are in our headwaters set
  filter(origin_gage_provider_id %in% hw_gage_info$site_no & updown == 'downmain') %>%
  select(headwater_id = origin_gage_provider_id, downstream_id = provider_id,
         drainage_ratio = origin_to_compared_gage_DA_ratio_NWIS)


# Filter downstream gages by selected criteria -----------------------------
#filters for data avilability and hydrologic modification are calculated in 
#exactly the same way as those for the headwaters set. Instead of filtering for
#stream order here though, we instead filter for the drainage ratio between the
#headwater and downstream site so they are still of somewhat comparable size.
save = T #save filtered gage and connections list as csv? Will overwrite previous

min_drainage_ratio <- 0.1

## Data Availability (max number of missing values within selected period)
start_year <- 1981
end_year <- 2022
max_nas <- 365*3
#function to calculate number of NA values in selected period using the 'count_nu'
#column from the NWIS gage info that gives the total number of observations
#available for each gage.
get_nas_in_period <- function(period_start, period_end, record_start, record_end, num_obs){
  period_duration = as.numeric(period_end - period_start + 1)
  days_out_of_period = (as.numeric(record_end - period_end)) + (as.numeric(period_start - record_start))
  days_in_period = num_obs - days_out_of_period
  days_missing = period_duration - days_in_period
  return(days_missing)
}
all_gages$days_missing <- get_nas_in_period(ymd(paste0(start_year,'-10-01')), ymd(paste0(end_year,'-09-30')),
                                            all_gages$begin_date, all_gages$end_date, all_gages$obs_count)

## Hydrologic Modifications
max_storage_ratio <- 0.25
max_dist_index <- 56 #ranges from 0 to 56

# Apply Filters and Save
fil_connections <- filter(connections, drainage_ratio >= min_drainage_ratio)
fil_gages_ds <- all_gages %>%
  filter(site_no %in% fil_connections$downstream_id,
         ymd(end_date) >= ymd(paste0(end_year,'-09-30')),
         ymd(begin_date) <= ymd(paste0(start_year,'-10-01')),
         days_missing <= max_nas,
         storage_precip_ratio <= max_storage_ratio,
         dist_index <= max_dist_index)
#apply additional filter to connections to get ones where downstream gages are in
#our new downstream set
hw_ds_connections <- filter(fil_connections, downstream_id %in% fil_gages_ds$site_no)

print(paste0('Filter connections contain ',
             length(unique(hw_ds_connections$headwater_id)),' unique headwater gages',
             ' connected to ',nrow(fil_gages_ds),' unique downstream gages'))

if(save) write_csv(fil_gages_ds, paste0('./data/gages/ds_gage_info.csv'))
if(save) write_csv(hw_ds_connections, paste0('./data/gages/hw_ds_connections.csv'))


# map filtered gages ------------------------------------------------------
library(sf)
states <- st_read('./data/shapefiles/states/States_shapefile.shp') %>%
  filter(State_Code != 'AK' & State_Code != 'HI')
hw_gages_sf <- read_csv('./data/gages/hw_gage_info.csv') %>%
  st_as_sf(., coords = c('lon', 'lat'), crs = 4269)
ds_gages_sf <- read_csv('./data/gages/ds_gage_info.csv') %>%
  st_as_sf(., coords = c('lon', 'lat'), crs = 4269)
hw_ds_connections <- read_csv('./data/gages/hw_ds_connections.csv')

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = ds_gages_sf, color = 'blue', shape = 3) +
  geom_sf(data = subset(hw_gages_sf, site_no %in% hw_ds_connections$headwater_id)) +
  ggtitle(paste0('n = ',nrow(subset(hw_gages_sf, site_no %in% hw_ds_connections$headwater_id)),
                 ' headwater gages (black) with connections to downstream gages (blue)'))


# Explore filter values ---------------------------------------------------
# plots and stuff to help in selecting appropriate filter values
# Exact same filters for data availability in the headwater gages were used
# here in the downstream gages.

# drainage area ratio histogram -------------------------------------------
ggplot(connections, aes(x = drainage_ratio)) +
  geom_histogram() + geom_vline(xintercept = 0.1, color = 'red')


# hydrologic modifications -----------------------------------------------
start_year <- 1981
end_year <- 2022
min_da_ratio <- 0.1 #optional filter for drainage area ratio. set to 0 if unwanted

fil_connections <- filter(connections, drainage_ratio >= min_drainage_ratio)
ds_gages_modifications <- all_gages %>%
  filter(site_no %in% fil_connections$downstream_id,
         ymd(end_date) >= ymd(paste0(end_year,'-09-30')),
         ymd(begin_date) <= ymd(paste0(start_year,'-10-01')))

#distribution of dam influence here is pretty similar to the one for the headwaters.
#similar cutoff values of 0.1 seems fine.
ggplot(ds_gages_modifications, aes(x = storage_precip_ratio)) +
  geom_histogram(binwidth = 0.05) +
  xlim(-0.03,2) +
  geom_vline(xintercept = 0.25, color = 'red')

#disturbance index tends to be much higher for the downstream gages
ggplot(ds_gages_modifications, aes(x = dist_index)) +
  geom_histogram()

#but again applying the 0.1 storage ratio filter cuts out many of the gages with high disturbance indices
ggplot(subset(ds_gages_modifications, storage_precip_ratio >= 0.1), aes(x = dist_index)) +
  geom_histogram()

#reading column 'wr_comments' for the gages that remain after applying the storage
#ratio filter indicates that additionally cutting out gages with a disturbance index
# > 20 would take care of most of the gages with significant flow modification
#via reservoirs/diversions
view(subset(ds_gages_modifications, storage_precip_ratio <= 0.1))




