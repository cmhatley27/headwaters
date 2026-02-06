# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/Theme+Settings.R')
source('scripts/functions/load_states.R')
source('scripts/functions/utilities.R')
source('scripts/connections_v2/functions/get_gages_diff.R')
source('scripts/connections_v2/functions/get_huc8_matches.R')

all_gages <- read_csv('./data/gagesii/all_gages_summary.csv')
hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')
connections <- read_gage_info('connections')

# set matching criteria and get matches -----------------------------------
#which metrics to match based on? Use GAGESII column names. Geographic distance
#is automatically included, so don't need to specify it here
matching_metrics <- c('ELEV_MEAN_M_BASIN', 'PPTAVG_BASIN', 'T_AVG_BASIN', 'WD_BASIN', 'PRECIP_SEAS_IND')
#weights with which to match based on. The first weight in the vector is for
#geographic distance, and since distance is not specified in the columns vector above
#the weights vector will be 1 value longer than the columns vector
weights = c(1,1,1,1,1,1)
#calculate diffs
# diffs <- get_gages_diff(goi = hw_gage_info$site_no, 
#                         gagesii_cols = matching_metrics,
#                         weights = weights)
#or load a previous one
diffs <- read_csv('scripts/connections_v2/order123_hw_diffs_set1.csv')

#get list of gage pairings in the same huc8 and attach to differences frame
huc8_matches <- get_huc8_matches(hw_gage_info$site_no)
diffs <- left_join(diffs, huc8_matches)

#get difference metrics for nhd-connected gages
connection_diffs <- left_join(connections, diffs, by = join_by(headwater_id == goi_id,
                                                               downstream_id == target_id))

#filter matches to those that are between upstream and downstream site (drainage
#area ratio < 1), are in the same huc8, have a difference metric less than
#the average value of the nhd-connected pairings, have a downstream gage with a 
#defined stream order that is larger than the headwater order, and are not 
#already included in the nhd-connected pairings 
matches <- diffs %>%
  filter(drainage_ratio < 1,
         !is.na(huc8),
         diffmetric <= mean(connection_diffs$diffmetric),
         target_order >= 1,
         target_order >= goi_order) %>%
  select(headwater_id = goi_id, downstream_id = target_id, drainage_ratio, 
         hw_order = goi_order, ds_order = target_order, diffmetric, ranking) %>%
  left_join(select(connections, c(headwater_id, downstream_id, connection_id))) %>%
  filter(is.na(connection_id)) %>%
  select(!connection_id) %>%
  mutate(match_id = 1:nrow(.))
rm(diffs, huc8_matches)

# Filter downstream gages by additional criteria ----------------------------
#filters for data avilability and hydrologic modification are calculated in 
#exactly the same way as those for the headwaters set. Instead of filtering for
#stream order here though, we instead filter for the drainage ratio between the
#headwater and downstream site so they are still of somewhat comparable size.
save = T #save filtered gage and connections list as csv? Will overwrite previous

min_drainage_ratio <- 0.05

## Data Availability (max number of missing values within selected period)
start_year <- 1981
end_year <- 2023
max_nas <- 365*5
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
fil_matches <- filter(matches, drainage_ratio >= min_drainage_ratio)
fil_gages_ds <- all_gages %>%
  filter(site_no %in% fil_matches$downstream_id,
         ymd(end_date) >= ymd(paste0(end_year,'-09-30')),
         ymd(begin_date) <= ymd(paste0(start_year,'-10-01')),
         days_missing <= max_nas,
         storage_precip_ratio <= max_storage_ratio,
         dist_index <= max_dist_index) %>%
  filter(site_no %nin% ds_gage_info$site_no,
         site_no %nin% hw_gage_info$site_no)
fil_matches <- filter(fil_matches, downstream_id %in% fil_gages_ds$site_no) %>%
  mutate(match_id = 1:nrow(.))

hw_as_ds <- sum(fil_matches$downstream_id %in% hw_gage_info$site_no)
print(paste0('There are ',nrow(fil_matches),' total matches involving ',
             length(unique(fil_matches$headwater_id)),' unique headwater gages and ',
             nrow(fil_gages_ds), ' unique downstream gages. ',
             hw_as_ds, ' of these matches are between two headwater gages, leaving ',
             nrow(fil_matches)-hw_as_ds, ' connections between a headwater and a true downstream'))

hw_match_in_con <- sum(unique(fil_matches$headwater_id) %in% unique(connections$headwater_id))
unique_hw <- length(unique(fil_matches$headwater_id)) + length(unique(connections$headwater_id)) - hw_match_in_con
print(paste0(hw_match_in_con,' of the unique headwaters involved in matches are also ',
             'involved in direct connections, bringing us to ', unique_hw, 
             ' unique headwater gages with a connection OR a match.' ))

if(save) write_csv(fil_matches, paste0('./data/gages/hw_ds_matches.csv'))
if(save) write_csv(fil_gages_ds, paste0('./data/gages/ds_matched_gage_info.csv'))
