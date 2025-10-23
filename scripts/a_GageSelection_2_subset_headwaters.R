# Load libraries and data -------------------------------------------------
library(tidyverse)
source('./scripts/Theme+Settings.R')
all_gages <- read_csv('./data/gagesii/all_gages_summary.csv')

# Filter gagesii by selected criteria -------------------------------------
save = T #save filtered gage list as csv? will overwrite previous one.

## Stream Order
stream_orders_sel <- 1:3

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
fil_gages <- all_gages %>%
  filter(order %in% stream_orders_sel,
         ymd(end_date) >= ymd(paste0(end_year,'-09-30')),
         ymd(begin_date) <= ymd(paste0(start_year,'-10-01')),
         days_missing <= max_nas,
         storage_precip_ratio <= max_storage_ratio,
         dist_index <= max_dist_index)
print(paste0('Filtered gage list contains ',
             nrow(fil_gages),'/', nrow(all_gages),' gages'))

if(save) write_csv(fil_gages, paste0('./data/gages/hw_gage_info.csv'))

# map filtered gages ------------------------------------------------------
library(sf)
source('scripts/functions/load_states.R')
gages_sf <- fil_gages %>%
  st_as_sf(., coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(5070)

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = gages_sf) +
  ggtitle(paste0('n = ',nrow(gages_sf),' filtered gages'))

# Explore filter values ---------------------------------------------------
# plots and stuff to help in selecting appropriate filter values

# stream order histogram --------------------------------------------------
ggplot(all_gages, aes(x = order)) +
  geom_bar() + xlim(c(0,20))


# data availability -------------------------------------------------------
#set time period of interest
start_year <- 1981
end_year <- 2022
start_date_sel <- paste0(start_year,'-10-01')
end_date_sel <- paste0(end_year,'-09-30')
dur <- as.numeric(ymd(end_date_sel)-ymd(start_date_sel)) + 1

stream_order_fil <- 3 #optional filter for stream order. set to like 50 if unwanted

#function to calculate number of NA values in selected period
get_nas_in_period <- function(period_start, period_end, record_start, record_end, num_obs){
  period_duration = as.numeric(period_end - period_start + 1)
  days_out_of_period = (as.numeric(record_end - period_end)) + (as.numeric(period_start - record_start))
  days_in_period = num_obs - days_out_of_period
  days_missing = period_duration - days_in_period
  return(days_missing)
}

#filter gage list by selected period and count number of NAs
gages_availability <- all_gages %>%
  filter(order <= stream_order_fil) %>%
  filter(ymd(end_date) >= ymd(paste0(end_year,'-09-30')),
         ymd(begin_date) <= ymd(paste0(start_year,'-10-01')))

gages_availability$days_missing <- get_nas_in_period(ymd(start_date_sel), ymd(end_date_sel),
                                                     gages_availability$begin_date, gages_availability$end_date, gages_availability$obs_count)

#calculate cumulative number of gages with at most X number of NAs to decide
#the number of NAs we shold tolerate in the final gage list
max_nas <- 0:(365*(end_year-start_year))

nas_summary <- tibble(
  missing_days = max_nas,
  n = NA
)
for(i in seq_along(max_nas)){
  nas_summary$n[i] <- nrow(filter(gages_availability, days_missing <= max_nas[i]))
}
#Function levels out at around ~3 years of NAs so using this as the maximum
#number of NAs that we tolerate
ggplot(data = nas_summary) +
  geom_line(aes(x = missing_days, y = n)) +
  scale_x_continuous(breaks = seq(0,max(max_nas), by = 365), labels = seq(0,max(max_nas)/365), minor_breaks = NULL) +
  geom_vline(xintercept = 365*3, color = 'red') +
  xlab('Max Missing Values (yrs)') +
  ggtitle(paste0('# of gages with at most X missing values between ',start_year,'-',end_year,' (',floor(dur/365),' years)'))


# hydrologic modifications -----------------------------------------------
# uses 'gages_availability' data frame from the above section, so run that first
# (including stream order filter if wanted)

max_nas <- 365*3 #optional filter for data availability. set to something really high if unwanted

gages_modifications <- gages_availability %>%
  filter(days_missing <= max_nas)

#most gages have minimal dam influence but some are higher. 0.1 seems like a decent
#cutoff
ggplot(gages_modifications, aes(x = storage_precip_ratio)) +
  geom_histogram(binwidth = 0.05) +
  xlim(-0.03,2) +
  geom_vline(xintercept = 0.1, color = 'red')

#disturbance index is another measure for modifications which might impact
#the streamflow metrics that we analyze. Ranges from 0 to 56
ggplot(gages_modifications, aes(x = dist_index)) +
  geom_histogram()

#applying the 0.1 storage ratio filter cuts out many of the gages with high disturbance indices
ggplot(subset(gages_modifications, storage_precip_ratio >= 0.1), aes(x = dist_index)) +
  geom_histogram()

#reading column 'wr_comments' for the gages that remain after applying the storage
#ratio filter indicates that additionally cutting out gages with a disturbance index
# > 20 would take care of most of the gages with significant flow modification
#via reservoirs/diversions
view(subset(gages_modifications, storage_precip_ratio <= 0.1))


# check temporal distribution of NAs --------------------------------------
#check filtered gages with at least X NAs for how the NAs are spread out. If lots of
#gages all have extended periods of NA at the same time that will be a problem.

#start year and max na amount from filters
start_year <- 1981
max_nas <- 365*3

#min number of NAs at a gage to trigger a check here
min_nas <- 5
gages_to_check <- fil_gages$site_no[gil_gages$days_missing >= min_nas]

#download daily data for all selected gages. Skip this section if already downloaded
gages_missing <- tibble()
count <- 0
for(gage in gages_to_check){
  gage_q <- readNWISdv(
    siteNumbers = gage,
    parameterCd = '00060',
    startDate = start_date_sel, endDate = end_date_sel
  ) %>%
    select(c(site_no, Date, X_00060_00003))
  gages_missing <- rbind(gages_missing, gage_q)
  rm(gage_q)

  count <- count + 1
  print(paste0(count,'/',length(gages_to_check),' done!!'))
}

#transform downloaded gage data for plotting
gages_missing <- gages_missing %>%
  select(site_no, date = Date, q = X_00060_00003) %>%
  pivot_wider(id_cols = date, names_from = site_no, values_from = q) %>%
  pivot_longer(!date, names_to = 'site_no', values_to = 'q') %>%
  left_join(.,select(fil_gages, site_no, days_missing)) %>%
  arrange(days_missing)

gages_missing_fil <- filter(gages_missing, is.na(q) == TRUE) %>%
  mutate(site_no = factor(site_no, levels = unique(site_no)),
         site_no_num = match(site_no, levels(site_no)),
         is_na = is.na(q))

#plot all NAs. There are a few overlapping NA periods among sites but overall
#nothing too concerning.
ggplot(data = gages_missing_fil) +
  geom_hline(aes(yintercept = site_no_num), linetype = 'solid', color = 'grey80') +
  geom_point(aes(x = date, y = site_no_num, color = site_no), size = 1.7) +
  scale_color_discrete(guide = 'none') +
  ggtitle(paste0('Timing of NAs for each gage with >= 5 NA days, max ',max_nas/365,' years of NA'))

