# load libraries and data -------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(climateR)
source('./scripts/functions/utilities.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
ds_match_gage_info <- read_gage_info('downstream_matched')
gage_list <- unique(c(hw_gage_info$site_no, ds_gage_info$site_no, ds_match_gage_info$site_no))


# load watershed boundaries -----------------------------------------------
boundary_files <- list.files('./data/gagesii/boundaries-shapefiles-by-aggeco/',
                             pattern = '*.shp', full.names = T)

boundaries <- map(boundary_files, st_read) %>% list_rbind() %>%
  filter(GAGE_ID %in% gage_list) %>% 
  st_as_sf(crs = 5070) %>%
  st_transform(crs = 4269)


# download climate data and summarize to daily mean -----------------------

#function to convert the output of getGridMET to the correct crs, mask the 
#grid cells that are not within the watershed boundary, and then calculate the
#daily mean value
meaninator <- function(x,i){
  crs(x) <- crs('epsg:4269')
  x_clip <- mask(x, boundaries[i,])
  return(global(x_clip, 'mean', na.rm = T))
}

start_date = '1981-10-01'
end_date = '2023-09-30'
date_series <- seq.Date(from = ymd(start_date), to = ymd(end_date), by = '1 day')

for(i in 1:nrow(boundaries)){
  gage_id <- boundaries$GAGE_ID[i]
  save_path <- paste0('./data/gages/climate/',gage_id,'.csv')
  if(file.exists(save_path))  next
  
  dat <- getGridMET(boundaries[i,],
                    varname = c('pr','pet','tmmn','tmmx'),
                    startDate = start_date,
                    endDate = end_date)
  
  dat_daily <- map(dat, ~meaninator(.x, i))
  
  tmean <- {(dat_daily[['daily_minimum_temperature']][['mean']] + 
             dat_daily[['daily_maximum_temperature']][['mean']])/2 - 273.15}
  
  output <- data.frame(
    date = date_series,
    site_no = gage_id,
    precip = dat_daily[['precipitation_amount']][['mean']],
    pet = dat_daily[['daily_mean_reference_evapotranspiration_grass']][['mean']],
    temp = tmean
  ) %>%
    mutate(across(c(precip,pet,temp), ~round(.x,2)))
  
  write_csv(output, save_path)
  
  print(paste0('watershed ',i,'/',nrow(boundaries),' done!!!!!'))
}


# fix some weird outliers -------------------------------------------------
#03079000 temp
bad_temp <- read_csv('./data/gages/climate/03079000.csv') %>%
  mutate(dtemp = temp-lag(temp))
#at 1999-07-02 all temperatures suddenly drop by 32 degrees
shift_date <- bad_temp$date[which.min(bad_temp$dtemp)]
before_mean <- mean(bad_temp$temp[bad_temp$date < shift_date])
after_mean <- mean(bad_temp$temp[bad_temp$date >= shift_date])
#fix by shifting the bad temp up and rescaling
temp_fix <- bad_temp %>%
  mutate(new_temp = ifelse(date >= shift_date, (temp-after_mean)*9/5+before_mean,temp))
ggplot(temp_fix, aes(x = date, y = temp)) +
  geom_point(color = 'red') +
  geom_point(aes(y = new_temp))
temp_fix_save <- select(temp_fix, date, site_no, precip, pet, temp = new_temp)
write_csv(temp_fix_save, './data/gages/climate/03079000.csv')

#03360500 and 08202700 pet
#These just go to 0 for some reason midway through the period so going to 
#set the whole series to NA so that they're not used later on
bad_pet1 <- read_csv('./data/gages/climate/03360500.csv') %>%
  mutate(pet = NA)
write_csv(bad_pet1, './data/gages/climate/03360500.csv')
bad_pet2 <- read_csv('./data/gages/climate/08202700.csv') %>%
  mutate(pet = NA)
write_csv(bad_pet2, './data/gages/climate/08202700.csv')

