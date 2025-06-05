# load libraries and data -------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(climateR)
source('./scripts/functions/read_gages.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
gage_list <- unique(c(hw_gage_info$site_no, ds_gage_info$site_no))


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
end_date = '2022-09-30'
date_series <- seq.Date(from = ymd(start_date), to = ymd(end_date), by = '1 day')

for(i in 1:nrow(boundaries)){
  gage_id <- boundaries$GAGE_ID[i]
  save_path <- paste0('./data/gages/climate/',gage_id,'.csv')
  if(file.exists(save_path))  next
  
  dat <- getGridMET(boundaries[i,],
                    varname = c('pr','pet','tmmn','tmmx'),
                    startDate = '1981-10-01',
                    endDate = '2022-09-30')
  
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


# merge climate with discharge --------------------------------------------
for(i in 1:length(gage_list)){
  gage_id <- gage_list[i]
  q <- read_csv(paste0('./data/gages/q/',gage_id,'.csv'),
                col_types = cols(site_no = 'c'))
  climate <- read_csv(paste0('./data/gages/climate/',gage_id,'.csv'),
                      col_types = cols(site_no = 'c'))
  
  merge <- left_join(q, climate)
  write_csv(merge, paste0('./data/gages/merged/',gage_id,'.csv'))
  
  print(paste0('watershed ',i,'/',length(gage_list),' done!!!!!'))
}

