# load libraries and data -------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
source('./scripts/functions/utilities.R')

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


# extract SWE for each watershed and summarize to spatial daily mean -------
# SWE data downloaded individually for each water year from:
# https://nsidc.org/data/nsidc-0719/versions/1

swes <- list.files('./data/rasters/swe',
                  pattern = '*.nc', full.names = T)
swe <- rast(swes)["SWE"]

date_vec <- seq.Date(ymd('1981-10-01'), ymd('2023-09-30'), by = 'day')
names(swe) <- date_vec

for(i in 1:nrow(boundaries)){
  site <- boundaries$GAGE_ID[i]
  if(file.exists(paste0('data/gages/swe/',site,'.csv'))) next
  bnd <- vect(boundaries[i,])
  swe_mask <- crop(swe, bnd) %>% mask(., bnd)
  swe_mean <- global(swe_mask, 'mean', na.rm = T) %>%
    mutate(date = rownames(.), 
           swe = mean,
           site_no = site,
           .keep = 'none')
  
  write_csv(swe_mean, paste0('data/gages/swe/',site,'.csv'))
  print(paste0('watershed ',i,'/',nrow(boundaries),' done!!!'))
}
