# load libraries ----------------------------------------------------------
library(sf)
library(terra)
library(tidyverse)
source('scripts/functions/utilities.R')

# load watershed boundaries -----------------------------------------------
hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
gage_list <- unique(c(hw_gage_info$site_no, ds_gage_info$site_no))

boundary_files <- list.files('./data/gagesii/boundaries-shapefiles-by-aggeco/',
                             pattern = '*.shp', full.names = T)

boundaries <- map(boundary_files, st_read) %>% list_rbind() %>%
  filter(GAGE_ID %in% gage_list) %>% 
  st_as_sf(crs = 5070)


# load all geology ----------------------------------------------------------
# download shapefile from https://www.sciencebase.gov/catalog/item/5888bf4fe4b05ccb964bab9d
geol <- vect('data/shapefiles/sgmc/SGMC_Geology.shp') %>%
  project(., 'EPSG:5070')

# loop through watersheds and calculate areas of each geologic unit ---------
for(i in 1:nrow(boundaries)){
  site <- boundaries$GAGE_ID[i]
  if(file.exists(paste0('data/gages/geology/',site,'.csv'))) next
  
  bnd <- (vect(boundaries[i,]))
  geol_mask <- crop(geol, bnd) %>% mask(., bnd)
  
  geol_areas <- tibble(
    unit = geol_mask$UNIT_LINK,
    area = expanse(geol_mask)
  ) %>%
    group_by(unit) %>%
    summarise(area = sum(area)) %>%
    mutate(area = round(area/sum(area), 4),
           site_no = site)
  
  write_csv(geol_areas, paste0('data/gages/geology/',site,'.csv'))
  print(paste0('watershed ',i,'/',nrow(boundaries),' done!!!'))
}
