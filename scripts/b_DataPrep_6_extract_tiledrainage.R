# load libraries ----------------------------------------------------------
library(sf)
library(terra)
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

# load agtile and watershed boundaries -----------------------------------------------
agtile <- rast('data/rasters/agtile/AgTile-US.tif')

gage_list <- all_gage_info$site_no

boundary_files <- list.files('./data/gagesii/boundaries-shapefiles-by-aggeco/',
                             pattern = '*.shp', full.names = T)

boundaries <- map(boundary_files, st_read) %>% list_rbind() %>%
  filter(GAGE_ID %in% gage_list) %>% 
  st_as_sf(crs = 5070) %>%
  st_transform(., crs(agtile))


# loop through watersheds and extract tile drainage -----------------------
tile_pct <- tibble(site_no = gage_list,
                   tile_pct = NA)
for(i in 1:nrow(boundaries)){
  site <- boundaries$GAGE_ID[i]
  
  bnd <- (vect(boundaries[i,]))
  tile_mask <- crop(agtile, bnd) %>% mask(., bnd)
  
  tile_amt <- global(tile_mask, 'mean', na.rm = T)[1,1]
  tile_pct$tile_pct[tile_pct$site_no == site] <- tile_amt
  
  print(paste0('watershed ',i,'/',nrow(boundaries),' done!!!'))
}
write_csv(tile_pct, paste0('data/gages/tile_drainage/tile_pct.csv'))
