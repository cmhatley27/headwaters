# load libraries and data -------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
source('./scripts/functions/read_gages.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
gage_list <- unique(c(hw_gage_info$site_no, ds_gage_info$site_no))

# calculate climate predictors --------------------------------------------
#annual
for(i in 1:length(gage_list)){
  gage_sel <- gage_list[i]
  gage_dat <- read_csv(paste0('./data/gages/climate/',gage_sel,'.csv'))
  
  annual_climate_preds <- gage_dat %>%
    mutate(wateryear = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>%
    group_by(wateryear) %>%
    summarise(precip_annual = sum(precip, na.rm = T),
              pet_annual = sum(pet, na.rm = T),
              temp_annual = mean(temp, na.rm = T),
              ppet_annual = precip_annual/pet_annual) %>%
    mutate(across(ends_with('annual'), ~lag(.x), .names = '{.col}_prev'),
           site_no = gage_sel)

  seasonal_climate_preds <- gage_dat %>%
    mutate(wateryear = ifelse(month(date) >= 10, year(date) + 1, year(date)),
           month = month(date),
           season = case_when(
             month %in% 10:12 ~ 'ond',
             month %in% 1:3 ~ 'jfm',
             month %in% 4:6 ~ 'amj',
             month %in% 7:9 ~ 'jas'
           )) %>%
    group_by(wateryear, season) %>%
    summarise(precip = sum(precip, na.rm = T),
              pet = sum(pet, na.rm = T),
              temp = mean(temp, na.rm = T),
              ppet = precip/pet) %>%
    pivot_wider(names_from = season, values_from = !c(wateryear,season)) %>%
    ungroup() %>%
    mutate(across(!wateryear, ~lag(.x), .names = '{.col}_prev'),
           site_no = gage_sel)
  
  climate_preds <- left_join(annual_climate_preds, seasonal_climate_preds) %>%
    select(site_no, wateryear, everything())
  
  write_csv(climate_preds, paste0('./data/gages/predictors/climate/',gage_sel,'.csv'))
  print(paste0('gage ',i,'/',length(gage_list),' done!!!'))
}


# Extract land cover ----------------------------------------------------
#load watershed boundaries
boundary_files <- list.files('./data/gagesii/boundaries-shapefiles-by-aggeco/',
                             pattern = '*.shp', full.names = T)

boundaries <- map(boundary_files, st_read) %>% list_rbind() %>%
  filter(GAGE_ID %in% gage_list) %>% 
  st_as_sf(crs = 5070) %>%
  st_transform(crs = 4269)

#load annual NLCD rasters
lcs <- list.files('./data/rasters/nlcd',
                  pattern = '*.tif', full.names = T)
lc <- rast(lcs)
#get and save the color table for future plotting
coltable <- coltab(lc)[[1]]
write_csv(coltable, './data/rasters/nlcd_coltable.csv')

#loop through watershed boundaries, clip the raster to them, and save
for(i in 1:nrow(boundaries)){
  bnd <- vect(boundaries[i,]) %>% project(crs(lc))
  lc_mask <- crop(lc, bnd) %>% mask(., bnd) %>% project('epsg:4269')
  writeRaster(lc_mask, 
              paste0('./data/gages/land_cover/',boundaries[i,]$GAGE_ID,'.tif'),
              overwrite = T)
  print(paste0('watershed ',i,'/',nrow(boundaries),' done!!!'))
}


# Calculate land cover variables ------------------------------------------
#lump land covers into categories using codes from:
#https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
lc_cats <- list(
  water = c(11), #open water only
  ag = c(81,82), #cultivated crop + pasture/hay
  forest = c(41,42,43), #deciduous, evergreen, and mixed forest
  grass = c(71), #grassland only
  barren = c(31), #barren only
  developed = c(21,22,23,24) #developed open space + low-high intensity developed
)

for(i in 1:length(gage_list)){
  #load lc raster for a gage and get basic info
  gage_sel <- gage_list[i]
  lc <- rast(paste0('./data/gages/land_cover/',gage_sel,'.tif'))
  total_cells <- freq(is.na(lc[[1]]))$count[1]
  years <- word(names(lc), 4, sep = '_')
  
  #calculate percentage of land cover categories for each year available
  cat_pct <- tibble()
  for(j in 1:length(years)){
    freqs <- freq(lc[[j]])
    cat_pct[j,1:7] <- freqs %>%
      summarise(water = sum(count[value %in% lc_cats[['water']]]),
                ag = sum(count[value %in% lc_cats[['ag']]]),
                forest = sum(count[value %in% lc_cats[['forest']]]),
                grass = sum(count[value %in% lc_cats[['grass']]]),
                barren = sum(count[value %in% lc_cats[['barren']]]),
                developed = sum(count[value %in% lc_cats[['developed']]])) %>%
      mutate(across(everything(), ~.x/total_cells*100),
             year = years[j])
  }
  #add site number and save
  cat_pct$site_no <- gage_sel
  write_csv(cat_pct, paste0('./data/gages/predictors/land_cover/',gage_sel,'.csv'))
  
  print(paste0('gage ',i,'/',length(gage_list),' done!!!'))
}

