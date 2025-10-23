library(tidyverse)
library(sf)
source('./scripts/functions/read_gages.R')
source('scripts/functions/read_spatial.R')
source('scripts/Theme+Settings.R')

states <- st_transform(states, 5070)
hw_gage_info <- read_gage_info(type = 'headwaters') %>%
  st_as_sf(., coords = c('lon','lat'), crs = 4269) %>%
  st_transform(., 5070)
ds_gage_info <- read_gage_info(type = 'downstream')

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = hw_gage_info, aes(color = drainage_area)) +
  scale_color_viridis_b()

ggplot(hw_gage_info, aes(x = drainage_area)) +
  geom_histogram(bins = 50)

