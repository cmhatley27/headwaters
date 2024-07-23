# Load libraries and data --------------------------------------------
library(tidyverse)
library(sf)

#find file path for evey downloaded gage
q_paths <- list.files(path = file.path('data', 'out', 'gage_q', '1970_2022'), 
                      full.names = TRUE)

#indicate that we want every file except for the last (gage info)
gages_select <- seq(1,length(q_paths)-1)


# # #Optional - select just a subset of gages for faster testing
# num_gages <- 25
# set.seed(52798)
# gages_select <- floor(runif(num_gages, 1, length(q_paths) - 1))
# 
# #Load spatial data to see which gages the subsetting picked up
# dir_states <- file.path('data','shapefiles','states','States_shapefile.shp')
# states <- st_read(dir_states) %>%
#   filter(State_Code != 'AK', State_Code != 'HI')

#pull in selected gages (will pull everything if the subsetting is not done)
qs <- map_df(q_paths[gages_select], read_csv, col_types = 'ccDdc')

gage_info <- read_csv(q_paths[length(q_paths)]) %>%
  filter(site_no %in% unique(qs$site_no))

#Show gages
ggplot() +
  geom_sf(data = states) +
  geom_point(data = gage_info, aes(x = dec_long_va, y = dec_lat_va))


plot_dat <- qs %>%
  pivot_longer(cols = c(q, bf), names_to = 'series', values_to = 'q')
year_sel <- 2000
ggplot(data = subset(plot_dat, wateryear == year_sel), aes(x = Date, y = q, color = series, fill = series)) +
  geom_area(position = 'identity') +
  scale_color_manual(breaks = c('q', 'bf'), values = c('black', 'red')) +
  scale_fill_manual(breaks = c('q', 'bf'), values = c(alpha('white',0), alpha('red', 0.5))) +
  facet_wrap(vars(site_no), scales = 'free_y')



