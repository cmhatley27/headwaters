# data --------------------------------------------------------------------
library(tidyverse)
library(sf)
source('scripts/Theme+Settings.R')
source('scripts/functions/load_states.R')

hw_gage_info <- read_gage_info() %>% 
  mutate(type = 'hw')
ds_gage_info <- read_gage_info('downstream') %>%
  filter(site_no %nin% hw_gage_info$site_no) %>% 
  mutate(type = 'ds')
gage_info <- rbind(hw_gage_info, ds_gage_info) %>%
  select(site_no, lat, lon, name = station_nm)

model_name <- 'hw_order2_senval'
fig_dir <- paste0('figures/model_output/performance/',model_name,'/map/')
if(!dir.exists(fig_dir)) dir.create(fig_dir)

performance_summary <- read_csv(paste0('data/models/performance/',model_name,'_summary.csv'))
predictions <- read_csv(paste0('data/models/performance/',model_name,'_predictions.csv')) %>%
  mutate(rse = sqrt((pred - obs)^2),
         error = rse/abs(obs)) %>%
  left_join(gage_info)

metrics_sel = performance_summary$var

metric_sel = 'Q5'#metrics_sel[i]

plot_dat <- filter(predictions, var == metric_sel) %>%
  arrange(rse) %>%
  st_as_sf(., coords = c('lon', 'lat'), crs = 4269) %>% 
  st_transform(., 5070)
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = plot_dat, aes(color = rse)) +
  scale_color_viridis_b()
