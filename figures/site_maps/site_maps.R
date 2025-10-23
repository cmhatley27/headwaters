# libraries and data ------------------------------------------------------
library(tidyverse)
library(sf)
source('scripts/functions/utilities.R')
source('scripts/functions/load_states.R')
source('scripts/Theme+Settings.R')

hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')
connections <- read_gage_info('connections')

all_gage_info <- rbind(
  mutate(hw_gage_info, type = 'headwater'),
  mutate(ds_gage_info, type = 'downstream')
) %>%
  st_as_sf(coords = c('lon','lat'), crs = 4269) %>%
  st_transform(5070)


# subset -----------------------------------------------------------------
plot_dat <- all_gage_info %>%
  filter(type == 'headwater' & site_no %in% connections$headwater_id & order <= 2)
ggplot() +
  geom_sf(data = states, linewidth = 0.5) +
  geom_sf(data = plot_dat, size = 2) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = 'none') +
  ggtitle(paste0('Headwater gages, order <= 2, with downstream pair, n = ', nrow(plot_dat)))
ggsave('figures/site_maps/headwaters_order2_w_downstream.png', height = 5, width = 8, units = 'in', dpi = 500)
table(table(connections$headwater_id))
# all --------------------------------------------------------------------
plot_dat <- all_gage_info
ggplot() +
  geom_sf(data = states, linewidth = 0.5) +
  geom_sf(data = plot_dat, aes(color = type, shape = type), size = 2) +
  scale_color_manual(limits = c('headwater', 'downstream'),
                     labels = c('Headwaters (n = 565)', 'Downstream (n = 128)'),
                     values = c('black', 'royalblue'), 
                     name = 'Gage Set') +
  scale_shape_manual(limits = c('headwater', 'downstream'),
                     labels = c('Headwaters (n = 565)', 'Downstream (n = 128)'),
                     values = c(16, 17), 
                     name = 'Gage Set') +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = 'none') +
  ggtitle(paste0('All gages, n = ', nrow(plot_dat)))
ggsave('figures/site_maps/all_sites_draft.png', height = 5, width = 8, units = 'in', dpi = 500)

