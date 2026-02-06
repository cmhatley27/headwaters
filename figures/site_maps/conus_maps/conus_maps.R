# libraries and data ------------------------------------------------------
library(tidyverse)
library(sf)
source('scripts/functions/utilities.R')
source('scripts/functions/load_states.R')
source('scripts/functions/load_gages.R')
source('scripts/Theme+Settings.R')

all_gage_info <- all_gage_info %>%
  st_as_sf(coords = c('lon','lat'), crs = 4269) %>%
  st_transform(5070)

all_connection_info <- rbind(
  {select(connections, c(headwater_id, downstream_id)) %>%
      mutate(type = 'connection')},
  {select(matches, c(headwater_id, downstream_id)) %>%
      mutate(type = 'match')}
)

# subset -----------------------------------------------------------------
plot_dat <- all_gage_info %>%
  filter(type == 'headwater',
         site_no %in% connections$headwater_id,
         order <= 3)
ggplot() +
  geom_sf(data = states, linewidth = 0.5) +
  geom_sf(data = plot_dat, size = 2) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = 'none') +
  ggtitle(paste0('Headwater gages with downstream connection, n = ', nrow(plot_dat)))
ggsave('figures/site_maps/conus_maps/headwaters_w_connection.png', height = 5, width = 8, units = 'in', dpi = 500)
table(table(all_connection_info$headwater_id))

# all --------------------------------------------------------------------
plot_dat <- all_gage_info
ggplot() +
  geom_sf(data = states, linewidth = 0.5) +
  geom_sf(data = plot_dat, aes(color = type, shape = type), size = 2) +
  scale_color_manual(limits = c('headwater', 'downstream', 'downstream_matched'),
                     values = c('black', 'royalblue', 'firebrick'), 
                     name = 'Gage Set') +
  scale_shape_manual(limits = c('headwater', 'downstream', 'downstream_matched'),
                     values = c(16, 17, 17), 
                     name = 'Gage Set') +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = 'none') +
  ggtitle(paste0('All gages, n = ', nrow(plot_dat)))
ggsave('figures/site_maps/conus_maps/all_sites_draft.png', height = 5, width = 8, units = 'in', dpi = 500)


# by region ---------------------------------------------------------------
ggplot() +
  geom_sf(data = states, linewidth = 0.5) +
  geom_sf(data = subset(all_gage_info, type == 'headwater'),
          aes(color = factor(aggregion), shape = type),
          size = 3) +
  scale_color_discrete(name = 'region')

table(hw_gage_info$aggregion)

region_recoder <- function(eco2){
  recodes <- c('nf' = 5.2,
               'nf' = 5.3,
               'mw' = 6.2,
               'mw' = 7.1,
               'ef' = 8.1,
               'cp' = 8.2,
               'ef' = 8.3,
               'ap' = 8.4,
               'ef' = 8.5,
               'gp' = 9.2,
               'gp' = 9.3,
               'gp' = 9.4,
               'gp' = 9.5,
               'sw' = 10.1,
               'sw' = 10.2,
               'sw' = 11.1,
               'sw' = 12.1,
               'sw' = 13.1)
  match_order <- match(eco2, recodes)
  recode <- names(recodes)[match_order]
  return(recode)
}

ggplot() +
  geom_sf(data = states, linewidth = 0.5) +
  geom_sf(data = subset(all_gage_info),
          aes(color = region_recoder(region2)),
          size = 2) +
  scale_color_discrete(name = 'region')
ggsave('figures/site_maps/conus_maps/ecoregions.png', height = 2.5, width = 4, units= 'in')

table(region_recoder(hw_gage_info$region2))
ggplot(subset(all_gage_info, site_no %in% c(connections$headwater_id)), aes(x = region_recoder(region2), fill = region_recoder(region2)))+
  geom_bar(color = 'black') +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_discrete(guide = NULL)
ggsave('figures/site_maps/conus_maps/ecoregions_count_conly.png', height = 1, width = 2, units= 'in')
