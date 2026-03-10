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

region_sel <- 'cp'
if(region_sel == 'all') region_sel <- unique(all_gage_info$region)

region_dat <- filter(all_gage_info, region %in% region_sel) %>%
  mutate(order_lumped = ifelse(order <= 3, 'hw', 'ds'))

ext <- st_crop(states, st_buffer(region_dat, 1e5))

ggplot() +
  geom_sf(data = ext) +
  geom_sf(data = region_dat, aes(shape = order_lumped)) +
  # scale_color_discrete(limits = c('hw', 'ds'), labels = c('Order 1-3', 'Order 4-6'), name = NULL) +
  scale_shape_manual(limits = c('hw', 'ds'), values = c(16, 17), labels = c('Order 1-3', 'Order 4-6'), name = NULL) +
  geom_sf(data = filter(region_dat, site_no %in% connections$headwater_id), color = 'red') +
  theme(legend.position = 'bottom')
ggsave('figures/site_maps/region_maps/all_connections.png', height = 4, width = 4, units = 'in')



# color points by data ----------------------------------------------------
point_dat <- left_join(connection_diffs_summary, region_dat) %>%
  left_join(., groups) %>%
  mutate(top_var_cat = labelinator(inv_labelinator(top_var, pred_labels), pred_cats),
         top_var_cat = factor(top_var_cat, levels = unique(names(pred_cats)))) %>%
  st_as_sf(.)

#color connections by top shap var
ggplot() +
  geom_sf(data = ext) +
  geom_sf(data = point_dat, aes(color = top_var, size = sig_diff)) +
  scale_size_discrete(name = 'Significant difference in Q95 trends?') +
  scale_color_discrete(name = 'Predictor with largest SHAP trend diff')
ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/connection_top_diffs_map.png'), height = 7.5, width = 10, units = 'in')

#color connections by top shap var CATEGORY
ggplot() +
  geom_sf(data = ext) +
  geom_sf(data = point_dat, aes(color = top_var_cat, size = sig_diff)) +
  scale_size_discrete(name = 'Significant difference in Q95 trends?') +
  scale_color_discrete(name = 'Predictor with largest SHAP trend diff')
ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/connection_top_diffs_cat_map.png'), height = 7.5, width = 10, units = 'in')
ggplot(point_dat, aes(x = top_var_cat, fill = top_var_cat, alpha = sig_diff)) +
  geom_bar(color = 'black') +
  scale_fill_discrete(guide = NULL) +
  scale_alpha_discrete(name = 'Significant difference in Q95 trends?') +
  xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom')
ggsave(paste0('figures/annual_model/',metric_sel,'/all/connection_top_diffs_cat_map.png'), height = 4, width = 3.5, units = 'in')

#color connections by cluster
ggplot() +
  geom_sf(data = ext) +
  geom_sf(data = point_dat, aes(color = factor(group), size = sig_diff)) +
  scale_size_discrete(name = 'Significant difference in Q95 trends?') +
  scale_color_discrete(name = 'SHAP trend cluster')
ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/connection_clusters_map.png'), height = 7.5, width = 10, units = 'in')
