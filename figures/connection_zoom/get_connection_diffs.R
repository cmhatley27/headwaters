library(tidyverse)
library(plotly)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  mutate(type = 'metric')
pred_trends <- read_csv('data/gages/predictors/pred_trends.csv') %>%
  mutate(type = 'pred')
pred_statics <- read_csv('data/gages/predictors/pred_statics.csv')

trends <- rbind(metric_trends, pred_trends) %>%
  select(site_no, var, val = sen) %>%
  rbind(., pred_statics) %>%
  mutate(set = ifelse(site_no %in% hw_gage_info$site_no, 'headwater', 'downstream'))

dat <- connections %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, lat, lon, region1, region2, region3)) %>%
  left_join(., select(trends, headwater_id = site_no, var, hw_val = val)) %>%
  left_join(., select(trends, downstream_id = site_no, var, ds_val = val)) %>%
  mutate(diff = abs(hw_val) - abs(ds_val),
         region = region_recoder(region2)) %>%
  mutate(diff_z = scale(diff, center = F)[,1], .by = var)

write_csv(dat, 'figures/connection_zoom/cn_diffs.csv')
