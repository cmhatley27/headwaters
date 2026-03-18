# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/read_spatial.R')
source('./scripts/Theme+Settings.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
hw_gage_info_fil <- filter(hw_gage_info, order <= 3, dist_index <= 56)

ds_gage_info <- read_gage_info(type = 'downstream')

connections <- read_gage_info(type = 'connections') %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, hw_order = order)) %>%
  left_join(., select(ds_gage_info, downstream_id = site_no, ds_order = order)) %>%
  mutate(across(c(hw_order, ds_order), ~ifelse(.x == -99, 0, .x))) %>%
  mutate(connection_id = 1:nrow(.))
connections_fil <- filter(connections, headwater_id %in% hw_gage_info_fil$site_no)

window_size <- 3
trends <- read_csv(paste0('./data/gages/metrics/trends_window',window_size,'.csv'))

trends_fil <- filter(trends, site_no %in% hw_gage_info_fil$site_no)

metrics_sel <- c('Q_mean', 'Q95', 'Q10', 'TotalRR', 'Q_frequency_high_3', 'Q_frequency_noflow',
                'Q_totalduration_high_3', 'Q_totalduration_noflow',
                'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                'BaseflowRecessionK', 'Recession_a_Seasonality')

pred_trends <- read_csv(paste0('./data/gages/predictors/pred_trends_window',window_size,'.csv'))

connections_trends <- tibble()
for(i in 1:length(metrics_sel)){
  metric = metrics_sel[i]
  hw_dat <- filter(trends, var == metric & site_no %in% hw_gage_info$site_no)
  ds_dat <- filter(trends, var == metric & site_no %in% ds_gage_info$site_no)
  connection_i <- left_join(connections_fil, hw_dat, by = join_by(headwater_id == site_no)) %>%
    left_join(., ds_dat, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds'))
  
  connections_trends <- rbind(connections_trends, connection_i)
}

trend_diffs <- mutate(connections_trends, across(contains('tau_trend'), ~replace_na(.x, 'none'))) %>%
  mutate(tau_comp = case_when(
    tau_trend_hw == 'none' & tau_trend_ds == 'none' ~ 'None',
    tau_trend_hw == tau_trend_ds ~ 'Same',
    tau_trend_hw %in% c('neg', 'pos') & tau_trend_ds == 'none' ~ 'HW Sensitive',
    tau_trend_ds %in% c('neg', 'pos') & tau_trend_hw == 'none' ~ 'DS Sensitive',
    tau_trend_hw == 'pos' & tau_trend_ds == 'neg' ~ 'Opposite',
    tau_trend_hw == 'neg' & tau_trend_ds == 'pos' ~ 'Opposite'),
    sen_comp = case_when(
      sen_trend_hw == 'none' & sen_trend_ds == 'none' ~ 'None',
      sen_trend_hw == sen_trend_ds ~ 'Same',
      sen_trend_hw %in% c('neg', 'pos') & sen_trend_ds == 'none' ~ 'HW Sensitive',
      sen_trend_ds %in% c('neg', 'pos') & sen_trend_hw == 'none' ~ 'DS Sensitive',
      sen_trend_hw == 'pos' & sen_trend_ds == 'neg' ~ 'Opposite',
      sen_trend_hw == 'neg' & sen_trend_ds == 'pos' ~ 'Opposite'),
    sen_diff = abs(sen_hw)-abs(sen_ds))

pred_join <- pred_trends %>%
  select(site_no, var, sen) %>%
  pivot_wider(id_cols = site_no, names_from = 'var', values_from = 'sen')

plot_dat <- trend_diffs %>%
  filter(tau_comp %in% c('HW Sensitive', 'DS Sensitive', 'Same')) %>%
  select(headwater_id, downstream_id, metric = var_hw, tau_comp) %>%
  left_join(pred_join, by = join_by(headwater_id == site_no)) %>%
  left_join(pred_join, by = join_by(downstream_id == site_no), suffix = c('_hw', '_ds')) %>%
  pivot_longer(c(ends_with('_ds'), ends_with('_hw')), names_to = 'pred', values_to = 'sen') %>%
  mutate(gage = word(pred, -1, sep = '_'),
         pred = word(pred,1,-2, sep = '_')) %>%
  pivot_wider(names_from = 'gage', values_from = 'sen')


# plot --------------------------------------------------------------------
metric = 'Q_mean'
preds_sel <- c('precip_annual','temp_annual','pet_annual',
               'precip_jfm','temp_jfm','pet_jfm',
               'precip_amj','temp_amj','pet_amj',
               'precip_jas','temp_jas','pet_jas',
               'precip_ond','temp_ond','pet_ond',
               'ag','developed','forest','grass',
               'water_use')


ggplot(data = subset(plot_dat, metric == metric & pred %in% preds_sel), aes(x = ds, y = hw, color = tau_comp)) +
  geom_point() +
  geom_abline(slope = 1) +
  scale_color_manual(limits = c('HW Sensitive','DS Sensitive', 'Same'),
                     values = c('#F8766D','#619CCf', '#00BA38')) +
  facet_wrap(vars(pred), scales = 'free')






