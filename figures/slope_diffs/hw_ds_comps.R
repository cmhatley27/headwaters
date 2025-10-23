# libraries and data ------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_states.R')
source('scripts/Theme+Settings.R')
library(tmap)
tmaptools::palette_explorer()

hw_gage_info <- read_gage_info(type = 'headwaters')
hw_gage_info_fil <- filter(hw_gage_info, order <= 3, dist_index <= 56)

ds_gage_info <- read_gage_info(type = 'downstream')

connections <- read_gage_info(type = 'connections') %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, hw_order = order)) %>%
  left_join(., select(ds_gage_info, downstream_id = site_no, ds_order = order)) %>%
  mutate(across(c(hw_order, ds_order), ~ifelse(.x == -99, 0, .x))) %>%
  mutate(connection_id = 1:nrow(.))
connections_fil <- filter(connections, hw_order %in% 1:2)

window_size <- 3
trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_window',window_size,'.csv'))

trends_fil <- filter(trends, site_no %in% hw_gage_info_fil$site_no)

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR',
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 
                 'BFI', 'FlashinessIndex', 'FDC_slope', 'BaseflowRecessionK', 'Recession_a_Seasonality')

# calc trend diffs --------------------------------------------------------
connections_trends <- tibble()
for(i in 1:length(metrics_sel)){
  metric = metrics_sel[i]
  hw_dat <- filter(trends, var == metric & site_no %in% hw_gage_info$site_no)
  ds_dat <- filter(trends, var == metric & site_no %in% ds_gage_info$site_no)
  connection_i <- left_join(connections, hw_dat, by = join_by(headwater_id == site_no)) %>%
    left_join(., ds_dat, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds'))
  
  connections_trends <- rbind(connections_trends, connection_i)
}

trend_diffs <- mutate(connections_trends, across(contains('sen_sig'), ~replace_na(.x, 'none'))) %>%
  mutate(
    sen_comp = case_when(
      sen_sig_hw == 'none' & sen_sig_ds == 'none' ~ 'None',
      sen_sig_hw == sen_sig_ds ~ 'Same',
      sen_sig_hw %in% c('neg', 'pos') & sen_sig_ds == 'none' ~ 'HW Sensitive',
      sen_sig_ds %in% c('neg', 'pos') & sen_sig_hw == 'none' ~ 'DS Sensitive',
      sen_sig_hw == 'pos' & sen_sig_ds == 'neg' ~ 'Opposite',
      sen_sig_hw == 'neg' & sen_sig_ds == 'pos' ~ 'Opposite'),
    trend_diff = abs(sen_hw-sen_ds))



# scatter plots -----------------------------------------------------------

for(i in 1:length(metrics_sel)){
  metric_sel = metrics_sel[i]
  
  lm_summary <- summary(lm(trend_diff~drainage_ratio, data = subset(trend_diffs, drainage_ratio <= 1 & var_hw == metric_sel)))
  ggplot(subset(trend_diffs, drainage_ratio <= 1 & var_hw == metric_sel), aes(x = drainage_ratio, y = trend_diff)) +
    geom_point() +
    geom_smooth(method = 'lm', se = F) +
    scale_y_continuous(name = '|HW Sen\'s - DS Sen\'s|') +
    scale_x_continuous(name = 'Drainage Area Ratio (HW/DS)') +
    theme(text = element_text(size = 14)) +
    ggtitle(paste0(metric_sel, ', LM p = ',round(lm_summary$coefficients[2,4], 3)))
  ggsave(paste0('figures/slope_diffs/v_area/',metric_sel,'_v_area.png'), height = 4, width = 5, units = 'in')
}


# swarm plots -------------------------------------------------------------
library(ggbeeswarm)

swarm_dat <- select(trend_diffs, connection_id, var = var_hw, hw = sen_hw, ds = sen_ds) %>%
  pivot_longer(c(hw, ds), names_to = 'set', values_to = 'sen') %>%
  mutate(set = factor(set, levels = c('hw', 'ds'))) %>%
  group_by(var, set) %>%
  filter(sen <= mean(sen, na.rm = T) + 3*(var(sen, na.rm = T)^0.5) & 
           sen >= mean(sen, na.rm = T) - 3*(var(sen, na.rm = T)^0.5))
orders <- select(trend_diffs, connection_id, var = var_hw, hw = hw_order, ds = ds_order) %>%
  pivot_longer(c(hw, ds), names_to = 'set', values_to = 'order')
swarm_dat <- left_join(swarm_dat, orders) %>%
  mutate(alt_set = ifelse(order <= 2, 'hw', 'ds'))

for(i in 1:length(metrics_sel)){
  metric_sel = metrics_sel[i]
  
  #HW v DS
  ggplot(subset(swarm_dat, var == metric_sel), aes(x = set, y = sen, fill = set, color = set)) +
    geom_hline(yintercept = 0) + 
    # geom_violin() +
    geom_quasirandom(width = 0.15) +
    scale_x_discrete(labels = c('Headwaters', 'Downstream'), name = NULL) +
    ylab('Sen\'s Slope') +
    scale_fill_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    scale_color_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    theme(legend.position = 'none',
          text = element_text(size = 14)) +
    ggtitle(paste0(metric_sel,' Trends'))
  
  #HW v DS Abs
  abs_dat <- filter(swarm_dat, var == metric_sel) %>%
    mutate(abs_sen = abs(sen)) %>%
    mutate(set = factor(set, levels = c('hw', 'ds')),
           order = factor(order))
  ks <- ks.test(abs_dat$abs_sen[abs_dat$set == 'hw'], abs_dat$abs_sen[abs_dat$set == 'ds'])
    
  ggplot(abs_dat, aes(x = set, y = abs_sen, fill = set, color = set)) +
    # geom_violin(width = 0.25) +
    geom_quasirandom(width = 0.2) +
    geom_boxplot(width = 0.5, fill = NA, color = 'black', outliers = F) +
    scale_x_discrete(labels = c('Headwaters', 'Downstream'), name = NULL) +
    ylab('|Sen\'s Slope|') +
    scale_fill_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    scale_color_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    theme(legend.position = 'none',
          text = element_text(size = 14)) +
    ggtitle(paste0(metric_sel,' Abs Trends, KS p = ', round(ks$p.value, 3)))
  ggsave(paste0('figures/slope_diffs/hw_ds_abs/',metric_sel,'_hw_ds_abs.png'), height = 4, width = 5, units = 'in')
  
  #Abs by order
  ggplot(subset(abs_dat, order %in% 1:6), aes(x = order, y = abs_sen, fill = order, color = order)) +
    # geom_violin(width = 0.25) +
    # geom_quasirandom(width = 0.25) +
    geom_boxplot(width = 0.5, color = 'black') +
    xlab('Stream Order') +
    # scale_x_discrete(labels = c('Headwaters', 'Downstream'), name = NULL) +
    ylab('|Sen\'s Slope|') +
    # scale_fill_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    # scale_color_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    theme(legend.position = 'none',
          text = element_text(size = 14)) +
    ggtitle(paste0(metric_sel,' Abs Trends'))
  ggsave(paste0('figures/slope_diffs/order_abs/',metric_sel,'_order_abs.png'), height = 4, width = 5, units = 'in')

  #Alternate HW v DS Abs
  abs_dat <- filter(swarm_dat, var == metric_sel) %>%
    filter(connection_id %in% connections$connection_id[connections$hw_order %in% 1:2]) %>%
    mutate(abs_sen = abs(sen)) %>%
    mutate(set = factor(set, levels = c('hw', 'ds')),
           order = factor(order))
  ks <- ks.test(abs_dat$abs_sen[abs_dat$set == 'hw'], abs_dat$abs_sen[abs_dat$set == 'ds'])
  
  ggplot(abs_dat, aes(x = set, y = abs_sen, fill = set, color = set)) +
    # geom_violin(width = 0.25) +
    geom_quasirandom(width = 0.2) +
    geom_boxplot(width = 0.5, fill = NA, color = 'black', outliers = F) +
    scale_x_discrete(labels = c('Headwaters', 'Downstream'), name = NULL) +
    ylab('|Sen\'s Slope|') +
    scale_fill_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    scale_color_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    theme(legend.position = 'none',
          text = element_text(size = 14)) +
    ggtitle(paste0(metric_sel,' Abs Trends, KS p = ', round(ks$p.value, 3)))
  ggsave(paste0('figures/slope_diffs/hw_ds_order2_abs/',metric_sel,'_hw_ds_order2_abs.png'), height = 4, width = 5, units = 'in')
  
  #Alternative abs by Order
  ggplot(subset(abs_dat, order %in% 1:6), aes(x = order, y = abs_sen, fill = order, color = order)) +
    # geom_violin(width = 0.25) +
    # geom_quasirandom(width = 0.25) +
    geom_boxplot(width = 0.5, color = 'black') +
    xlab('Stream Order') +
    # scale_x_discrete(labels = c('Headwaters', 'Downstream'), name = NULL) +
    ylab('|Sen\'s Slope|') +
    # scale_fill_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    # scale_color_manual(values = c('#F8766D','#619CCf'), guide = 'none') +
    theme(legend.position = 'none',
          text = element_text(size = 14)) +
    ggtitle(paste0(metric_sel,' Abs Trends'))
  ggsave(paste0('figures/slope_diffs/order_order2_abs/',metric_sel,'_order_order2_abs.png'), height = 4, width = 5, units = 'in')
}
