# common libraries and data ----------------------------------------------
library(tidyverse)
source('./scripts/functions/utility_functions.R')
source('./scripts/functions/read_gages.R')
source('./scripts/functions/read_spatial.R')
source('./scripts/Theme+Settings.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')

window_size <- 1
metrics <- read_csv(paste0('./data/gages/metrics/metrics_window',window_size,'.csv')) %>%
  select(site_no, wateryear, everything())
trends <- read_csv(paste0('./data/gages/metrics/trends_window',window_size,'.csv'))
metric_order <- colnames(select(metrics, !c(site_no, wateryear)))


# subsetting metrics from correlation matrix ------------------------------
library(corrplot)
cor_dat <- select(metrics, where(is.numeric)) %>% select(!wateryear)
metrics_cor <- cor(cor_dat, method = 'spearman', use = 'pairwise.complete')
corrplot(metrics_cor, method = 'number')

good_vars <- c(
  'Q_mean',
  'Q10',
  'BFI',
  'Q_frequency_high_2',
  'Q_meanduration_high_2',
  'Q_totalduration_noflow',
  'HFD_mean',
  'HFI_mean',
  'RLD',
  'FDC_slope',
  'Recession_a_Seasonality',
  'qp_elasticity'
)

metrics_sub <- select(metrics, site_no, wateryear, all_of(good_vars))
trends_sub <- filter(trends, var %in% good_vars)

metrics_cor_sub <- cor(select(cor_dat, all_of(good_vars)), method = 'spearman', use = 'pairwise.complete')
corrplot(metrics_cor_sub, method = 'number')


# number of significant trends --------------------------------------------
hw_gage_info_fil <- filter(hw_gage_info, order <= 3, dist_index <= 56)
hw_trends <- filter(trends, site_no %in% hw_gage_info_fil$site_no) %>%
  mutate(tau_trend = replace_na(tau_trend, 'none'))

tau_summary_by_metric <- as.data.frame(table(select(hw_trends, var, trend = tau_trend))) %>%
  mutate(var = factor(var, levels = metric_order),
         trend = factor(trend, levels = c('none','pos','neg')))
ggplot(tau_summary_by_metric, aes(x = var, y = Freq/nrow(hw_gage_info_fil), fill = trend)) +
  geom_col(color = 'black') +
  scale_fill_manual(values = c('grey90','blue2','red2'), name = 'MK trend') +
  scale_y_continuous(name = '% of headwater gages') +
  scale_x_discrete(name = NULL) +
  ggtitle(paste0('n = ', nrow(hw_gage_info_fil), ' headwater gages')) +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45))

tau_summary_by_gage <- group_by(hw_trends, site_no) %>%
  summarise(n_sig = length(tau_trend) - length(tau_trend[tau_trend == 'none'])) %>%
  left_join(hw_gage_info) %>%
  st_as_sf(coords = c('lon','lat'), crs = 4269)
ggplot(tau_summary_by_gage, aes(x = n_sig)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0,length(metric_order))) +
  labs(x = '# of metrics with significant trend', y = '# of headwater gages')
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = tau_summary_by_gage, aes(color = n_sig)) +
  scale_color_viridis_b(breaks = c(0,1,2,3,4,5,6,7), labels = c('0','1','2','3','4','5','6','7+'))


# upstream/downstream comparisons -----------------------------------------
connections <- read_gage_info(type = 'connections') %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, hw_order = order)) %>%
  left_join(., select(ds_gage_info, downstream_id = site_no, ds_order = order)) %>%
  mutate(across(c(hw_order, ds_order), ~ifelse(.x == -99, 0, .x))) %>%
  mutate(connection_id = 1:nrow(.))
connections_fil <- filter(connections, hw_order <= 3)

connections_trends <- tibble()
for(i in 1:length(metric_order)){
  metric = metric_order[i]
  hw_dat <- filter(trends, var == metric & site_no %in% hw_gage_info$site_no)
  ds_dat <- filter(trends, var == metric & site_no %in% ds_gage_info$site_no)
  connection_i <- left_join(connections_fil, hw_dat, by = join_by(headwater_id == site_no)) %>%
    left_join(., ds_dat, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds'))
  
  connections_trends <- rbind(connections_trends, connection_i)
}

connections_trends <- mutate(connections_trends, across(contains('tau_trend'), ~replace_na(.x, 'none'))) %>%
  mutate(tau_comp = case_when(
            tau_trend_hw == 'none' & tau_trend_ds == 'none' ~ 'None',
            tau_trend_hw == tau_trend_ds ~ 'Same',
            tau_trend_hw %in% c('neg', 'pos') & tau_trend_ds == 'none' ~ 'HW Sensitive',
            tau_trend_ds %in% c('neg', 'pos') & tau_trend_hw == 'none' ~ 'DS Sensitive',
            tau_trend_hw == 'pos' & tau_trend_ds == 'neg' ~ 'Opposite',
            tau_trend_hw == 'neg' & tau_trend_ds == 'pos' ~ 'Opposite'),
         sen_comp = (abs(sen_hw)-abs(sen_ds))/abs(sen_ds),
         median_comp =  median_diff_hw/median_diff_ds)
        
comp_by_metric <- as.data.frame(table(select(connections_trends, var_hw, tau_comp))) %>%
  filter(tau_comp != 'Opposite') %>%
  mutate(tau_comp = factor(tau_comp, levels = c('None', 'Same', 'DS Sensitive', 'HW Sensitive')),
         var_hw = factor(var_hw, levels = metric_order))

#sig difference bars
ggplot(comp_by_metric, aes(x = var_hw, y = Freq/nrow(connections_fil), fill = tau_comp)) +
  geom_col(color = 'black') +
  scale_fill_discrete(name = 'MK Trends') +
  labs(x = NULL, y = '% of HW-DS connections') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle(paste0('n = ',nrow(connections_fil),' HW-DS connections'))



comp_by_connection <- group_by(connections_trends, connection_id) %>%
  summarise(n_hw_sensitive = length(tau_comp[tau_comp == 'HW Sensitive'])) %>%
  left_join(connections) %>%
  left_join(hw_gage_info, by = join_by(headwater_id == site_no)) %>%
  mutate(order_dif = ds_order - hw_order) %>%
  st_as_sf(coords = c('lon','lat'), crs = 4269)
ggplot(comp_by_connection, aes(x = n_hw_sensitive)) +
  geom_bar()
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = comp_by_connection, aes(color = n_hw_sensitive)) +
  scale_color_viridis_b(breaks = c(0,1,2,3,4,5), labels = c('0','1','2','3','4','5+'))
