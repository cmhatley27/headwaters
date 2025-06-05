# libraries and data ------------------------------------------------------
library(tidyverse)
library(trend)
source('./scripts/functions/read_gages.R')
source('./scripts/Theme+Settings.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
connections <- read_gage_info(type = 'connections')
all_metrics <- read_csv('./data/metrics/all_metrics.csv') %>%
  select(site_no, wateryear, everything())
all_trends <- read_csv('./data/metrics/all_trends.csv')

metric_order <- colnames(select(all_metrics, !c(ends_with('error_str'), site_no, wateryear)))


# bar charts of trend significance ----------------------------------------
hw_trends <- filter(all_trends, site_no %in% hw_gage_info$site_no) %>%
  left_join(select(hw_gage_info, c(site_no, ref, order, drainage_area, precip, storage_precip_ratio, dist_index))) %>%
  mutate(var = factor(var, levels = metric_order),
         across(ends_with('trend'), ~factor(.x, levels = c('neg','pos','none'))))

table(select(hw_trends, c(var, tau_trend)))

ggplot(subset(hw_trends, tau_trend %in% c('pos','neg') & order <= 2), aes(x = var, fill = tau_trend)) +
  geom_bar(color = 'grey30',linewidth = 0.2, width = 0.7, position = position_stack()) +
  # scale_fill_manual(limits = c('neg','pos'),
  #                   values = c('firebrick','steelblue')) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# Plot spatial distribution of trends per metric --------------------------
source('./scripts/functions/read_spatial.R')

ds_trends <- filter(all_trends, site_no %in% ds_gage_info$site_no)

metrics <- unique(hw_trends$var)
for(i in 1:length(metrics)){
  metric = metrics[i]
  plot_dat <- ds_trends %>%
    filter(var == metric) %>%
    left_join(., ds_gage_info) %>%
    st_as_sf(coords = c('lon','lat'), crs = 4269) %>%
    arrange(tau_trend)
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = plot_dat, aes(color = tau_trend, size = tau_trend)) +
    scale_color_manual(values = c('black','red','blue'),
                       limits = c('none','neg','pos'),
                       labels = c('Non-significant','Negative','Positive'),
                       name = NULL, guide = NULL) +
    scale_size_manual(values = c(0.3,0.5,0.5), 
                      limits = c('none','neg','pos'),
                      labels = c('Non-significant','Negative','Positive'),
                      name = NULL, guide = NULL) +
    theme(legend.position = 'bottom',
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    ggtitle(paste0(metric, ' Downstream'))
  ggsave(paste0('./figures/trend_maps/downstream/',metric,'.png'), height = 2, width = 4, units = 'in')
}

# headwater downstream grid comparison -------------------------------------
connections <- read_gage_info(type = 'connections') %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, hw_order = order)) %>%
  left_join(., select(ds_gage_info, downstream_id = site_no, ds_order = order)) %>%
  mutate(across(c(hw_order, ds_order), ~ifelse(.x == -99, 0, .x)))

metrics <- unique(all_trends$var)
for(i in 1:length(metrics)){
  metric = metrics[i]
  hw_dat <- filter(all_trends, var == metric & site_no %in% hw_gage_info$site_no) %>%
    #reorder trend levels for plotting
    mutate(trend = factor(trend, levels = c('neg', 'none', 'pos'), labels = c('Negative', 'None', 'Positive')))
  ds_dat <- filter(all_trends, var == metric & site_no %in% ds_gage_info$site_no) %>%
    mutate(trend = factor(trend, levels = c('neg', 'none', 'pos'), labels = c('Negative', 'None', 'Positive')))
  trend_comp <- connections %>%
    left_join(., select(hw_dat, headwater_id = site_no, hw_trend = trend)) %>%
    left_join(., select(ds_dat, downstream_id = site_no, ds_trend = trend)) %>%
    mutate(cat = case_when(
      hw_trend == ds_trend ~ 'Same',
      hw_trend %in% c('Negative', 'Positive') & ds_trend == 'None' ~ 'HW Sensitive',
      ds_trend %in% c('Negative', 'Positive') & hw_trend == 'None' ~ 'DS Sensitive',
      hw_trend == 'Positive' & ds_trend == 'Negative' ~ 'Opposite',
      hw_trend == 'Negative' & ds_trend == 'Positive' ~ 'Opposite'),
      order_dif = ds_order - hw_order) %>%
    filter(!is.na(hw_trend) & !is.na(ds_trend))
  
  #grid plot
  ggplot(subset(trend_comp, hw_order <= 3), aes(x = hw_trend, y = ds_trend)) +
    geom_jitter(width = 0.3, height = 0.3) +
    geom_vline(xintercept = c(1.5,2.5)) +
    geom_hline(yintercept = c(1.5,2.5)) +
    scale_x_discrete(limits = c('Negative', 'None', 'Positive')) +
    scale_y_discrete(limits = c('Negative', 'None', 'Positive')) +
    labs(x = 'Headwater Trend', y = 'Downstream Trend', title = metric)
  ggsave(paste0('./figures/trend_squares/hw3/',metric,'.png'), height = 5, width = 6, units = 'in')
}


# hw ds summary -----------------------------------------------------------
connections <- read_gage_info(type = 'connections') %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, hw_order = order)) %>%
  left_join(., select(ds_gage_info, downstream_id = site_no, ds_order = order)) %>%
  mutate(across(c(hw_order, ds_order), ~ifelse(.x == -99, 0, .x))) %>%
  mutate(connection_id = 1:nrow(.))

category_summary <- tibble()

metrics <- unique(all_trends$var)
for(i in 1:length(metrics)){
  metric = metrics[i]
  hw_dat <- filter(all_trends, var == metric & site_no %in% hw_gage_info$site_no) %>%
    #reorder trend levels for plotting
    mutate(tau_trend = factor(tau_trend, levels = c('neg', 'none', 'pos'), labels = c('Negative', 'None', 'Positive')),
           mw_trend = factor(mw_trend, levels = c('neg', 'none', 'pos'), labels = c('Negative', 'None', 'Positive')),
           mw_diff = median_diff)
  ds_dat <- filter(all_trends, var == metric & site_no %in% ds_gage_info$site_no) %>%
    mutate(tau_trend = factor(tau_trend, levels = c('neg', 'none', 'pos'), labels = c('Negative', 'None', 'Positive')),
           mw_trend = factor(mw_trend, levels = c('neg', 'none', 'pos'), labels = c('Negative', 'None', 'Positive')),
           mw_diff = median_diff)
  trend_comp <- connections %>%
    left_join(., select(hw_dat, headwater_id = site_no, hw_tau_trend = tau_trend, hw_mw_trend = mw_trend, hw_diff = mw_diff)) %>%
    left_join(., select(ds_dat, downstream_id = site_no, ds_tau_trend = tau_trend, ds_mw_trend = mw_trend, ds_diff = mw_diff)) %>%
    mutate(cat = case_when(
      hw_tau_trend == ds_tau_trend ~ 'Same',
      hw_tau_trend %in% c('Negative', 'Positive') & ds_tau_trend == 'None' ~ 'HW Sensitive',
      ds_tau_trend %in% c('Negative', 'Positive') & hw_tau_trend == 'None' ~ 'DS Sensitive',
      hw_tau_trend == 'Positive' & ds_tau_trend == 'Negative' ~ 'Opposite',
      hw_tau_trend == 'Negative' & ds_tau_trend == 'Positive' ~ 'Opposite'),
      order_dif = ds_order - hw_order,
      metric = metric) %>%
    filter(!is.na(hw_tau_trend) & !is.na(ds_tau_trend))
  
  category_summary <- rbind(category_summary, trend_comp)
}
category_summary$metric <- factor(category_summary$metric, levels = metric_order)

ggplot(subset(category_summary, cat %in% c('HW Sensitive', 'DS Sensitive')), aes(x = metric, fill = cat)) +
  geom_bar(width = 0.7, linewidth = 0.2, color = 'grey30') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggplot(subset(category_summary, hw_mw_trend != 'None' | ds_mw_trend != 'None'), aes(x = hw_diff, y = ds_diff)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(vars(metric), scales = 'free')


