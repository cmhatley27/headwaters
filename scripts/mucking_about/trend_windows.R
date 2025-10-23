# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/Theme+Settings.R')
source('scripts/functions/utilities.R')

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR',
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 
                 'BFI', 'FlashinessIndex', 'FDC_slope', 'BaseflowRecessionK', 'Recession_a_Seasonality')

metrics <- read_csv('data/gages/metrics/merged/metrics_window3.csv')  %>%
  select(site_no, wateryear, !contains('error_str')) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'metric', values_to = 'val') %>%
  filter(metric %in% metrics_sel)


# calculate trends on sliding 30-year window ------------------------------
alpha = 0.1

start_years <- unique(metrics$wateryear)[1:(length(unique(metrics$wateryear))-29)]

trends <- data.frame()
for(i in 1:length(start_years)){
  start_year = start_years[i]
  end_year = start_year + 29
  
  metrics_fil <- filter(metrics, wateryear %in% start_year:end_year)
  
  trends_i <- select(metrics_fil, !c(wateryear)) %>%
    group_by(site_no, metric) %>%
    summarise(across(val, ~trendinator(.x, length_thresh = 5), .unpack = '{inner}')) %>%
    mutate(across(c(sen, tau), ~trend_classifier(.x, p, alpha = alpha*2), .names = '{.col}_sig2a')) %>%
    mutate(start_year = start_year)
  
  trends <- rbind(trends, trends_i)
  print(paste0('window ',i,'/',length(start_years),' done!!!!!'))
}
write_csv(trends, 'data/gages/metrics/trends/trends_window3_multiwindow')


# compare windows ---------------------------------------------------------
trends <- read_csv('data/gages/metrics/trends/trends_window3_multiwindow')
start_years <- unique(trends$start_year)

first_window <- filter(trends, start_year == start_years[1])
last_window <- filter(trends, start_year == start_years[length(start_years)])

window_comp <- left_join(first_window, last_window, by = c('site_no', 'metric'), suffix = c('_f', '_l')) %>%
  mutate(metric = factor(metric, levels = metrics_sel))

cors <- window_comp %>%
  group_by(metric) %>%
  summarise(cor = cor(sen_f, sen_l, use = 'pairwise.complete', method = 'pearson'))

ggplot(window_comp, aes(x = sen_f, y = sen_l)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(slope = 1) +
  facet_wrap(vars(metric), scales = 'free')

sig_comp <- window_comp %>%
  group_by(metric) %>%
  summarise(n_same = sum(sen_sig2a_f == sen_sig2a_l, na.rm = T),
            pct_same = n_same/n())
sig_comp


# plot windows ------------------------------------------------------------
source('scripts/functions/load_states.R')
library(sf)

sig_var <- 'sen_sig2a'
set = 'hw'

if(set == 'hw'){gage_info <- hw_gage_info
                set_label <- 'Headwaters'}
if(set == 'ds'){gage_info <- ds_gage_info
                set_label <- 'Downstream'}

trends <- read_csv('data/gages/metrics/trends/trends_window3_multiwindow')
start_years <- unique(trends$start_year)

hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')
gage_coords <- rbind(hw_gage_info, ds_gage_info) %>%
  select(site_no, lon, lat) %>%
  filter(!duplicated(.))

trends_plot <- filter(trends, start_year %in% c(start_years[1], start_years[length(start_years)])) %>%
  left_join(gage_coords) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(5070)

for(i in 1:length(metrics_sel)){
  plot_dat <- trends_plot %>%
    filter(metric == metrics_sel[i] & site_no %in% gage_info$site_no) %>%
    mutate(across(contains('sig'), ~replace_na(.x, 'none'))) %>%
    mutate(across(contains('sig'), ~factor(.x, levels = c('none','neg','pos')))) %>%
    rename(trend = any_of(sig_var)) %>%
    arrange(trend)
  
  ggplot() +
    geom_sf(data = states, linewidth = 0.5) +
    geom_sf(data = plot_dat, aes(color = trend), size = 1.2) +
    scale_color_manual(values = c('black','red','blue'),
                       limits = c('none','neg','pos'),
                       labels = c('Non-significant','Negative','Positive'),
                       name = NULL, guide = NULL) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = 'none',
          text = element_text(size = 12)) +
    facet_wrap(vars(start_year)) +
    ggtitle(paste0(metrics_sel[i]))
  
  ggsave(paste0('figures/data_maps/metric_trends/window_diffs/',set,'/',metrics_sel[i],'_sig.png'), height = 3.6, width = 12, units = 'in')
}


