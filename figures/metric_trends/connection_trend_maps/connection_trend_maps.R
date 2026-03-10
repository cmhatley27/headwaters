# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('./scripts/Theme+Settings.R')

trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_window3.csv'))

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 
                 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
metrics_labels <- labelinator(metrics_sel, metric_labels)


cn_dat <- left_join(connections, select(trends, headwater_id = site_no, var, up_sen = sen, up_tau = tau, up_p = p, up_sig = sig_ar2a)) %>%
  left_join(select(trends, downstream_id = site_no, var, down_sen = sen, down_tau = tau, down_p = p, down_sig = sig_ar2a)) %>%
  filter(var %in% metrics_sel) %>%
  mutate(sen_diff = abs(up_sen) - abs(down_sen),
         tau_diff = abs(up_tau) - abs(down_tau),
         sig_cat = case_when(
           up_sig != 'none' & down_sig == 'none' ~ 'up_only',
           up_sig == 'none' & down_sig != 'none' ~ 'down_only',
           up_sig == 'none' & down_sig == 'none' ~ 'both_none',
           up_sig == down_sig & up_sig != 'none' ~ 'both_sig',
           up_sig != down_sig & up_sig != 'none' & down_sig != 'none' ~ 'opposite'
         )) %>%
  filter(!is.na(sen_diff)) %>%
  left_join(select(all_gage_info, headwater_id = site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(cn_dat) <- 4326
cn_dat <- st_transform(cn_dat, 5070)
  

# plot maps ---------------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff))
  
  p <- ggplot() +
    geom_sf(data = states) +
    geom_sf(data = cn_dat_m,
            aes(color = sen_diff),
            size = 1.5) +
    scale_color_gradient2(limits = quantile(cn_dat_m$sen_diff, c(0.05,0.95)),
                          oob = scales::squish,
                          # high = 'blue',
                          low = '#619CCF',
                          # high = "#4575b4",
                          mid = "#ffffcf",
                          high = '#F8766D',
                          # low = "#d73027",
                          # low = 'red',
                          name = '|Sen| Diff') +
    # scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    ggtitle(paste(labelinator(m, metric_labels),'Connection Trend Differences'))
  ggsave(paste0('figures/metric_trends/connection_trend_maps/',m,'.png'),
         plot = p,
         width = 5, height = 3, units = 'in')
}
