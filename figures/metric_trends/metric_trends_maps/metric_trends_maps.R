# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('./scripts/Theme+Settings.R')

trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_window3.csv'))

sites_sel <- all_gage_info$site_no[all_gage_info$order <= 3]
site_label <- 'hw'

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 
                 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
metrics_labels <- labelinator(metrics_sel, metric_labels)

plot_dat <- filter(trends, site_no %in% sites_sel, var %in% metrics_sel) %>%
  left_join(select(all_gage_info, site_no, lat, lon)) %>%
  filter(!is.na(sen)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat) <- 4326
plot_dat <- st_transform(plot_dat, 5070)

# make plots --------------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(plot_dat, var == m) %>%
    arrange(abs(tau))
  
  p <- ggplot() +
    geom_sf(data = states) +
    geom_sf(data = plot_dat_m,
            aes(color = tau, shape = p <= 0.1),
            fill = NA, size = 1.5) +
    scale_color_gradient2(limits = quantile(plot_dat_m$tau, c(0.01,0.99)),
                          oob = scales::squish,
                          # high = 'blue',
                          high = "#4575b4",
                          mid = "#ffffcf",
                          low = "#d73027",
                          # low = 'red',
                          name = 'Kendall \u03c4') +
    scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    ggtitle(paste(labelinator(m, metric_labels),'Trends'))
  ggsave(paste0('figures/metric_trends/metric_trends_maps/',site_label,'/',m,'.png'),
         plot = p,
         width = 5, height = 3, units = 'in')
}



