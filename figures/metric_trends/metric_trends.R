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
  mutate(var = factor(var, levels = rev(metrics_sel), labels = labelinator(rev(metrics_sel), metric_labels)),
         sig = factor(sig_ar2a, levels = c('none', 'pos', 'neg'), labels = c('Non-sig.', 'Positive', 'Negative'))) %>%
  filter(!is.na(sig), !is.na(sen)) %>% 
  left_join(select(all_gage_info, site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat) <- 4326
plot_dat <- st_transform(plot_dat, 5070)

# maps --------------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(plot_dat, var == labelinator(m, metric_labels)) %>%
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
  ggsave(paste0('figures/metric_trends/maps/',m,'_',site_label,'.png'),
         plot = p,
         width = 5, height = 3, units = 'in')
}


# plot hist ---------------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(plot_dat, var == labelinator(m, metric_labels))
  
  p <- ggplot(plot_dat_m, aes(x = sen, fill = sig)) +
    geom_histogram(color = 'black',
                   bins = 30,
                   boundary = 0) +
    scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
                      name = 'Trend Sig.') +
    xlim(quantile(plot_dat_m$sen, c(0.01,0.99))) +
    geom_vline(xintercept = 0, linewidth = 0.75) +
    xlab('Sen\'s Slope') +
    ylab('') +
    ggtitle(paste(labelinator(m, metric_labels),'Trends'))
  ggsave(paste0('figures/metric_trends/hists/',m,'_',site_label,'.png'),
         plot = p,
         width = 5, height = 3, units = 'in')
}



# mini inset hists --------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(plot_dat, var == labelinator(m, metric_labels))
  
  p <- ggplot(plot_dat_m, aes(x = sen, fill = sig)) +
    geom_histogram(color = 'black',
                   bins = 30,
                   boundary = 0) +
    scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
                      name = 'Trend Sig.', guide = NULL) +
    scale_y_continuous(labels = NULL, breaks = NULL) +
    xlim(quantile(plot_dat_m$sen, c(0.01,0.99))) +
    geom_vline(xintercept = 0, linewidth = 0.5) +
    xlab(NULL) +
    ylab(NULL) +
    theme_classic(base_line_size = 0.25, base_size = 8) +
    theme(plot.margin = margin(l = 1),
          plot.background = element_rect(fill = fill_alpha('white',0), color = alpha('white',0)),
          panel.background = element_rect(fill = fill_alpha('white',0)))
  p
  ggsave(paste0('figures/metric_trends/hists_mini/',m,'_',site_label,'.png'),
         plot = p,
         width = 1.15, height = 0.67, units = 'in')
}
