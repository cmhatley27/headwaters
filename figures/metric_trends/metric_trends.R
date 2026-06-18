# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
# source('./scripts/Theme+Settings.R')

trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_window1.csv'))

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
         sig = factor(sig_ar, levels = c('none', 'pos', 'neg'))) %>%
  filter(!is.na(sig), !is.na(sen)) %>% 
  left_join(select(all_gage_info, site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat) <- 4326
plot_dat <- st_transform(plot_dat, 5070)

percents <- st_drop_geometry(plot_dat) %>%
  group_by(var) %>%
  summarise(n = n(),
            pos = sum(sig == 'pos')/n,
            neg = sum(sig == 'neg')/n,
            total = pos+neg) %>%
  mutate(across(c(pos, neg, total), ~round(.x, 2)))

n_sigs <- st_drop_geometry(plot_dat) %>% 
  filter(var %in% c('Q95', 'Q5', 'Half Flow Date', 'Flashiness Index')) %>%
  group_by(site_no) %>%
  summarise(n_sig = sum(sig != 'none')) %>%
  group_by(n_sig) %>%
  summarise(n= n())

# maps --------------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(plot_dat, var == labelinator(m, metric_labels)) %>%
    arrange(abs(tau))
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = plot_dat_m,
            aes(color = tau, shape = sig != 'none'),
            fill = NA, size = 1) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    scale_color_gradient2(limits = quantile(plot_dat_m$tau, c(0.01,0.99)),
                          oob = scales::squish,
                          # high = 'blue',
                          high = "#4575b4",
                          mid = "#ffffcf",
                          low = "#d73027",
                          # low = 'red',
                          name = 'Kendall\'s \u03c4') +
    scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    labs(title = paste(labelinator(m, metric_labels),'Trends')) +
    theme(panel.border = element_blank(),
          legend.position = 'right',
          legend.box.spacing = unit(-2,'mm'),
          legend.key.height = unit(7, 'mm'),
          legend.key.width = unit(3, 'mm'))
    
  ggsave(paste0('figures/metric_trends/maps/',m,'_',site_label,'.png'),
         width = 90, height = 52.5, units = 'mm',
         dpi = 600)
}


# plot hist ---------------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(plot_dat, var == labelinator(m, metric_labels))
  
  
  p <- ggplot(plot_dat_m, aes(x = sen, fill = sig)) +
    geom_histogram(color = 'black',
                   bins = 30,
                   boundary = 0,
                   linewidth = 0.15) +
    scale_fill_manual(limits = c('neg','none','pos'),
                      values = c('#d73027', 'grey90', '#4575b4'),
                      labels = c('Negative', 'None', 'Positive'),
                      name = 'Trend sig.') +
    xlim(quantile(plot_dat_m$sen, c(0.01,0.99))) +
    geom_vline(xintercept = 0, linewidth = 0.5) +
    xlab('Sen\'s Slope') +
    ylab('Count') +
    theme(panel.border = element_blank(),
          axis.line = element_line(color = 'black', linewidth = 0.25),
          legend.position = 'bottom',
          legend.box.spacing = unit(0.5,'mm'),
          legend.key.size = unit(2,'mm'))
  p
  ggsave(paste0('figures/metric_trends/hists/',m,'_',site_label,'.png'),
         plot = p,
         width = 70, height = 50, units = 'mm',
         dpi = 600)
}



# mini inset hists --------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(plot_dat, var == labelinator(m, metric_labels))
  break_extreme <- unname(round(quantile(abs(plot_dat_m$sen), 0.95),3))
  break_vals <- c(-1*break_extreme, 0, break_extreme)

  ggplot(plot_dat_m, aes(x = sen, fill = sig)) +
    geom_histogram(color = 'black',
                   bins = 20,
                   boundary = 0,
                   linewidth = 0.15) +
    scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
                      name = 'Trend Sig.', guide = NULL) +
    scale_y_continuous(breaks = c(0,50,100)) +
    scale_x_continuous(breaks = break_vals, limits = quantile(plot_dat_m$sen, c(0.01,0.99))) + #ifelse(m %in% c('Q5', 'FlashinessIndex'), 4, 5),
    geom_vline(xintercept = 0, linewidth = 0.5) +
    xlab('Theil-Sen Slope') +
    ylab(NULL) +
    theme(panel.border = element_blank(),
          axis.line = element_line(color = 'black', linewidth = 0.25),
          axis.text = element_text(size = rel(0.8)),
          axis.title = element_text(size = rel(0.9)))

  ggsave(paste0('figures/metric_trends/hists_mini/',m,'_',site_label,'.png'),
         width = 30, height = 18, units = 'mm')
}
