# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
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

plot_dat <- filter(trends, var %in% metrics_sel, site_no %in% sites_sel) %>%
  mutate(var = factor(var, levels = rev(metrics_sel), labels = labelinator(rev(metrics_sel), metric_labels)),
         sig = factor(sig_ar2a, levels = c('none', 'pos', 'neg'), labels = c('Non-sig.', 'Positive', 'Negative'))) %>%
  filter(!is.na(sig))



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
  ggsave(paste0('figures/metric_trends/metric_trend_hists/',site_label,'/',m,'.png'),
         plot = p,
         width = 5, height = 3, units = 'in')
  
}


