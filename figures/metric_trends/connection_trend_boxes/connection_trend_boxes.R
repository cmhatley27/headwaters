# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('./scripts/Theme+Settings.R')

trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_window3.csv'))

sites_sel <- unique(c(connections$headwater_id, connections$downstream_id))

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 
                 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')

plot_dat <- filter(trends, var %in% metrics_sel, site_no %in% sites_sel) %>%
  filter(!is.na(sen)) %>%
  left_join(select(all_gage_info, site_no, order)) %>%
  mutate(order_lump = ifelse(order <= 3, 'upstream', 'downstream'))

# connection boxes --------------------------------------------------------

for(m in metrics_sel){
  plot_dat_m <- filter(plot_dat, var == m)
  cn_dat <- left_join(connections, select(plot_dat_m, headwater_id = site_no, up_sen = sen)) %>%
    left_join(select(plot_dat_m, downstream_id = site_no, down_sen = sen))
  
  unpaired_wilcox_p <- round(wilcox.test(abs(plot_dat_m$sen[plot_dat_m$order_lump == 'upstream']), abs(plot_dat_m$sen[plot_dat_m$order_lump == 'downstream']))$p.value, 3)
  paired_wilcox_p <- round(wilcox.test(abs(cn_dat$up_sen), abs(cn_dat$down_sen), paired = T)$p.value, 3)
  
  p <- ggplot(plot_dat_m, aes(x = order_lump, fill = order_lump, y = abs(sen))) +
    geom_boxplot() +
    scale_y_continuous(limits = c(0, quantile(abs(plot_dat_m$sen), 0.99, na.rm = T))) +
    scale_x_discrete(limits = c('upstream', 'downstream'), labels = c('Upstream', 'Downstream')) +
    scale_fill_discrete(limits = c('upstream', 'downstream'), guide = NULL) +
    annotate('text', label = paste('Mann-Whitney p:',unpaired_wilcox_p),
             x = -Inf, y = Inf, hjust = -.1, vjust = 2) +
    xlab('') +
    ylab('|Sen\'s Slope|') +
    ggtitle(paste(m,'Connection Trends'))
  p
  ggsave(paste0('figures/metric_trends/connection_trend_boxes/',m,'.png'),
         plot = p,
         width = 3, height = 3, units = 'in')
}
