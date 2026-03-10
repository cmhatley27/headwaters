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
  filter(!is.na(sen_diff))


# plot histograms ---------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff))
  
  p <- ggplot(cn_dat_m, aes(x = sen_diff)) +
    geom_histogram(color = 'black', fill = 'grey80',
                   bins = 20,
                   boundary = 0) +
    # scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
    #                   name = 'Trend Sig.') +
    xlim(quantile(cn_dat_m$sen_diff, c(0.01,0.99))) +
    geom_vline(xintercept = 0, linewidth = 0.75) +
    xlab('|Sen\'s Slope| Difference') +
    ylab('') +
    ggtitle(paste(labelinator(m, metric_labels),'Connection Trend Differences'))
  ggsave(paste0('figures/metric_trends/connection_trend_hists/',m,'.png'),
         plot = p,
         width = 4, height = 3, units = 'in')
}
