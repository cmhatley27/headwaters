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
  group_by(var, sig = sig_ar2a) %>%
  summarise(n = n()) %>%
  filter(!is.na(sig)) %>%
  group_by(var) %>%
  mutate(n_pct = n/sum(n)) %>%
  mutate(var = factor(var, levels = rev(metrics_sel), labels = labelinator(rev(metrics_sel), metric_labels)),
         sig = factor(sig, levels = c('none', 'pos', 'neg'), labels = c('Non-sig.', 'Positive', 'Negative')))
  



# plot pos-neg bars -------------------------------------------------------

ggplot(data = plot_dat, aes(y = var, x = n_pct, fill = sig)) +
  geom_col(color = 'black') +
  scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
                    name = 'Trend Sig.') +
  ylab('') +
  xlab(paste0('% of gages (n = ',length(sites_sel),')'))
ggsave(paste0('figures/metric_trends/sig_bar_stacks/',site_label,'.png'),
       width = 6, height = 5.5, units = 'in')



# plot connection diff bars -----------------------------------------------

cn_dat <- left_join(connections, select(trends, headwater_id = site_no, var, up_sig = sig_ar2a)) %>%
  left_join(select(trends, downstream_id = site_no, var, down_sig = sig_ar2a)) %>%
  filter(var %in% metrics_sel) %>%
  # filter(!is.na(up_sig), !is.na(down_sig)) %>%
  mutate(sig_cat = case_when(
    up_sig != 'none' & down_sig == 'none' ~ 'up_only',
    up_sig == 'none' & down_sig != 'none' ~ 'down_only',
    up_sig == 'none' & down_sig == 'none' ~ 'both_none',
    up_sig == down_sig & up_sig != 'none' ~ 'both_sig',
    up_sig != down_sig & up_sig != 'none' & down_sig != 'none' ~ 'opposite'
  ))

plot_dat <- cn_dat %>%
  group_by(var, sig_cat) %>%
  summarise(n = n()) %>%
  filter(!is.na(sig_cat)) %>%
  group_by(var) %>%
  mutate(n_pct = n/sum(n)) %>%
  mutate(var = factor(var, levels = rev(metrics_sel), labels = labelinator(rev(metrics_sel), metric_labels)),
         sig_cat = factor(sig_cat, 
                          levels = c('both_none', 'opposite', 'both_sig', 'down_only', 'up_only'),
                          labels = c('Neither', 'Both (opp. dir.)', 'Both (same dir.)', 'Downstream', 'Upstream')))


ggplot(data = plot_dat, aes(y = var, x = n_pct, fill = sig_cat)) +
  geom_col(color = 'black') +
  scale_fill_manual(values = c('grey90', '#FFFF00', '#00BA38', '#619CCF', '#F8766D'),
                    name = 'Gage with\nsig. trend') +
  ylab('') +
  xlab(paste0('% of connections (n = ',nrow(connections),')'))
ggsave(paste0('figures/metric_trends/sig_bar_stacks/connections.png'),
       width = 6, height = 5.5, units = 'in')
