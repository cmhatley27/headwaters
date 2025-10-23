library(tidyverse)
source('scripts/Theme+Settings.R')
source('scripts/functions/utilities.R')

hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')
gage_info <- rbind(hw_gage_info, ds_gage_info) %>%
  filter(!duplicated(.)) %>%
  select(site_no, order)

trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv')
metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
# metrics_sel <- c('Q_mean', 'Q5', 'Q95', 
# 'Q_frequency_high_2', 
# 'HFD_mean', 'FlashinessIndex')

plot_dat <- filter(trends, var %in% metrics_sel) %>%
  left_join(gage_info) %>%
  group_by(var, order) %>%
  summarize(pos = sum(sen_sig2a == 'pos', na.rm = T),
            neg = sum(sen_sig2a == 'neg', na.rm = T),
            none = sum(sen_sig2a == 'none', na.rm = T)) %>%
  mutate(n = pos + neg + none) %>%
  mutate(across(c(pos, neg, none), ~.x/n)) %>%
  filter(order >= 0 & order <= 6) %>%
  pivot_longer(c(pos, neg, none), names_to = 'sig', values_to = 'pct') %>%
  mutate(sig = factor(sig, levels = c('none', 'pos', 'neg')),
         var = factor(var, levels = metrics_sel))

ggplot(plot_dat, aes(x = order, y = pct, fill = sig)) +
  facet_wrap(vars(var)) +
  geom_col(color = 'black') +
  scale_fill_manual(values = c('grey90','blue2','red2')) +
  xlab('Stream Order') +
  ylab('Percent of Gages') +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = ~.x*100) +
  theme(legend.position = 'none',
        text = element_text(size = 14))
ggsave('figures/one_offs/ntrends_by_order.png', height = 6, width = 10, units = 'in')  

order_n <- as.data.frame(table(gage_info$order)) %>%
  mutate(order = as.numeric(as.character(Var1))) %>%
  filter(order >= 0 & order <= 6)
ggplot(order_n, aes(x = order, y = Freq)) +
  geom_col() +
  scale_x_continuous(breaks = 1:6) +
  xlab('Stream Order') +
  ylab('# of Gages') +
  theme(legend.position = 'none',
        text = element_text(size = 14))
ggsave('figures/one_offs/ngages_by_order.png', height = 3, width = 4, units = 'in')  
