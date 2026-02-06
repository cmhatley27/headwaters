library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
# metrics_sel <- c('Q_mean', 'Q5', 'Q95',
#                  'Q_totalduration_low_4', 'Q_frequency_high_2',
#                  'HFD_mean', 'HFI_mean',
#                  'FlashinessIndex', 'BaseflowRecessionK')

trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  left_join(., select(all_gage_info, site_no, type, order, region2, drainage_area)) %>%
  filter(order %in% 1:6,
         var %in% metrics_sel) %>%
  mutate(var = factor(var, levels = metrics_sel),
         region = region_recoder(region2),
         type2 = ifelse(str_detect(type, 'downstream'), 'downstream', 'headwater')) %>%
  mutate(type2 = factor(type2, levels = c('headwater', 'downstream')))

ggplot(filter(all_gage_info, order <= 6), aes(x = factor(order), fill = factor(order))) +
  geom_bar(color = 'black') +
  scale_fill_discrete(guide = NULL) +
  xlab('Order') +
  ylab('Gage count')
ggsave('figures/trends/box_plots/gagecount_byorder.png', width = 2, height = 1.5, units = 'in')

box_plt <- ggplot(trends, aes(x = factor(order), y = abs(sen), fill = factor(order))) +
  geom_boxplot(outliers = F) +
  scale_fill_discrete(guide = 'none') +
  ylab('|Sen\'s Slope|') +
  xlab('Stream Order') +
  facet_wrap(vars(var), scales = 'free_y')
box_plt
ggsave('figures/trends/box_plots/abs_sen_byorder.png', box_plt, width = 8, height = 5, units = 'in')


for(v in metrics_sel){
  asdf <- filter(trends, var == v)
  qwer <- pairwise.wilcox.test(abs(asdf$sen), asdf$order, p.adjust.method = 'BH')$p.value
  print(v)
  print(qwer)
}
