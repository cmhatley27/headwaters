library(tidyverse)
source('scripts/functions/utilities.R')

hw_gage_info <- read_gage_info()
trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv')
metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
# metrics_sel <- c('Q_mean', 'Q5', 'Q95', 
                 # 'Q_frequency_high_2', 
                 # 'HFD_mean', 'FlashinessIndex')

plot_dat <- filter(trends, site_no %in% hw_gage_info$site_no & var %in% metrics_sel) %>%
  group_by(site_no) %>%
  summarize(nsig = sum(sen_sig2a != 'none', na.rm = T)) %>%
  group_by(nsig) %>%
  summarise(n = n())

ggplot(plot_dat, aes(x = nsig, y = n)) +
  geom_col() +
  xlab('Number of significant trends') +
  ylab('Number of gages')
ggsave('figures/one_offs/ntrends_by_gage_histogram.png', height = 3, width = 5, units = 'in')  
