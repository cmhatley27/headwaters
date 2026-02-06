# data --------------------------------------------------------------------
source('scripts/functions/utilities.R')
library(tidyverse)

hw_gage_info <- read_gage_info('headwaters')
order2 <- hw_gage_info$site_no[hw_gage_info$order <= 2]
ds_connected_gage_info <- read_gage_info('downstream')
ds_matched_gage_info <- read_gage_info('downstream_matched')
ds_gage_info <- rbind(ds_connected_gage_info, ds_matched_gage_info)

all_preds <- read_csv('data/models/performance/all_senval_predictions.csv') %>%
  mutate(set = 'all')
all_preds_fil <- filter(all_preds, site_no %in% order2) %>%
  mutate(set = 'all_fil')
nomatch_preds <- read_csv('data/models/performance/nomatch_senval_predictions.csv') %>%
  mutate(set = 'nomatch')
nomatch_preds_fil <- filter(nomatch_preds, site_no %in% order2) %>%
  mutate(set = 'nomatch_fil')
hw_preds <- read_csv('data/models/performance/hw_senval_predictions.csv') %>%
  mutate(set = 'hw')
hw_preds_fil <- filter(hw_preds, site_no %in% order2) %>%
  mutate(set = 'hw_fil')
hw2_preds <- read_csv('data/models/performance/hw2_senval_predictions.csv') %>%
  mutate(set = 'hw2')
dat <- rbind(all_preds, all_preds_fil, nomatch_preds, nomatch_preds_fil, hw_preds, hw_preds_fil, hw2_preds)

# comp --------------------------------------------------------------------
perf <- dat %>%
  group_by(set, var) %>%
  summarise(r2 = r2(pred, obs),
            cor = cor(pred, obs, use = 'pairwise.complete')) %>%
  mutate(set = factor(set, levels = c('hw2', 'hw_fil', 'nomatch_fil', 'all_fil',
                                      'hw', 'nomatch', 'all')))

ggplot(perf, aes(x = set, fill = set, y = r2)) +
  geom_col(position = 'dodge') +
  facet_wrap(vars(var)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_hline(yintercept = 0.3, linetype = 'dashed') +
  geom_hline(yintercept = 0)

metrics_sel <- c('Q_mean', 'Q5', 'Q10', 'Q90', 'Q95', 'TotalRR',
                 'Q_frequency_high_2', 'Q_frequency_low_1', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_low_1', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing',
                 'BFI', 'FlashinessIndex', 'FDC_slope', 'BaseflowRecessionK', 'Recession_a_Seasonality')

perf_wide <- perf %>%
  select(set, var, r2) %>%
  mutate(r2 = round(r2, 3)) %>%
  pivot_wider(names_from = set, values_from = r2) %>%
  filter(var %in% metrics_sel) %>%
  mutate(var = factor(var, levels = metrics_sel)) %>%
  arrange(var) %>%
  select(var, hw2, hw3, hw3_fil, all, all_fil)
write_csv(perf_wide, 'data/models/performance/perf_comp.csv')

