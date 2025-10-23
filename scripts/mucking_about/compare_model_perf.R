# data --------------------------------------------------------------------
source('scripts/functions/utilities.R')
library(tidyverse)

hw_gage_info <- read_gage_info()
order2 <- filter(hw_gage_info, order <= 2)$site_no
ds_gage_info <- read_gage_info('downstream')

all_preds <- read_csv('data/models/performance/all_senval_predictions.csv') %>%
  mutate(set = 'all')
all_preds_fil <- filter(all_preds, site_no %in% order2) %>%
  mutate(set = 'all_fil')
hw3_preds <- read_csv('data/models/performance/hw_order3_senval_predictions.csv') %>%
  mutate(set = 'hw3')
hw3_preds_fil <- filter(hw3_preds, site_no %in% order2) %>%
  mutate(set = 'hw3_fil')
hw2_preds <- read_csv('data/models/performance/hw_order2_senval_predictions.csv') %>%
  mutate(set = 'hw2')
dat <- rbind(all_preds, all_preds_fil, hw3_preds, hw3_preds_fil, hw2_preds)

# comp --------------------------------------------------------------------
perf <- dat %>%
  group_by(set, var) %>%
  summarise(r2 = r2(pred, obs),
            cor = cor(pred, obs, use = 'pairwise.complete'))

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

