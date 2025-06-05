# libraries and data ------------------------------------------------------
library(tidyverse)
library(trend)
source('./scripts/functions/read_gages.R')

window_size <- 20
metrics <- read_csv(paste0('./data/gages/metrics/metrics_window',window_size,'.csv')) %>%
  select(site_no, wateryear, everything())

# calculate MK trends -----------------------------------------------------
a = 0.05 #significance level
thresh = 10 #min number of years needed to calculate trend
trends <- metrics %>%
  select(!contains('error_str')) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val') %>%
  mutate(period = ifelse(wateryear <= 1991, 1, 2)) %>%
  select(!wateryear) %>%
  filter(!is.na(val)) %>%
  group_by(site_no, var) %>%
  summarise(tau = ifelse(length(val) >= thresh, mk.test(val)$estimates['tau'], NA),
            tau_p = ifelse(length(val) >= thresh, mk.test(val)$p.value, NA),
            median_diff = ifelse(length(val) >= thresh, 
                                 median(val[period == 2], na.rm = T) - median(val[period == 1], na.rm = T), NA),
            mw_p = ifelse(length(val) >= thresh, wilcox.test(val~period)$p.value, NA),
            sen = ifelse(length(val) >= thresh, sens.slope(val)$estimates[[1]], NA),
            sen_p = ifelse(length(val) >= thresh, sens.slope(val)$p.value, NA)) %>%
  ungroup() %>%
  mutate(tau_trend = ifelse(tau > 0 & tau_p <= a, 'pos',
                            ifelse(tau < 0 & tau_p <= a, 'neg',
                                   'none')),
         mw_trend = ifelse(median_diff > 0 & mw_p <= a, 'pos',
                           ifelse(median_diff < 0 & mw_p <= a, 'neg',
                                  'none')),
         sen_trend = ifelse(sen > 0 & sen_p <= a, 'pos',
                            ifelse(sen < 0 & sen_p <= a, 'neg',
                                   'none'))
         )
#save
write_csv(trends, paste0('./data/gages/metrics/trends_window',window_size,'.csv'))
