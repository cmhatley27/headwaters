# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')

preds <- read_csv('data/gages/predictors/pred_timeseries.csv')

# apply rolling average and calculate trends -------------------------------
#rolling average to match window size of metrics
window_size = 3

avgs <- preds %>%
  #drop previous year climate variables since trends will be identical
  select(!ends_with('prev')) %>%
  group_by(site_no) %>%
  #split out water use because it is already provided on a 5 year increment
  #and does not need a rolling average
  mutate(across(!c(wateryear, water_use), ~rollmean(.x, window_size, fill = NA))) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val')

a = 0.05
trends <- select(avgs, !wateryear) %>%
  group_by(site_no, var) %>%
  summarise(across(val, ~trendinator(.x, length_thresh = 5), .unpack = '{inner}')) %>%
  mutate(sig_0 = trend_classifier(sen, p0, alpha = a),
         sig_ar = trend_classifier(sen, p, alpha = a),
         sig_ar2a = trend_classifier(sen, p, alpha = 2*a),
         sig_arfdr = trend_classifier(sen, p, alpha = get_fdr_p(p, fdr_a = 2*a)), #FDR alpha recommended as 2*a
         sig_arfdr2a= trend_classifier(sen, p, alpha = get_fdr_p(p, fdr_a = 4*a))) #so for 2a the FDR alpha is 4*a

write_csv(trends, paste0('data/gages/predictors/pred_trends.csv'))
