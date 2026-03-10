# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')

preds <- read_csv('data/gages/predictors/pred_timeseries_window3.csv')

# apply rolling average and calculate trends -------------------------------

a = 0.05
trends <- preds %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val') %>%
  select(!wateryear) %>%
  group_by(site_no, var) %>%
  summarise(across(val, ~trendinator(.x, length_thresh = 5), .unpack = '{inner}')) %>%
  mutate(sig_0 = trend_classifier(sen, p0, alpha = a),
         sig_ar = trend_classifier(sen, p, alpha = a),
         sig_ar2a = trend_classifier(sen, p, alpha = 2*a),
         sig_arfdr = trend_classifier(sen, p, alpha = get_fdr_p(p, fdr_a = 2*a)), #FDR alpha recommended as 2*a
         sig_arfdr2a= trend_classifier(sen, p, alpha = get_fdr_p(p, fdr_a = 4*a))) #so for 2a the FDR alpha is 4*a

write_csv(trends, paste0('data/gages/predictors/pred_trends_window3.csv'))
