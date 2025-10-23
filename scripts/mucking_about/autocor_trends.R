library(tidyverse)
library(rtrend)
library(modifiedmk)
library(trend)
source('scripts/functions/rtrend.R')
library(fume)
source('scripts/Theme+Settings.R')

metrics_sel = c('Q_mean', 'Q95', 'Q10', 'TotalRR', 'Q_frequency_high_3', 'Q_frequency_noflow',
                'Q_totalduration_high_3', 'Q_totalduration_noflow',
                'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                'BaseflowRecessionK', 'Recession_a_Seasonality')

metrics1 <- read_csv('./data/gages/metrics/merged/metrics_window1.csv') %>%
  select(site_no, wateryear, all_of(metrics_sel)) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val') %>%
  arrange(site_no, var) %>%
  mutate(var = factor(var, levels = metrics_sel))
metrics3 <- read_csv('./data/gages/metrics/merged/metrics_window3.csv') %>%
  select(site_no, wateryear, all_of(metrics_sel)) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val') %>%
  arrange(site_no, var) %>%
  mutate(var = factor(var, levels = metrics_sel))

alpha = 0.05
rtrend_names <- c('old_z', 'old_p', 'correct_z', 'correct_p', 'sen', 'intercept')
rtrend_cust_names <- c('old_z', 'old_p', 'correct_z', 'correct_p', 'sen', 'intercept', 'tau')

orig <- metrics1 %>%
  group_by(site_no, var) %>%
  reframe(sen = tryCatch(sens.slope(val)$estimates, error = function(e) NA),
          sen_p = tryCatch(sens.slope(val)$p.value, error = function(e) NA),
          tau = tryCatch(mk.test(val)$estimates['tau'], error = function(e) NA),
          tau_p = tryCatch(mk.test(val)$p.value, error = function(e) NA))
summary(orig$sen_p)

# rtrend <- metrics1 %>%
#   group_by(site_no, var) %>%
#   reframe(val = tryCatch(rtrend::mkTrend(val), error = function(e) NA)) %>%
#   mutate(stat = rep(rtrend_names, length.out = nrow(.))) %>%
#   pivot_wider(id_cols = c(site_no, var), names_from = stat, values_from = val)
# summary(rtrend$old_p)
# summary(rtrend$correct_p)

rtrend_cust <- metrics3 %>%
  group_by(site_no, var) %>%
  summarise(val = tryCatch(mkTrend_r(val), error = function(e) NA)) %>%
  mutate(stat = rtrend_cust_names) %>%
  pivot_wider(id_cols = c(site_no, var), names_from = stat, values_from = val)
ggplot(rtrend_cust, aes(x = old_p, y = correct_p)) +
  geom_point() +
  geom_abline(slope = 1, color = 'red')

comp <- select(orig, site_no, var, old_sen = sen, old_tau = tau, old_p = tau_p) %>%
  left_join(select(rtrend_cust, site_no, var, new_sen = sen, new_tau = tau, new_orig_p = old_p,
                   new_p = correct_p))
ggplot(comp, aes(x = old_p, y = new_p)) +
  geom_point() +
  geom_abline(slope = 1, color = 'red')


mkTrend_r(metrics1$val[metrics1$site_no == '01054200' & metrics1$var == 'Q_mean'])


trends1 <- metrics1 %>%
  group_by(site_no, var) %>%
  summarise(val = tryCatch(mkTrend_r(val), error = function(e) NA)) %>%
  mutate(stat = rtrend_cust_names) %>%
  pivot_wider(id_cols = c(site_no, var), names_from = stat, values_from = val) %>%
  mutate(old_p = ifelse(is.infinite(old_z), NA, old_p))
trend1_summary <- trends1 %>%
  group_by(var) %>%
  summarise(original = sum(old_p <= alpha, na.rm = T),
            corrected = sum(correct_p <= alpha, na.rm = T)) %>%
  pivot_longer(!var, names_to = 'type', values_to = 'n_sig') %>%
  mutate(type = factor(type, levels = c('original', 'corrected')))
ggplot(trend1_summary, aes(x = var, fill = type, y = n_sig)) +
  geom_col(color = 'black', position = 'dodge') +
  labs(x = NULL, y = '# Sig Trends') +
  scale_fill_discrete(name = 'MK Version') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

trends3 <- metrics3 %>%
  group_by(site_no, var) %>%
  summarise(val = tryCatch(mkTrend_r(val), error = function(e) NA)) %>%
  mutate(stat = rtrend_cust_names) %>%
  pivot_wider(id_cols = c(site_no, var), names_from = stat, values_from = val) %>%
  mutate(old_p = ifelse(is.infinite(old_z), NA, old_p))
trend3_summary <- trends3 %>%
  group_by(var) %>%
  summarise(original = sum(old_p <= alpha, na.rm = T),
            corrected = sum(correct_p <= alpha, na.rm = T)) %>%
  pivot_longer(!var, names_to = 'type', values_to = 'n_sig') %>%
  mutate(type = factor(type, levels = c('original', 'corrected')))
ggplot(trend3_summary, aes(x = var, fill = type, y = n_sig)) +
  geom_col(color = 'black', position = 'dodge') +
  labs(x = NULL, y = '# Sig Trends') +
  scale_fill_discrete(name = 'MK Version') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))