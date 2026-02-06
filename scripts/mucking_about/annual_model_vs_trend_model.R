library(modifiedmk)
annual_preds <- read_csv('data/models/all_annual/predictions.csv')

annual_trend_preds <- annual_preds %>%
  group_by(site_no, var) %>%
  summarise(across(pred, ~trendinator(.x, length_thresh = 5), .unpack = '{inner}'))

obs_trend <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  group_by(var) %>%
  mutate(sen_z = scale(sen)[,1])

comp <- left_join(annual_trend_preds, obs_trend, by = join_by(site_no, var),
                  suffix = c('_p', '_o'))

ggplot(filter(comp, abs(sen_z) <= 3), aes(x = sen_o, y = sen_p)) +
  geom_abline(slope = 1)+
  geom_point() +
  facet_wrap(vars(var), scales = 'free')

comp_perf <- comp %>%
  filter(abs(sen_z) <= 3) %>%
  group_by(var) %>%
  summarise(r2 = r2(sen_p, sen_o),
            rmse = rmse(sen_p, sen_o),
            p_cor = cor(sen_p, sen_o, use = 'pairwise.complete'))

trend_model_preds <- read_csv('data/models/all_senval/predictions.csv') %>%
  left_join(obs_trend, by = join_by(site_no, var))
ggplot(filter(trend_model_preds, abs(sen_z) <= 3), aes(x = sen, y = pred)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(vars(var), scales = 'free')

trend_model_perf <- read_csv('data/models/all_senval/performance.csv')

perf_comparison <- left_join(trend_model_perf, comp_perf, by = join_by(var), suffix = c('_t', '_a'))

ggplot(perf_comparison, aes(x = rmse_t, y = rmse_a)) +
  geom_point() +
  # xlim(c(0,.1)) +
  # ylim(c(0,.1)) +
  geom_abline(slope = 1) +
  xlab('R2 (trend predicted directly)') +
  ylab('R2 (trend calculated from annual predictions)')
  


pred_comparison <- left_join(comp, trend_model_preds)
ggplot(filter(pred_comparison, abs(sen_z) <= 3), aes(x = pred, y = sen_p)) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(vars(var), scales = 'free')
