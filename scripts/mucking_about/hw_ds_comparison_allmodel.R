# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')

hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')
connections <- read_gage_info('connections')

hw_shaps <- read_csv('data/models/shaps/all_senval_shaps.csv') %>%
  filter(type == 'shap' & site_no %in% hw_gage_info$site_no) %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'hw_shap') %>%
  rename(headwater_id = site_no)
ds_shaps <- read_csv('data/models/shaps/all_senval_shaps.csv') %>%
  filter(type == 'shap' & site_no %in% ds_gage_info$site_no) %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'ds_shap') %>%
  rename(downstream_id = site_no)

hw_vals <- read_csv('data/models/shaps/all_senval_shaps.csv') %>%
  filter(type == 'obs' & site_no %in% hw_gage_info$site_no) %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'hw_val') %>%
  rename(headwater_id = site_no)
ds_vals <- read_csv('data/models/shaps/all_senval_shaps.csv') %>%
  filter(type == 'obs' & site_no %in% ds_gage_info$site_no) %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'ds_val') %>%
  rename(downstream_id = site_no)

trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  filter(var %in% hw_shaps$metric)
hw_ds_splitter <- function(x){
  out <- tibble()
  vars <- unique(x$var)
  for(i in 1:length(vars)){
    var_i = vars[i]
    hw_dat <- filter(x, var == var_i & site_no %in% hw_gage_info$site_no)
    ds_dat <- filter(x, var == var_i & site_no %in% ds_gage_info$site_no)
    out_i <- left_join(connections, hw_dat, by = join_by(headwater_id == site_no)) %>%
      left_join(., ds_dat, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds'))
    
    out <- rbind(out, out_i)
  }
  return(out)
}
trend_comparinator <- function(x, var, type){
  #parameter 'var' selects which trend metric (tau, sen, or mw), while 'type'
  #selects which version of that metric to compare. 'Val' compares the raw values
  #of the trend metrics as the ratio of headwater/downstream, while 'sig' sets
  #categories based on the significance levels and directions of the headwater
  #and downstream trends.
  df <- select(x, contains(var) & !contains('p'))
  if(type == 'sig'){
    df <- select(df, contains('sig_')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = case_when(
        is.na(hw) | is.na(ds) ~ NA,
        hw == 'none' & ds == 'none' ~ 'none',
        hw == ds ~ 'same',
        hw %in% c('pos', 'neg') & ds == 'none' ~ 'hw_only',
        hw == 'none' & ds %in% c('pos', 'neg') ~ 'ds_only',
        hw == 'pos' & ds == 'neg' ~ 'opposite',
        hw == 'neg' & ds == 'pos' ~ 'opposite'
      ))
  }
  if(type == 'sig0'){
    df <- select(df, contains('sig0')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = case_when(
        is.na(hw) | is.na(ds) ~ NA,
        hw == 'none' & ds == 'none' ~ 'none',
        hw == ds ~ 'same',
        hw %in% c('pos', 'neg') & ds == 'none' ~ 'hw_only',
        hw == 'none' & ds %in% c('pos', 'neg') ~ 'ds_only',
        hw == 'pos' & ds == 'neg' ~ 'opposite',
        hw == 'neg' & ds == 'pos' ~ 'opposite'
      ))
  }
  if(type == 'val'){
    df <- select(df, !contains('sig')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = hw/ds)
  }
  return(df$comp)
}

connection_trends <- hw_ds_splitter(trends) %>%
  mutate(trend_comp = trend_comparinator(., 'sen', 'sig')) %>%
  select('headwater_id', 'downstream_id', 'drainage_ratio', 'hw_order', 'ds_order',
         'connection_id', metric = 'var_hw', trend_comp, sen_hw, sen_ds)

cn_shaps <- left_join(connection_trends, hw_shaps) %>%
  left_join(ds_shaps) %>%
  left_join(hw_vals) %>%
  left_join(ds_vals) %>%
  mutate(shap_diff = abs(hw_shap) - abs(ds_shap))


# select metric -----------------------------------------------------------
metric_sel = 'Q_mean'

plot_dat <- filter(cn_shaps, metric == metric_sel) %>%
  filter(!is.na(trend_comp))

ggplot(subset(plot_dat, trend_comp != 'none'), aes(x = hw_shap, y = ds_shap, color = trend_comp)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_abline(slope = 1) +
  scale_color_manual(limits = c('none', 'same', 'ds_only', 'hw_only'),
                     values = c('black', '#00BA38', '#619CCf', '#F8766D')) +
  facet_wrap(vars(pred))

# ggplot(subset(plot_dat, trend_comp != 'none'), aes(x = hw_val, y = ds_val, color = trend_comp)) +
#   geom_point() +
#   geom_abline(slope = 1) +
#   scale_color_manual(limits = c('none', 'same', 'ds_only', 'hw_only'),
#                      values = c('black', '#00BA38', '#619CCf', '#F8766D')) +
#   facet_wrap(vars(pred), scales = 'free')


# select connection -------------------------------------------------------
cn_id_sel = 162
plot_dat2 <- filter(cn_shaps, connection_id == cn_id_sel & metric == metric_sel) %>%
  mutate(pred = fct_reorder(pred, shap_diff, .desc = T))
mods_preds <- read_csv('data/models/performance/all_senval_predictions.csv') %>%
  filter(var == metric_sel) %>%
  filter(site_no %in% c(plot_dat2$headwater_id, plot_dat2$downstream_id)) %>%
  mutate(rse = sqrt((pred-obs)^2),
         error = rse/abs(obs))
mods_preds$error

ggplot(plot_dat2, aes(x = pred, y = shap_diff)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mods_preds, aes(x = obs, y = pred)) + geom_point() +
  geom_abline(slope = 1)


# group connections -------------------------------------------------------
metrics_sel <- c('Q_mean', 'Q5', 'Q95',
                 'Q_frequency_high_2', 'HFD_mean',  'FlashinessIndex')

plot_dat3 <- filter(cn_shaps, metric == metrics_sel[i]) %>%
  filter(trend_comp %in% c('none', 'ds_only', 'hw_only', 'same'))
ggplot(plot_dat3, aes(x = trend_comp, y = shap_diff, fill = trend_comp)) +
  facet_wrap(vars(pred)) +
  geom_boxplot()


# connection performance --------------------------------------------------
connections <- read_gage_info('connections')
mods_preds <- read_csv('data/models/performance/all_senval_predictions.csv')
cn_trends <- select(connection_trends, connection_id, var = metric, trend_comp)

metric_sel = 'Q_mean'
cn_perf <- left_join(connections, mods_preds, by = join_by(headwater_id == site_no)) %>%
  left_join(., mods_preds, by = join_by(downstream_id == site_no, var), suffix = c('_hw', '_ds')) %>%
  mutate(obs_diff = abs(obs_hw) - abs(obs_ds),
         pred_diff = abs(pred_hw) - abs(pred_ds)) %>%
  left_join(cn_trends) %>%
  filter(var == metric_sel) %>%
  filter(!is.na(trend_comp))

r2(cn_perf$pred_diff, cn_perf$obs_diff)
ggplot(subset(cn_perf), aes(x = obs_diff, y = pred_diff)) +
  geom_abline(slope = 1) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  scale_color_manual(limits = c('none', 'opposite', 'same', 'ds_only', 'hw_only'),
                     values = c('black', 'yellow', '#00BA38', '#619CCf', '#F8766D')) +
  geom_point(size = 1.2) +
  theme(legend.position = 'none', text = element_text(size = 14)) +
  ggtitle('Difference in Mean Annual Q trend, HW-DS Pairs') +
  xlab('Observed Difference') +
  ylab('Predicted Difference')
ggsave('figures/one_offs/q_mean_pair_diffs.png', height = 4.5, width = 6, units = 'in')
