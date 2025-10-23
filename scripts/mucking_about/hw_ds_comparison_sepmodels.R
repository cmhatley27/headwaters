library(tidyverse)
source('scripts/functions/utilities.R')

hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')
connections <- read_gage_info('connections')

hw_shaps <- read_csv('data/models/shaps/hw_senval_shaps.csv') %>%
  filter(type == 'shap') %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'hw_shap') %>%
  rename(headwater_id = site_no)
ds_shaps <- read_csv('data/models/shaps/ds_senval_shaps.csv') %>%
  filter(type == 'shap' & site_no %in% ds_gage_info$site_no) %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'ds_shap') %>%
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
  mutate(shap_diff = abs(hw_shap) - abs(ds_shap))

cn_id_sel = 1
metric_sel = 'Q95'

plot_dat <- filter(cn_shaps, metric == metric_sel) %>%
  filter(!is.na(trend_comp))

ggplot(subset(plot_dat, trend_comp != 'non'), aes(x = abs(hw_shap), y = abs(ds_shap), color = trend_comp)) +
  geom_point() +
  geom_abline(slope = 1) +
  scale_color_manual(limits = c('none', 'same', 'ds_only', 'hw_only'),
                     values = c('black', '#00BA38', '#619CCf', '#F8766D')) +
  facet_wrap(vars(pred))

plot_dat2 <- filter(cn_shaps, connection_id == 173 & metric == metric_sel)

ggplot(plot_dat2, aes(x = pred, y = shap_diff)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


trend_hists <- filter(connection_trends, metric == metric_sel) %>%
  select(connection_id, sen_hw, sen_ds)

summary(trend_hists$sen_ds)

