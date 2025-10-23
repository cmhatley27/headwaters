# shap comp ---------------------------------------------------------------
hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')

hw_shaps <- read_csv('data/models/shaps/hw_senval_shaps.csv') %>%
  filter(type == 'shap') %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'hw_shap') 


ds_shaps <- read_csv('data/models/shaps/ds_senval_shaps.csv') %>%
  filter(type == 'shap') %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'ds_shap') %>%
  mutate(is_hw = site_no %in% hw_gage_info$site_no)

all_shaps <- read_csv('data/models/shaps/all_senval_shaps.csv') %>%
  filter(type == 'shap') %>% select(!type) %>%
  pivot_longer(!c(site_no, metric), names_to = 'pred', values_to = 'all_shap') 


hw_comp <- left_join(hw_shaps, all_shaps)
ds_comp <- left_join(ds_shaps, all_shaps) %>%
  filter(!is_hw)

ggplot(subset(ds_comp, metric == 'Q_mean'), aes(x = abs(ds_shap), y = abs(all_shap), color = pred)) +
  geom_point()+
  geom_abline(slope = 1) +
  theme(legend.position = 'none') +
  facet_wrap(vars(pred))


# acc comp ----------------------------------------------------------------
source('scripts/functions/utilities.R')
hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')


all_preds <- read_csv('data/models/performance/all_senval_predictions.csv')

hw_perf <- read_csv('data/models/performance/hw_senval_summary.csv')
ds_perf <- read_csv('data/models/performance/ds_senval_summary.csv')


all_ds_perf <- all_preds %>%
  filter(site_no %in% ds_gage_info$site_no) %>%
  group_by(var) %>%
  summarise(r2 = r2(pred, obs),
            rmse = rmse(pred, obs),
            pcor = cor(pred, obs))

ds_comp <- left_join(ds_perf, all_ds_perf, by = 'var', suffix = c('_dsmod', '_allmod'))

all_hw_perf <- all_preds %>%
  filter(site_no %in% hw_gage_info$site_no) %>%
  group_by(var) %>%
  summarise(r2 = r2(pred, obs),
            rmse = rmse(pred, obs),
            pcor = cor(pred, obs))

hw_comp <- left_join(hw_perf, all_hw_perf, by = 'var', suffix = c('_hwmod', '_allmod'))
