# data --------------------------------------------------------------------
library(tidyverse)
library(ggExtra)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'Q95'

model_name <- 'q95_annual'

model_dir <- paste0('data/models/',model_name,'/')

region_sel <- 'all'
if(region_sel == 'all') region_sel <- unique(all_gage_info$region)

sites_sel <- all_gage_info$site_no[all_gage_info$region %in% region_sel]

predictor_statics <- read_csv('data/gages/predictors/pred_statics.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)
predictor_temporals <- read_csv('data/gages/predictors/pred_timeseries.csv')
predictor_variables <- left_join(predictor_temporals, predictor_statics) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val')

shaps <- read_csv(list.files(paste0(model_dir,'shaps/'), full.names = T)) %>%
  select(!c(explain_id, none)) %>%
  left_join(select(all_gage_info, site_no, order)) %>%
  pivot_longer(!c(metric, site_no, order, wateryear), names_to = 'var', values_to = 'shap') %>%
  mutate(
    order_lump3 = case_when(
      order <= 2 ~ 'hw',
      order == 3 ~ 'order3',
      order >= 4 ~ 'ds'
    ),
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  ) %>%
  left_join(predictor_variables) %>%
  left_join(mses) %>%
  filter(site_no %in% sites_sel)

metric_values <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  rename(val = obs) %>%
  left_join(select(all_gage_info, site_no, order)) %>%
  filter(site_no %in% sites_sel) %>%
  mutate(
    order_lump3 = case_when(
      order <= 2 ~ 'hw',
      order == 3 ~ 'order3',
      order >= 4 ~ 'ds'
    ),
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  )

#gage counts
gage_counts <- filter(all_gage_info, site_no %in% sites_sel) %>%
  mutate(
    order_lump3 = case_when(
      order <= 2 ~ 'hw',
      order == 3 ~ 'order3',
      order >= 4 ~ 'ds'
    ),
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  )
ggplot(gage_counts, aes(x = order)) +
  geom_bar() +
  scale_x_continuous(breaks = 1:6)


# model performance -------------------------------------------------------

#annual value prediction performance
model_predictions <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  left_join(select(all_gage_info, site_no, region)) 

global_performance <- round(r2(model_predictions$pred, model_predictions$obs),3)
ggplot(model_predictions, aes(x = obs, y = pred)) +
  geom_point() +
  geom_abline(slope = 1) +
  annotate('text', x = (max(model_predictions$obs)-min(model_predictions$obs))/8, y = max(model_predictions$pred), 
           label = paste('R2:',global_performance)) +
  ylab(paste(metric_sel, 'Model Prediction')) +
  xlab(paste(metric_sel, 'Observed'))
ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/global_model_performance.png'), height = 4, width = 4, units = 'in')


#annual value prediction performance by site
model_performance_bysite <- model_predictions %>%
  group_by(site_no) %>%
  summarise(r2 = r2(pred, obs)) %>%
  left_join(select(all_gage_info, site_no, region))
ggplot(model_performance_bysite, aes(x = r2)) +
  geom_histogram() +
  xlim(c(-1,1))


#derived trend performance
derived_trends <- model_predictions %>%
  group_by(site_no) %>%
  summarise(across(c(obs, pred), ~trendinator(.x), .unpack = T)) %>%
  filter(abs(scale(obs_sen)) <= 4)

derived_trend_performance <- round(r2(derived_trends$pred_sen, derived_trends$obs_sen),3)
ggplot(derived_trends, aes(x = obs_sen, y = pred_sen)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_abline(slope = 1) +
  ylab('Derived trend') +
  xlab('Observed trend') +
  ggtitle(paste('Derived',metric_sel,'trends; R2:',derived_trend_performance))
ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/derived_trend_performance.png'), height = 4, width = 4, units = 'in')


#versus modeled trend performance
modeled_trends <- read_csv('data/models/all_senval/predictions.csv') %>%
  filter(var == metric_sel, site_no %in% derived_trends$site_no)

modeled_trend_performance <- round(r2(modeled_trends$pred, modeled_trends$obs),3)
ggplot(modeled_trends, aes(x = obs, y = pred)) +
  geom_point() +
  geom_abline(slope = 1)



# MSE ---------------------------------------------------------------------
mses <- read_csv(list.files(paste0(model_dir,'mse/'), full.names = T)) %>%
  rename(mse = value) %>%
  left_join(select(metric_values, site_no, wateryear, val, pred)) %>%
  left_join(select(all_gage_info, site_no, order, region)) %>%
  mutate(error_norm = mse/val)

ggplot(mses, aes(x = val, y = mse, color = region)) +
  geom_point() +
  facet_wrap(vars(region), scales = 'free_x')
