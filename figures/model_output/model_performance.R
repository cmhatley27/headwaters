# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'Q5'

model_name <- paste0(tolower(metric_sel),'_annual_w3_emp')
model_dir <- paste0('data/models/',model_name,'/')

region_sel <- 'all'

site_label <- 'hw'
sites_sel <- all_gage_info$site_no[all_gage_info$order <= 3]

shaps <- read_csv(paste0(model_dir,'shaps.csv')) %>%
  filter(site_no %in% sites_sel)

predictions <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  filter(site_no %in% sites_sel)

trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(site_no %in% sites_sel)

#gage counts
ggplot(filter(all_gage_info, site_no %in% sites_sel), aes(x = factor(order))) +
  geom_bar()


# model performance based on ANNUAL VALUES ----------------------------------

global_r2 <- round(r2(predictions$pred, predictions$obs),3)
ggplot(predictions, aes(x = obs, y = pred)) +
  geom_point() +
  geom_abline(slope = 1) +
  ylab(paste(metric_sel, 'Model Prediction')) +
  xlab(paste(metric_sel, 'Observed')) +
  ggtitle(paste(metric_sel,'values; R2:',global_r2))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_model_performance.png'), height = 4, width = 4, units = 'in')


#annual value prediction performance by site
performance_bysite <- predictions %>%
  group_by(site_no) %>%
  summarise(r2 = r2(pred, obs)) %>%
  left_join(select(all_gage_info, site_no, region))
ggplot(performance_bysite, aes(x = r2)) +
  geom_histogram() +
  xlim(c(-1,1))


#using sum of SHAP values at each point
metric_mean <- mean(predictions$obs)
shap_predictions <- shaps %>%
  group_by(site_no, wateryear) %>%
  summarise(shap_sum = sum(shap)+metric_mean) %>%
  right_join(predictions)
shap_sum_r2 <- round(r2(shap_predictions$shap_sum, shap_predictions$obs),3)
ggplot(shap_predictions, aes(x = obs, y = shap_sum)) +
  geom_abline(slope = 1) +
  geom_point() +
  ylab(paste(metric_sel, 'Sum of SHAPs')) +
  xlab(paste(metric_sel, 'Observed')) +
  ggtitle(paste(metric_sel,'sum of SHAPs; R2:',shap_sum_r2))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_shap_sum_performance.png'), height = 4, width = 4, units = 'in')

shap_sum_predictions_trends <- shap_predictions %>%
  group_by(site_no) %>%
  summarise(across(c(shap_sum, obs, pred), ~trendinator(.x), .unpack = '{inner}_{outer}'))

# model performance based on TRENDS ---------------------------------------

#gather trend values from different methods of estimation

#observed trends and trends derived from annual value predictions
derived_trends <- trends %>%
  filter(var == metric_sel) %>%
  pivot_wider(id_cols=site_no, names_from = type, values_from = sen)
#trends from the sum of SHAP trends
shap_sum_trends <- trends %>%
  filter(type == 'shap') %>%
  group_by(site_no) %>%
  summarise(shap_sum = sum(sen))
#trends that were directly modeled
modeled_trends <- read_csv('data/models/all_senval/predictions.csv') %>%
  filter(var == metric_sel, site_no %in% sites_sel) %>%
  select(site_no, modeled = pred)
#trends over the sum of SHAP values (difference from sum of SHAP trends)
metric_mean <- mean(predictions$obs)
shap_val_sum_trends <- shaps %>%
  group_by(site_no, wateryear) %>%
  summarise(shap_sum = sum(shap)+metric_mean) %>%
  group_by(site_no) %>%
  summarise(across(c(shap_sum), ~trendinator(.x), .unpack = '{inner}')) %>%
  select(site_no, shap_val_sum = sen)


#combine and calculate R2 values
trend_comp <- left_join(derived_trends, shap_sum_trends) %>% 
  left_join(shap_val_sum_trends) %>%
  left_join(modeled_trends)

trend_performance <- trend_comp %>%
  summarise(across(c(prediction, shap_sum, modeled, shap_val_sum), ~round(r2(.x, obs), 3)))


#plot obs vs DERIVED
ggplot(filter(trend_comp, !is.na(modeled)), aes(x = obs, y = prediction)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_abline(slope = 1) +
  ylab('Derived trend') +
  xlab('Observed trend') +
  ggtitle(paste('Derived',metric_sel,'trends; R2:',trend_performance$prediction))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_derived_trend_performance.png'), height = 4, width = 4, units = 'in')

#plot obs vs SHAP SUM
ggplot(filter(trend_comp, !is.na(modeled)), aes(x = obs, y = shap_sum)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_abline(slope = 1) +
  ylab('Sum of SHAP trends') +
  xlab('Observed trend') +
  ggtitle(paste('SHAP sum',metric_sel,'trends; R2:',trend_performance$shap_sum))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_shap_sum_trend_performance.png'), height = 4, width = 4, units = 'in')

#plot obs vs MODELED
ggplot(filter(trend_comp, !is.na(modeled)), aes(x = obs, y = modeled)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_abline(slope = 1) +
  ylab('Modeled trend') +
  xlab('Observed trend') +
  ggtitle(paste('Modeled',metric_sel,'trends; R2:',trend_performance$modeled))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_modeled_trends_performance.png'), height = 4, width = 4, units = 'in')
# ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/modeled_trend_performance.png'), height = 4, width = 4, units = 'in')



# Performance of CONNECTION DIFFERENCES -----------------------------------
metric_trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(var == metric_sel, type == 'obs') %>%
  select(site_no, obs=sen)
shap_sum_trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(type == 'shap') %>%
  group_by(site_no) %>%
  summarise(shap_sum = sum(sen)) %>%
  select(site_no, shap_sum)


cn_trends <- left_join(connections, select(metric_trends, headwater_id = site_no, up_obs = obs)) %>%
  left_join(select(metric_trends, downstream_id = site_no, down_obs = obs)) %>%
  left_join(select(shap_sum_trends, headwater_id = site_no, up_shap_sum = shap_sum)) %>%
  left_join(select(shap_sum_trends, downstream_id = site_no, down_shap_sum = shap_sum)) %>%
  mutate(obs_diff = up_obs - down_obs,
         shap_sum_diff = up_shap_sum - down_shap_sum) %>%
  filter(!is.na(shap_sum_diff)) %>%
  filter(abs(obs_diff) <= quantile(abs(obs_diff), 0.99, na.rm = T),
         abs(shap_sum_diff) <= quantile(abs(shap_sum_diff), 0.99, na.rm = T)) 
  

cn_trend_diffs_r2 <- round(r2(cn_trends$shap_sum_diff, cn_trends$obs_diff), 3)  
ggplot(cn_trends, aes(x = obs_diff, y = shap_sum_diff)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_abline(slope = 1) +
  ylab('Sum of \u0394SHAP trends') +
  xlab(paste('Observed \u0394',metric_sel, 'trend')) +
  ggtitle(paste(metric_sel,'Sum of \u0394SHAP trends; R2:',cn_trend_diffs_r2))
ggsave(paste0('figures/model_output/',model_name,'/connection_shap_sum_trend_diff_performance.png'), height = 4, width = 4, units = 'in')


# MSE ---------------------------------------------------------------------
mses <- read_csv(list.files(paste0(model_dir,'mse/'), full.names = T)) %>%
  rename(mse = value) %>%
  left_join(select(predictions, site_no, wateryear, obs)) %>%
  left_join(select(all_gage_info, site_no, order, region)) %>%
  mutate(error_norm = mse-(obs^2))

ggplot(mses, aes(x = obs, y = error_norm, color = region)) +
  geom_point() +
  facet_wrap(vars(region), scales = 'free')
