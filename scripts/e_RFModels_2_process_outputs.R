# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'FlashinessIndex'

model_name <- paste0(tolower(metric_sel),'_annual_w3_emp')
model_dir <- paste0('data/models/',model_name,'/')

predictor_statics <- read_csv('data/gages/predictors/pred_statics.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)
predictor_temporals <- read_csv('data/gages/predictors/pred_timeseries_window3.csv')
predictor_variables <- left_join(predictor_temporals, predictor_statics) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val')

# read outputs and format -------------------------------------------------
#gather loose shap files and add gage info
shaps <- read_csv(list.files(paste0(model_dir,'shaps/'), full.names = T)) %>%
  select(!c(explain_id, none)) %>%
  left_join(select(all_gage_info, site_no, order, region)) %>%
  pivot_longer(!c(metric, site_no, order, region, wateryear), names_to = 'var', values_to = 'shap') %>%
  mutate(
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  ) %>%
  left_join(predictor_variables) %>%
  select(metric, site_no, order, order_lump2, region, wateryear, var, val, shap)
write_csv(shaps, paste0(model_dir, 'shaps.csv'))

#add gage info to predictions
predictions <- read_csv(paste0(model_dir,'predictions_raw.csv')) %>%
  left_join(select(all_gage_info, site_no, order, region)) %>%
  mutate(metric = var) %>%
  mutate(
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  ) %>%
  select(metric, site_no, order, order_lump2, region, wateryear, var, obs, pred)
write_csv(predictions, paste0(model_dir, 'predictions.csv'))


# calculate trends --------------------------------------------------------

#calculate trends in SHAP values
shaps_trends <- shaps %>%
  group_by(metric, site_no, order, order_lump2, region, var) %>%
  summarise(across(shap, ~trendinator(.x), .unpack = '{inner}'),
            int = median(shap)-(sen*median(wateryear))) %>%
  mutate(type = 'shap') %>%
  select(metric, site_no, order, order_lump2, region, var, tau, sen, p, int, type)

#calculate trends in raw predictor variables
predictors_trends <- shaps %>%
  group_by(metric, site_no, order, order_lump2, region, var) %>%
  summarise(across(val, ~trendinator(.x), .unpack = '{inner}'),
            int = median(val)-(sen*median(wateryear))) %>%
  mutate(type = 'obs') %>%
  select(metric, site_no, order, order_lump2, region, var, tau, sen, p, int, type)

#get trends in observed metric values
metric_obs_trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  filter(var == metric_sel) %>%
  left_join(select(all_gage_info, site_no, order, region)) %>%
  mutate(metric = var) %>%
  mutate(
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  ) %>%
  select(metric, site_no, order, order_lump2, region, var, tau, sen, p, int) %>%
  mutate(type = 'obs')

#calculate trends in predicted metric values
metric_pred_trends <- predictions %>%
  group_by(metric, site_no, order, order_lump2, region, var) %>%
  summarise(across(pred, ~trendinator(.x), .unpack = '{inner}'),
            int = median(pred)-(sen*median(wateryear))) %>%
  select(metric, site_no, order, order_lump2, region, var, tau, sen, p, int) %>%
  mutate(type = 'prediction')

#join up and save
trends <- rbind(metric_obs_trends, metric_pred_trends, predictors_trends, shaps_trends)
write_csv(trends, paste0(model_dir,'trends.csv'))


# calculate local importance linear models --------------------------------
shap_lms <- shaps %>%
  group_by(metric, site_no, var) %>%
  summarise(lm_b = tryCatch(summary(lm(shap~val))$coefficients[2,1],
                            error = function(e) NA),
            lm_r2 = tryCatch(summary(lm(shap~val))$r.squared,
                             error = function(e) NA)) %>%
  mutate(type = 'self')

ggplot(shap_lms, aes(x = var, y = lm_r2)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shaps_w <- pivot_wider(shaps, id_cols = c(metric, site_no, order, region, wateryear), names_from = var, values_from = shap)
vals_w <- pivot_wider(shaps, id_cols = c(metric, site_no, order, region, wateryear), names_from = var, values_from = val)
dat_w <- left_join(shaps_w, vals_w, by = c('metric', 'site_no', 'wateryear', 'order', 'region'), suffix = c('_shap', '_val'))

precip_lm_b <- dat_w %>%
  group_by(metric, site_no) %>%
  summarise(across(ends_with('_shap'),
                   ~tryCatch(summary(lm(.x~precip_annual_val))$coefficients[2,1],
                             error = function(e) NA))) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'lm_b') %>%
  mutate(var = str_remove(var, '_shap'),
         type = 'precip_annual')
precip_lm_r2 <- dat_w %>%
  group_by(metric, site_no) %>%
  summarise(across(ends_with('_shap'),
                   ~tryCatch(summary(lm(.x~precip_annual_val))$r.squared,
                             error = function(e) NA))) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'lm_r2') %>%
  mutate(var = str_remove(var, '_shap'),
         type = 'precip_annual')
precip_lms <- left_join(precip_lm_b, precip_lm_r2) %>%
  select(metric, site_no, var, lm_b, lm_r2, type)

pet_lm_b <- dat_w %>%
  group_by(metric, site_no) %>%
  summarise(across(ends_with('_shap'),
                   ~tryCatch(summary(lm(.x~pet_annual_val))$coefficients[2,1],
                             error = function(e) NA))) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'lm_b') %>%
  mutate(var = str_remove(var, '_shap'),
         type = 'pet_annual')
pet_lm_r2 <- dat_w %>%
  group_by(metric, site_no) %>%
  summarise(across(ends_with('_shap'),
                   ~tryCatch(summary(lm(.x~pet_annual_val))$r.squared,
                             error = function(e) NA))) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'lm_r2') %>%
  mutate(var = str_remove(var, '_shap'),
         type = 'pet_annual')
pet_lms <- left_join(pet_lm_b, pet_lm_r2) %>%
  select(metric, site_no, var, lm_b, lm_r2, type)

lms <- rbind(shap_lms, precip_lms, pet_lms)
write_csv(lms, paste0(model_dir,'lms.csv'))

# get shap difference trends for each connection --------------------------
metrics <- read_csv('data/gages/metrics/merged/metrics_window3.csv') %>%
  select(site_no, wateryear, obs = all_of(metric_sel))
  
cn_metrics <- left_join(connections, select(metrics, headwater_id = site_no, wateryear, up_metric = obs)) %>%
  left_join(select(metrics, downstream_id = site_no, wateryear, down_metric = obs)) %>%
  mutate(metric_diff = up_metric-down_metric) %>%
  group_by(connection_id, headwater_id, downstream_id) %>%
  summarize(across(metric_diff, ~trendinator(.x), .unpack = '{inner}'),
            int = median(metric_diff, na.rm = T) - (sen*median(wateryear))) %>%
  mutate(metric = metric_sel, var = metric) %>%
  select(metric, connection_id, headwater_id, downstream_id, var, tau, sen, p, int) %>%
  mutate(type = 'obs')

cn_shaps <- left_join(connections, select(shaps, headwater_id = site_no, var, wateryear, up_shap = shap)) %>%
  left_join(select(shaps, downstream_id = site_no, var, wateryear, down_shap = shap)) %>%
  mutate(shap_diff = up_shap-down_shap) %>%
  group_by(connection_id, headwater_id, downstream_id, var) %>%
  summarize(across(shap_diff, ~trendinator(.x), .unpack = '{inner}'),
            int = median(shap_diff, na.rm = T) - (sen*median(wateryear))) %>%
  mutate(metric = metric_sel) %>%
  select(metric, connection_id, headwater_id, downstream_id, var, tau, sen, p, int) %>%
  mutate(type = 'shap')

cn_diff_trends <- rbind(cn_metrics, cn_shaps)
write_csv(cn_diff_trends, paste0(model_dir,'connection_difference_trends.csv'))


