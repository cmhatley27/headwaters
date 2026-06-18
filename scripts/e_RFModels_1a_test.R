library(tidyverse)
library(ranger)
library(caret)
source('scripts/functions/utilities.R')

# metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
metrics_sel <- c('FlashinessIndex')

all_gage_info <- read_csv('data/gages/all_gage_info.csv')
metrics <- read_csv(paste0('data/gages/metrics/merged/metrics_window1.csv')) %>%
  select(!contains('error_str'))
preds_temporal <- read_csv(paste0('data/gages/predictors/pred_timeseries_window1.csv')) 
preds_static <- read_csv('data/gages/predictors/pred_statics.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)

preds_temporal_sel = c('precip_annual','pet_annual', 
                       'precip_annual_prev','pet_annual_prev',
                       'precip_jfm','pet_jfm', 
                       'precip_amj','pet_amj',
                       'precip_jas','pet_jas', 
                       'precip_ond','pet_ond',
                       'si',
                       'swe_annual','max_swe','max_swe_day', 'zero_swe_day', 'swe_persistence', 'melt_duration',
                       'ag','developed','forest','grass')
preds_static_sel = c('drainage_area', 'elev', 'slope', 'twi',
                     'soil_perm', 'soil_depth',
                     'dist_index',
                     'dam_storage',
                     'age',
                     'water_use_mean',
                     'tile_pct')
n_preds <- length(c(preds_temporal_sel, preds_static_sel))

nfolds <- 2

all_gage_info$strata <- paste(all_gage_info$region1, all_gage_info$order)
set.seed(527)
all_gage_info$fold <- createFolds(all_gage_info$strata, k = nfolds, list = F)

param_grid <- expand.grid(mtry = c(10),
                          min.node.size = c(5),
                          splitrule = 'extratrees')

for(m in metrics_sel){
  print(paste('Starting model for metric:',m))
  
  model_name <- paste0(tolower(m),'_cv1_damsanddepth_w1')
  if(!dir.exists(paste0('data/models/',model_name))) dir.create(paste0('data/models/',model_name))
  if(!dir.exists(paste0('data/models/',model_name,'/training'))) dir.create(paste0('data/models/',model_name,'/training'))
  
  dat <- left_join(metrics, preds_temporal) %>%
    left_join(., preds_static) %>%
    select(site_no, wateryear, all_of(c(m, preds_temporal_sel, preds_static_sel))) %>%
    mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
    filter(if_all(everything(), ~!is.na(.x))) %>%
    filter(!is.na(.data[[m]])) %>%
    left_join(select(all_gage_info, site_no, fold))
  
  fold_index <- list()
  for(k in 1:nfolds){
    fold_index[[k]] <- which(dat$fold != k)
  } 
  
  train_dat <- select(dat, !c(site_no, wateryear, fold)) %>%
    rename(obs = 1)
  
  cv_control <- trainControl(method = 'cv',
                             number = nfolds,
                             index = fold_index,
                             savePredictions = 'final',
                             returnResamp = 'all',
                             summaryFunction = perf_summary,
                             search = 'grid',
                             verboseIter = T)
  
  rf_fit <- train(obs ~ ., data = train_dat,
                  method = 'ranger',
                  num.trees = 700,
                  seed = 527,
                  verbose = T,
                  metric = 'KGE',
                  maximize = T,
                  trControl = cv_control,
                  tuneGrid = param_grid)
  write_csv(rf_fit$results, paste0('data/models/',model_name,'/training/cv_results.csv'))
  
  results <- arrange(rf_fit$results, desc(R2))
  write_csv(results, paste0('data/models/',model_name,'/training/cv_results.csv'))
  write_csv(rf_fit$pred, paste0('data/models/',model_name,'/training/cv_preds.csv'))
}

# ggplot(rf_fit$pred, aes(x = obs, y = pred)) +
#   geom_abline(slope = 1) +
#   geom_point() +
#   facet_wrap(vars(Resample), scales = 'free')
# 
# refit_preds <- dat %>%
#   rename(obs = 3) %>%
#   mutate(pred = rf_fit$finalModel$predictions)
# 
# refit_trends <- refit_preds %>%
#   group_by(site_no) %>%
#   summarise(across(c(pred, obs), ~sens.slope(.x)$estimates)) %>%
#   filter(abs(obs) <= quantile(abs(obs), 0.99))
# 
# ggplot(refit_preds, aes(x = obs, y = pred)) +
#   geom_point()
# r2(refit_preds$pred, refit_preds$obs)
# ggplot(refit_trends, aes(x = obs, y = pred)) +
#   geom_point()
# r2(refit_trends$pred, refit_trends$obs)
# 
# m='FlashinessIndex'
# model_name=paste0(tolower(m),'_cv5_damsanddepth_w1')
# library(trend)
# pred_dat <- left_join(metrics, preds_temporal) %>%
#   left_join(., preds_static) %>%
#   select(site_no, wateryear, all_of(c(m, preds_temporal_sel, preds_static_sel))) %>%
#   mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
#   filter(if_all(everything(), ~!is.na(.x))) %>%
#   filter(!is.na(.data[[m]])) %>%
#   left_join(select(all_gage_info, site_no, fold))
# 
# preds <- read_csv(paste0('data/models/',model_name,'/training/cv_preds.csv')) %>%
#   arrange(rowIndex) %>%
#   mutate(site_no = pred_dat$site_no,
#          wateryear = pred_dat$wateryear)
# r2(preds$pred, preds$obs)
# kge(preds$pred, preds$obs)
# cor(preds$pred, preds$obs)
# ggplot(preds, aes(x = obs, y = pred)) +
#   geom_abline(slope = 1) +
#   geom_point()
# 
# pred_trends <- preds %>%
#   group_by(site_no) %>%
#   summarise(across(c(pred, obs), ~sens.slope(.x)$estimates)) %>%
#   filter(abs(obs) <= quantile(abs(obs), 0.99))
# r2(pred_trends$pred, pred_trends$obs)
# kge(pred_trends$pred, pred_trends$obs)
# cor(pred_trends$pred, pred_trends$obs)
# ggplot(pred_trends, aes(x = obs, y = pred)) +
#   geom_abline(slope = 1) +
#   geom_point()
# 
# 
# 
# cors <- dat %>%
#   select(!c(site_no, wateryear, fold, HFD_mean)) %>%
#   cor(., use = 'pairwise.complete')
