library(tidyverse)
library(ranger)
library(caret)
source('scripts/functions/utilities.R')

metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
# metrics_sel <- Sys.getenv('metrics_sel')

all_gage_info <- read_csv('data/gages/all_gage_info.csv')
metrics_trends <- read_csv(paste0('data/gages/metrics/trends/metrics_trends_window1.csv')) %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = sen)
preds_trends <- read_csv(paste0('data/gages/predictors/pred_trends_window1.csv'))  %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = sen)
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
                     'dist_index', 'water_use_mean', 'tile_pct', 'dam_storage',
                     'soil_perm', 'soil_depth', 'age')
n_preds <- length(c(preds_temporal_sel, preds_static_sel))

results <- data.frame(
  metric = metrics_sel,
  oob_r2 = NA,
  ib_r2 = NA
)
for(m in metrics_sel){
  print(paste('Starting model for metric:',m))
  
  model_name <- paste0(m,'_trends')
  # model_name <- paste0(tolower(m),'_cv5')
  if(!dir.exists(paste0('data/models/',model_name))) dir.create(paste0('data/models/',model_name))
  if(!dir.exists(paste0('data/models/',model_name,'/training'))) dir.create(paste0('data/models/',model_name,'/training'))
  
  dat <- left_join(metrics_trends, preds_trends) %>%
    left_join(., preds_static) %>%
    select(site_no, all_of(c(m, preds_temporal_sel, preds_static_sel))) %>%
    mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
    filter(if_all(everything(), ~!is.na(.x))) %>%
    filter(!is.na(.data[[m]]))
  
  train_dat <- dat %>%
    select(!site_no) %>%
    rename(obs = 1)
  
  set.seed(527)
  rf_fit <- ranger(obs~., data = train_dat, num.trees = 700)
  
  results$oob_r2[results$metric == m] <- rf_fit$r.squared
  results$ib_r2[results$metric == m] <- R2(predict(rf_fit, train_dat)$predictions, train_dat$obs)
}

