library(tidyverse)
library(ranger)
library(caret)
library(doFuture)
library(futurize)
source('scripts/functions/utilities.R')

jobname <- Sys.getenv('jobname')

slurm_cores = as.numeric(Sys.getenv('cores'))
print(paste('CPUs assigned by SLURM:',slurm_cores))
plan(multicore, workers = slurm_cores)

metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
# metrics_sel <- Sys.getenv('metrics_sel')

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
                     'dist_index', 'water_use_mean', 'tile_pct', 'dam_storage',
                     'soil_perm', 'soil_depth', 'age')
n_preds <- length(c(preds_temporal_sel, preds_static_sel))


n_folds <- 5

all_gage_info$strata <- paste(all_gage_info$region1, all_gage_info$order)
set.seed(527)
all_gage_info$fold <- createFolds(all_gage_info$strata, k = n_folds, list = F)


param_searches <- 60

param_grid <- expand.grid(mtry = 2:20,
                          min.node.size = 3:30,
                          splitrule = c('variance', 'extratrees'))
set.seed(527)
sample_indices <- sample(seq(1, nrow(param_grid)), size = param_searches)
param_grid_sample <- param_grid[sample_indices,]

for(m in metrics_sel){
  print(paste('Starting model for metric:',m))
  
  model_name <- m
  # model_name <- paste0(tolower(m),'_cv5')
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
  for(k in 1:n_folds){
    fold_index[[k]] <- which(dat$fold != k)
  }
  
  train_dat <- select(dat, !c(site_no, wateryear, fold)) %>%
    rename(obs = 1)
  
  cv_control <- trainControl(method = 'cv',
                             number = n_folds,
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
                  metric = 'R2',
                  maximize= T,
                  trControl = cv_control,
                  tuneGrid = param_grid_sample) |>
    futurize()
  
  results <- rf_fit$results %>%
    arrange(desc(R2))
  write_csv(results, paste0('data/models/',model_name,'/training/cv_results.csv'))
  
  preds <- rf_fit$pred %>%
    arrange(rowIndex) %>%
    mutate(site_no = dat$site_no,
           wateryear = dat$wateryear)
  write_csv(preds, paste0('data/models/',model_name,'/training/cv_preds.csv'))
}