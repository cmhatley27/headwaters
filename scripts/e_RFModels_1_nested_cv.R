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

# metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
metrics_sel <- Sys.getenv('metrics_sel')

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

all_gage_info$strata <- paste(all_gage_info$region1, all_gage_info$order)

n_outerfolds <- 5
n_innerfolds <- 5
param_searches <- 60

param_grid <- expand.grid(mtry = 2:20,
                          min.node.size = 3:30,
                          splitrule = c('variance', 'extratrees'))
set.seed(527)
sample_indices <- sample(seq(1, nrow(param_grid)), size = param_searches)
param_grid_sample <- param_grid[sample_indices,]

ggplot(param_grid_sample, aes(x = mtry, y = min.node.size, color = splitrule)) +
  geom_point() +
  xlim(c(2,20)) +
  ylim(c(3,30))

for(m in metrics_sel){
  print(paste('Starting model for metric:',m))
  
  model_name <- m
  # model_name <- paste0(tolower(m),'_cv5')
  if(!dir.exists(paste0('data/models/',model_name))) dir.create(paste0('data/models/',model_name))
  if(!dir.exists(paste0('data/models/',model_name,'/training'))) dir.create(paste0('data/models/',model_name,'/training'))
  
  results <- expand.grid(fold = 1:n_outerfolds)
  preds <- data.frame()
  
  dat <- left_join(metrics, preds_temporal) %>%
    left_join(., preds_static) %>%
    select(site_no, wateryear, all_of(c(m, preds_temporal_sel, preds_static_sel))) %>%
    mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
    filter(if_all(everything(), ~!is.na(.x))) %>%
    filter(!is.na(.data[[m]]))
  
  set.seed(527)
  outer_folds <- createFolds(all_gage_info$strata, k = n_outerfolds, list = F)
  
  for(o in 1:n_outerfolds){
    print(paste0('Starting fold ',o,'/',n_outerfolds,' at ',Sys.time()))
    
    set.seed(527)
    inner_gage_info <- all_gage_info[outer_folds != o,] %>%
      mutate(inner_fold = createFolds(strata, k = n_innerfolds, list = F))
    
    inner_dat <- filter(dat, site_no %in% inner_gage_info$site_no) %>%
      left_join(select(inner_gage_info, site_no, inner_fold))
    
    fold_index <- list()
    for(k in 1:n_innerfolds){
      fold_index[[k]] <- which(inner_dat$inner_fold != k)
    }
    
    train_dat <- select(inner_dat, !c(site_no, wateryear, inner_fold)) %>%
      rename(obs = 1)
    
    cv_control <- trainControl(method = 'cv',
                               number = n_innerfolds,
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
                    tuneLength = param_searches,
                    tuneGrid = param_grid_sample) |>
      futurize()
    inner_results <- rf_fit$results %>%
      mutate(fold = o)
    write_csv(inner_results, paste0('data/models/',model_name,'/training/ncv_inner_',o,'_results.csv'))
    
    test_dat <- filter(dat, site_no %nin% inner_gage_info$site_no) %>%
      select(!c(site_no, wateryear)) %>%
      rename(obs = 1)
    
    test_preds <- data.frame(
      obs = test_dat$obs,
      pred = predict(rf_fit, test_dat),
      fold = o)
    preds <- rbind(preds, test_preds)
    
    test_perf <- perf_summary(test_preds)
    
    results[o,2:(1+length(rf_fit$bestTune))] <- rf_fit$bestTune
    results[o,(2+length(rf_fit$bestTune)):(1+length(rf_fit$bestTune) + length(test_perf))] <- test_perf
    
    print(paste0('Outer fold ',o,'/',n_outerfolds,' for ',m,' done at ',Sys.time(),'!!'))
  }
  names(results)[(2+length(rf_fit$bestTune)):(1+length(rf_fit$bestTune) + length(test_perf))] <- names(test_perf)
  write_csv(results, paste0('data/models/',model_name,'/training/ncv_outer_results.csv'))
  write_csv(preds, paste0('data/models/',model_name,'/training/ncv_outer_preds.csv'))
}


# get performance once models are done
# metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
# 
# for(m in metrics_sel){
#   model_name <- m
#   ncv_perf <- read_csv(paste0('data/models/',model_name,'/training/ncv_outer_results.csv'))
#   
#   print(m)
#   print(paste0('R2:',mean(ncv_perf$R2)))
# }