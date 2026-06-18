library(tidyverse)
library(ranger)
library(caret)
library(doFuture)
library(futurize)
source('scripts/functions/utilities.R')

#settings for parallelization in the cluster
# slurm_cores = as.numeric(Sys.getenv('cores'))
# print(paste('CPUs assigned by SLURM:',slurm_cores))
# plan(multicore, workers = slurm_cores)

#choose which metrics to model
metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
# metrics_sel <- Sys.getenv('metrics_sel')

#load in all the data
all_gage_info <- read_csv('data/gages/all_gage_info.csv')
metrics <- read_csv(paste0('data/gages/metrics/merged/metrics_window1.csv')) %>%
  select(!contains('error_str'))
preds_temporal <- read_csv(paste0('data/gages/predictors/pred_timeseries_window1.csv')) 
preds_static <- read_csv('data/gages/predictors/pred_statics.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)

#select which predictors to use
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

#set up train/test split
train_split <- 0.8
#stratify based on ecoregion and stream order to make sure samples are balanced
all_gage_info$strata <- paste(all_gage_info$region1, all_gage_info$order)
set.seed(527)
#assign gages to samples and add that info to all_gage_info
train_rows <- createDataPartition(all_gage_info$strata, p = train_split, list = F)
all_gage_info$train <- F
all_gage_info$train[train_rows] <- T


#set up hyperparameter grid for tuning
#number of parameter sets to test. 60 is a good rule of thumb but can bump up to 100 if training is fast
param_searches <- 60
#create grid using set ranges of these three parameters. Can adjust as needed, good reference at:
#https://bradleyboehmke.github.io/HOML/random-forest.html#hyperparameters
param_grid <- expand.grid(mtry = 2:20,
                          min.node.size = 3:30,
                          splitrule = c('variance', 'extratrees'))
#randomly select parameter sets from the grid
set.seed(527)
sample_indices <- sample(seq(1, nrow(param_grid)), size = param_searches)
param_grid_sample <- param_grid[sample_indices,]

#loop through each metric to train models
for(m in metrics_sel){
  print(paste('Starting model for metric:',m))
  
  #set name for model and filepaths to results
  model_name <- m
  # model_name <- paste0(tolower(m),'_cv5')
  if(!dir.exists(paste0('data/models/',model_name))) dir.create(paste0('data/models/',model_name))
  if(!dir.exists(paste0('data/models/',model_name,'/training'))) dir.create(paste0('data/models/',model_name,'/training'))
  
  #set up all the data for modeling
  #select the metric and all predictors to be used
  dat <- left_join(metrics, preds_temporal) %>%
    left_join(., preds_static) %>%
    select(site_no, wateryear, all_of(c(m, preds_temporal_sel, preds_static_sel))) %>%
    #remove infinites, nans, nas, etc
    mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
    filter(if_all(everything(), ~!is.na(.x))) %>%
    filter(!is.na(.data[[m]])) %>%
    #join column about which cross-validation fold each gage is in
    left_join(select(all_gage_info, site_no, train))
  
  #split up train/test data and prep them for model input
  train_dat <- filter(dat, train == T) %>%
    select(!c(site_no, wateryear)) %>%
    rename(obs = 1)
  
  test_dat <- filter(dat, train == F) %>%
    select(!c(site_no, wateryear)) %>%
    rename(obs = 1)
  #save identifying info for test samples for later
  test_dat_info <- filter(dat, train == F) %>%
    select(c(site_no, wateryear))
  
  #set hyperparamter tuning settings
  oob_control <- trainControl(method = 'oob',
                             savePredictions = 'final',
                             returnResamp = 'all',
                             summaryFunction = defaultSummary,
                             search = 'grid',
                             verboseIter = T)
  
  #train models using tuning settings. This function will loop through all of our
  #parameter sets and save the model with the parameters that give the best performance
  rf_fit <- train(obs ~ ., data = train_dat,
                  method = 'ranger',
                  num.trees = 700,
                  seed = 527,
                  verbose = T,
                  metric = 'R2',
                  maximize= T,
                  trControl = oob_control,
                  tuneGrid = param_grid_sample) #|>
    #this enables parallelization when on the cluster
    # futurize()

  #save the results of hyperparameter tuning, arranged in descending performance order
  #Top row will give you the optimal parameters to use in final model
  tune_results <- rf_fit$results %>%
    arrange(desc(R2))
  write_csv(tune_results, paste0('data/models/',model_name,'/training/tune_results.csv'))
  
  #run test data through the best model to assess performance on new data
  test_preds <- test_dat_info %>%
    mutate(obs = test_dat$obs,
           pred = predict(rf_fit, test_dat))
  write_csv(test_preds, paste0('data/models/',model_name,'/training/test_preds.csv'))
  
  test_perf <- R2(test_preds$pred, test_preds$obs)
  write_csv(test_perf, paste0('data/models/',model_name,'/training/test_perf.csv'))
}