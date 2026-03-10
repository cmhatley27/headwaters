# libraries and data ------------------------------------------------------
library(tidyverse)
library(ranger)
library(shapr)
library(future)
library(progressr)
plan(multicore, workers = 32)
print('cores available:')
print(availableCores('multicore'))

all_gage_info <- read_csv('input_data/all_gage_info.csv')
metrics <- read_csv('input_data/metrics_window3.csv') %>%
  select(!contains('error_str'))
preds_temporal <- read_csv('input_data/pred_timeseries.csv') 
preds_static <- read_csv('input_data/pred_statics.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)

metrics_sel <- 'Q_mean'

model_name <- paste0(tolower(metrics_sel), '_annual')
if(!dir.exists(paste0('output/',model_name))) dir.create(paste0('output/',model_name))
if(!dir.exists(paste0('output/',model_name,'/intermediate'))) dir.create(paste0('output/',model_name,'/intermediate'))
if(!dir.exists(paste0('output/',model_name,'/shaps'))) dir.create(paste0('output/',model_name,'/shaps'))
if(!dir.exists(paste0('output/',model_name,'/timing'))) dir.create(paste0('output/',model_name,'/timing'))
if(!dir.exists(paste0('output/',model_name,'/mse'))) dir.create(paste0('output/',model_name,'/mse'))
trim_outliers = F

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
                     'soil_perm', 'soil_awc',
                     'dist_index',
                     'age',
                     #'precip_mean', 'temp_mean', 'pet_mean',
                     #'si_mean',
                     'water_use_mean',
                     'tile_pct')

dat <- left_join(metrics, preds_temporal) %>%
  left_join(., preds_static) %>%
  select(site_no, wateryear, all_of(c(metrics_sel, preds_temporal_sel, preds_static_sel))) %>%
  mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
  filter(if_all(everything(), ~!is.na(.x))) %>%
  filter(!is.na(.data[[metrics_sel]]))

if(trim_outliers == T){
  trimmer <- function(x){
    u = mean(x, na.rm = T)
    sd = sqrt(var(x, na.rm = T))
    x_lower = u-3*sd
    x_upper = u+3*sd
    x_trim = x[x >= x_lower & x <= x_upper]
  }
  dat <- dat %>%
    mutate(across(!c(site_no, wateryear), trimmer))
}

# run RF ------------------------------------------------------------------
obs_info <- select(dat, site_no, wateryear)
dat_in <- select(dat, all_of(c(metrics_sel, preds_temporal_sel, preds_static_sel))) %>%
  rename(obs = 1)

set.seed(527)
require(ranger)
rf <- ranger(obs ~ .,
             data = dat_in,
             num.trees = 1000)

predictions <- obs_info %>%
  mutate(var = metrics_sel,
         obs = dat_in$obs,
         pred = rf$predictions)
write_csv(predictions, paste0('output/',model_name,'/predictions.csv'))

#regression performance metrics
r2 <- function(pred, obs){
  ssr <- sum((obs-pred)^2, na.rm = T)
  obs_mean <- mean(obs, na.rm = T)
  sst <- sum((obs - obs_mean)^2, na.rm = T)
  
  r2 <- 1 - (ssr/sst)
  return(r2)
}
rmse <- function(pred, obs){
  se <- (pred-obs)^2
  rmse <- sqrt(mean(se, na.rm = T))
  return(rmse)
}
regress_performance <- function(pred, obs, wide = F){
  r2 <- r2(pred, obs)
  rmse <- rmse(pred, obs)
  pcor <- cor(pred, obs, use = 'pairwise.complete')
  
  out <- data.frame(
    var = c('r2', 'rmse', 'p_cor'),
    val = c(r2, rmse, pcor)
  )
  if(wide == T) out <- pivot_wider(out, names_from = var, values_from = val)
  
  return(out)
}

performance <- regress_performance(rf$predictions, dat_in$obs, wide = T) %>%
  mutate(var = metrics_sel,
         across(!var, ~round(.x, 3))) %>%
  select(var, everything())
write_csv(performance, paste0('output/',model_name,'/performance.csv'))


# shaps -------------------------------------------------------------------
require(shapr)
progressr::handlers(global=TRUE)


for(s in seq_along(unique(dat$site_no))){
  site_sel = unique(dat$site_no)[s]
  
  explain_dat <- filter(dat, site_no == site_sel) %>%
    select(!c(site_no, wateryear, all_of(metrics_sel)))
  explain_info <- filter(dat, site_no == site_sel) %>%
    select(site_no, wateryear)
  
  shap <- explain(model = rf,
                  x_explain = explain_dat,
                  x_train = select(dat, !c(site_no, wateryear, all_of(metrics_sel))),
                  approach = 'empirical',
                  phi0 = mean(dat[[metrics_sel]]),
                  iterative = T,
                  max_n_coalitions = 1000,
                  n_MC_samples = 250,
                  seed = 527,
                  verbose = c('basic','progress'),
                  output_args = list(saving_path = paste0('output/',model_name,'/intermediate')))
  
  shaps <- as_tibble(shap$shapley_values_est) %>%
    mutate(metric = metrics_sel) %>%
    cbind(explain_info)
  timing <- as_tibble(shap$timing$main_computation_timing_secs) %>%
    mutate(metric = metrics_sel,
           site_no = site_sel)
  mse <- as_tibble(shap$MSEv$MSEv_explicand$MSEv) %>%
    mutate(metric = metrics_sel) %>%
    cbind(explain_info)
  
  write_csv(shaps, paste0('output/',model_name,'/shaps/',site_sel,'.csv'))
  write_csv(timing, paste0('output/',model_name,'/timing/',site_sel,'.csv'))
  write_csv(mse, paste0('output/',model_name,'/mse/',site_sel,'.csv'))
  
  print(paste0('SITE ',s,'/',length(unique(dat$site_no)),' DONE!!!!!!'))
}

