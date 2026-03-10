# libraries and data ------------------------------------------------------
library(tidyverse)
library(ranger)
library(treeshap)
library(shapr)
library(future)
library(progressr)
slurm_cores = as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK'))
print(paste('CPUs assigned by SLURM:',slurm_cores))
plan(multicore, workers = slurm_cores)
print(paste('Using',availableCores('multicore'),'cores'))

all_gage_info <- read_csv('input_data/all_gage_info.csv')
metrics <- read_csv('input_data/metrics_window3.csv') %>%
  select(!contains('error_str'))
preds_temporal <- read_csv('input_data/pred_timeseries_window3.csv') 
preds_static <- read_csv('input_data/pred_statics.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)

metrics_sel <- Sys.getenv('metrics_sel')
region_sel <- Sys.getenv('region_sel')

print(paste('Starting model for metric:',metrics_sel))
print(paste('in region:',region_sel))

model_name <- paste0(tolower(metrics_sel), '_annual_treeshap_ind')
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
write_csv(predictions, paste0('output/',model_name,'/predictions_raw.csv'))

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
require(treeshap)
# progressr::handlers(global=TRUE)

sites_sel <- all_gage_info$site_no
if(region_sel != 'all'){
  sites_sel <- filter(all_gage_info, region %in% region_sel)$site_no
}
sites_sel <- sites_sel[sites_sel %in% dat$site_no]

explain_dat <- filter(dat, !is.na(.data[[metrics_sel]])) %>%
  select(all_of(c(metrics_sel, preds_temporal_sel, preds_static_sel))) %>%
  filter(if_all(everything(), ~!is.na(.x))) %>%
  rename(obs = 1)
explain_info <- filter(dat, !is.na(.data[[metrics_sel]])) %>%
  select(site_no, wateryear, all_of(c(metrics_sel, preds_temporal_sel, preds_static_sel))) %>%
  filter(if_all(everything(), ~!is.na(.x))) %>%
  select(site_no, wateryear)
shap_in <- ranger.unify(rf, explain_dat)

for(s in seq_along(sites_sel)){
  site_sel <- sites_sel[s]
  
  if(file.exists(paste0('output/',model_name,'/shaps/',site_sel,'.csv'))){
    print(paste0('Skipping site #',site_sel,' since it has already been run'))
    next
  } else
  print(paste0('Starting site #',site_sel,'at',Sys.time()))
  
  shap <- treeshap(shap_in, explain_dat[explain_info$site_no == site_sel,], interactions = F, verbose = F)
  
  shap_out <- shap$shaps %>%
    mutate(metric = metrics_sel,
           site_no = site_sel,
           wateryear = explain_info$wateryear[explain_info$site_no == site_sel])
  
  write_csv(shap_out, paste0('output/',model_name,'/shaps/',site_sel,'.csv'))
  
  print(paste('Site',site_sel,'done at',Sys.time()))
  print(paste0('(',s,'/',length(sites_sel),')'))
}