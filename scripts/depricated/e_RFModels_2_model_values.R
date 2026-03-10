# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/Theme+Settings.R')
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metrics <- read_csv('data/gages/metrics/merged/metrics_window3.csv') %>%
  select(!contains('error_str'))
preds_temporal <- read_csv('data/gages/predictors/pred_timeseries.csv') 
preds_static <- read_csv('data/gages/predictors/pred_statics.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)

# prep RF input -----------------------------------------------------------
model_name <- 'all_annual'

site_sel <- all_gage_info$site_no

#filter out values that are >3 standard deviations from the mean for each metric
#only has an effect if metric_trend_type == 'val'
trim_outliers = F

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR',
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing',
                 'BFI', 'FlashinessIndex', 'FDC_slope', 'BaseflowRecessionK', 'Recession_a_Seasonality')
metrics_sel <- colnames(select(metrics, !c(site_no, wateryear, contains('monthly'))))
metrics_sel <- 'Q_mean'


preds_temporal_sel = c('precip_annual','temp_annual','pet_annual', 'ppet_annual',
                       'precip_annual_prev','temp_annual_prev','pet_annual_prev', 'ppet_annual_prev',
                       'precip_jfm','temp_jfm','pet_jfm', 'ppet_jfm',
                       'precip_amj','temp_amj','pet_amj', 'ppet_amj',
                       'precip_jas','temp_jas','pet_jas', 'ppet_jas',
                       'precip_ond','temp_ond','pet_ond', 'ppet_ond',
                       'si',
                       'swe_annual','max_swe','max_swe_day', 'zero_swe_day', 'swe_persistence', 'melt_duration',
                       'ag','developed','forest','grass')
                       #'water_use')

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
  mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x)))

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
# metrics_sel <- 'Q_mean'
save = F

rf_list <- list()
pred_list <- list()

rf_performance <- tibble()

for(i in 1:length(metrics_sel)){
  metric_i <- metrics_sel[i]
  obs_info <- filter(dat, !is.na(.data[[metric_i]])) %>%
    select(site_no, wateryear, all_of(c(metric_i, preds_temporal_sel, preds_static_sel))) %>%
    filter(if_all(everything(), ~!is.na(.x))) %>%
    select(site_no, wateryear)
  dat_in <- filter(dat, !is.na(.data[[metric_i]])) %>%
    select(all_of(c(metric_i, preds_temporal_sel, preds_static_sel))) %>%
    filter(if_all(everything(), ~!is.na(.x))) %>%
    rename(obs = 1)
  
  # if(length(unique(dat_in$obs)) == 1){
  #   rf_list[[i]] <- NA
  #   rf_performance_i <- rep(NA, ncol(rf_performance))
  #   rf_performance <- rbind(rf_performance, rf_performance_i)
  #   next
  # } 
  
  set.seed(527)
  require(ranger)
  fit_rf <- ranger(obs ~ .,
                   data = dat_in,
                   num.trees = 1000)
  predictions <- fit_rf$predictions

  
  rf_list[[i]] <- fit_rf
  pred_list[[i]] <- obs_info %>%
    mutate(var = metric_i,
           obs = dat_in$obs,
           pred = predictions)
  
  rf_performance_i <- regress_performance(predictions, dat_in$obs, wide = T)
  rf_performance <- rbind(rf_performance, rf_performance_i)
  
  print(paste0('model ',i,'/',length(metrics_sel),' done!!'))
}

names(rf_list) <- metrics_sel
names(pred_list) <- metrics_sel
rf_performance <- rf_performance %>%
  mutate(var = metrics_sel,
         across(!var, ~round(.x, 3))) %>%
  select(var, everything())
rf_performance
if(save) write_csv(rf_performance, paste0('data/models/',model_name,'/performance.csv'))

predictions <- list_rbind(pred_list)
if(save) write_csv(predictions, paste0('data/models/',model_name,'/predictions.csv'))

# ggplot(data = subset(predictions), aes(x = obs, y = pred))+
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   geom_point() +
#   geom_abline(slope = 1)
# 
# asdf <- left_join(predictions, select(all_gage_info, site_no, region2, order, type))
# asdf %>%
#   group_by(order) %>%
#   summarise(r2 = r2(pred, obs))
# # ggplot(data = arrange(rf_performance, desc(r2)), aes(y=r2, x = 1:nrow(rf_performance))) +
#   geom_line() +
#   geom_hline(yintercept = 0.3)



# shaps -------------------------------------------------------------------
library(treeshap)
save = T
#only run SHAPs for models with reasonable performance, otherwise the results will
#be garbage anyway
perf_threshold <- 0.3
good_models <- which(rf_performance$r2 >= perf_threshold)

output <- data.frame()
for(i in 1:length(good_models)){
  model_number <- good_models[i]
  metric_i <- names(rf_list)[model_number]
  
  rf_sel <- rf_list[[metric_sel]]
  obs_info <- filter(dat, !is.na(.data[[metric_i]])) %>%
    select(site_no, all_of(c(metric_i, preds_temporal_sel, preds_static_sel))) %>%
    filter(if_all(everything(), ~!is.na(.x))) %>%
    select(site_no)
  dat_in <- filter(dat, !is.na(.data[[metric_i]])) %>%
    select(all_of(c(metric_i, preds_temporal_sel, preds_static_sel))) %>%
    filter(if_all(everything(), ~!is.na(.x))) %>%
    rename(obs = 1)
  rf_in <- ranger.unify(rf_sel, dat_in)
  time_start <- Sys.time()
  shap <- treeshap(rf_in, dat_in, interactions = F)
  time_end <- Sys.time()
  
  if(save){
    shaps <- shap$shaps %>%
      mutate(metric = metric_i,
             site_no = obs_info$site_no,
             type = 'shap')
    obs <- shap$observations %>%
      mutate(metric = metric_i,
             site_no = obs_info$site_no,
             type = 'obs')
    output_i <- rbind(shaps, obs)
    write_csv(output_i, paste0('data/models/',model_name,'/',metric_i,'_shaps.csv'))
    output <- rbind(output, output_i)
  }
  print(paste0('model ',i,'/',length(good_models),' done!!'))
  
}
if(save) write_csv(output, paste0('data/models/',model_name,'/shaps.csv'))

# plot_feature_importance(shap)
# plot_feature_dependence(shap, 'precip_mean')
# summary(dat_in$obs)