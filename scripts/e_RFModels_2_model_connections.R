# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')

hw_gage_info <- read_gage_info('headwaters')
ds_gage_info <- read_gage_info('downstream')
connections <- read_gage_info('connections')

metrics <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv')
pred_trends <- read_csv('data/gages/predictors/pred_trends.csv')
pred_statics <- read_csv('data/gages/predictors/pred_statics.csv')

# function for splitting out variables by headwater/downstream for connection comparison
hw_ds_splitter <- function(x){
  out <- tibble()
  vars <- unique(x$var)
  for(i in 1:length(vars)){
    var_i = vars[i]
    hw_dat <- filter(x, var == var_i & site_no %in% hw_gage_info$site_no)
    ds_dat <- filter(x, var == var_i & site_no %in% ds_gage_info$site_no)
    out_i <- left_join(connections, hw_dat, by = join_by(headwater_id == site_no)) %>%
      left_join(., ds_dat, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds'))
    
    out <- rbind(out, out_i)
  }
  return(out)
}

connection_metrics <- hw_ds_splitter(metrics)
connection_pred_trends <- hw_ds_splitter(pred_trends)
connection_pred_statics <- hw_ds_splitter(pred_statics)


# set up rf input ---------------------------------------------------------

#filter out observations with non-significant trends. only has an effect if 
#metric_trend_type == 'sig'
filter_nonsig = F

#include trends in headwater sites and trends in downstream sites as separate
#predictors. If false, trends in each headwater-downstream connection will be
#summarized as the ratio between the two (hw/ds).
split_preds = T

#pick tau, sen, or mw
metric_trend_var = 'sen'
#pick sig or val
metric_trend_type = 'sig'

#pick tau, sen, or mw
pred_trend_var = 'sen'
#pick sig or val
pred_trend_type = 'val'

metrics_sel = c('Q_mean', 'Q95', 'Q10', 'TotalRR', 'Q_frequency_high_3', 'Q_frequency_noflow',
                'Q_totalduration_high_3', 'Q_totalduration_noflow',
                'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                'BaseflowRecessionK', 'Recession_a_Seasonality')

pred_trends_sel = c('precip_annual','temp_annual','pet_annual',
                    'precip_jfm','temp_jfm','pet_jfm',
                    'precip_amj','temp_amj','pet_amj',
                    'precip_jas','temp_jas','pet_jas',
                    'precip_ond','temp_ond','pet_ond',
                    'swe_annual','max_swe','max_swe_day',
                    'ag','developed','forest','grass',
                    'water_use')

pred_statics_sel = c('drainage_area', 'elev', 'slope', 'twi',
                     'soil_perm', 'soil_awc',
                     'dist_index',
                     'age')

#function for calculating a comparison of headwater and downstream trends
trend_comparinator <- function(x, var, type){
  #parameter 'var' selects which trend metric (tau, sen, or mw), while 'type'
  #selects which version of that metric to compare. 'Val' compares the raw values
  #of the trend metrics as the ratio of headwater/downstream, while 'sig' sets
  #categories based on the significance levels and directions of the headwater
  #and downstream trends.
  df <- select(x, contains(var) & !contains('p'))
  if(type == 'sig'){
    df <- select(df, contains('sig_')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = case_when(
        is.na(hw) | is.na(ds) ~ NA,
        hw == 'none' & ds == 'none' ~ 'none',
        hw == ds ~ 'same',
        hw %in% c('pos', 'neg') & ds == 'none' ~ 'hw_only',
        hw == 'none' & ds %in% c('pos', 'neg') ~ 'ds_only',
        hw == 'pos' & ds == 'neg' ~ 'opposite',
        hw == 'neg' & ds == 'pos' ~ 'opposite'
      ))
  }
  if(type == 'sig0'){
    df <- select(df, contains('sig0')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = case_when(
        is.na(hw) | is.na(ds) ~ NA,
        hw == 'none' & ds == 'none' ~ 'none',
        hw == ds ~ 'same',
        hw %in% c('pos', 'neg') & ds == 'none' ~ 'hw_only',
        hw == 'none' & ds %in% c('pos', 'neg') ~ 'ds_only',
        hw == 'pos' & ds == 'neg' ~ 'opposite',
        hw == 'neg' & ds == 'pos' ~ 'opposite'
      ))
  }
  if(type == 'val'){
    df <- select(df, !contains('sig')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = hw/ds)
  }
  return(df$comp)
}

metrics_in <- connection_metrics %>%
  filter(var_hw %in% metrics_sel) %>%
  mutate(obs = trend_comparinator(., metric_trend_var, metric_trend_type)) %>%
  select(headwater_id, downstream_id, metric = var_hw, obs) %>%
  if(filter_nonsig == TRUE) filter(., obs != 'none') else .

if(split_preds == F){
  pred_trends_in <- connection_pred_trends %>%
    filter(var_hw %in% pred_trends_sel) %>%
    mutate(val = trend_comparinator(., pred_trend_var, pred_trend_type)) %>%
    select(headwater_id, downstream_id, var = var_hw, val)
  pred_statics_in <- connection_pred_statics %>%
    filter(var_hw %in% pred_statics_sel) %>%
    mutate(val = val_hw/val_ds) %>%
    select(headwater_id, downstream_id, var = var_hw, val)
  pred_in <- rbind(pred_trends_in, pred_statics_in) %>%
    pivot_wider(id_cols = c(headwater_id, downstream_id), names_from = var, values_from = val)
  
  dat <- left_join(metrics_in, pred_in)
}

if(split_preds == T){
  pred_in <- pred_trends %>%
    filter(var %in% pred_trends_sel) %>%
    {if(pred_trend_type == 'val'){
      select(., site_no, var, contains(pred_trend_var) & !contains(c('p', 'sig')))
    } else .} %>%
    {if(pred_trend_type == 'sig'){
      select(., site_no, var, contains(pred_trend_var) & contains('sig_'))
    } else .} %>% 
    {if(pred_trend_type == 'sig0'){
      select(., site_no, var, contains(pred_trend_var) & contains('sig0'))
    } else .} %>% 
    rename(val = 3) %>%
    rbind(pred_statics) %>%
    filter(var %in% c(pred_trends_sel, pred_statics_sel)) %>%
    pivot_wider(id_cols = site_no, names_from = 'var', values_from = 'val')
  
  dat <- metrics_in %>%
    left_join(pred_in, by = join_by(headwater_id == site_no)) %>%
    left_join(pred_in, by = join_by(downstream_id == site_no), suffix = c('_hw', '_ds'))
}

dat <- dat %>%
  mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
  filter(!is.na(obs))

if(metric_trend_type %in% c('sig', 'sig0')) dat$obs <- factor(dat$obs)
if(pred_trend_type %in% c('sig', 'sig0')) dat <- mutate(dat, 
                                                        across(contains(pred_trends_sel), ~factor(.x)),
                                                        across(contains(pred_statics_sel), ~as.numeric(.x)))

# run rf ------------------------------------------------------------------
library(partykit)
metrics_sel <- c('Q_mean')

rf_list <- list()

rf_performance <- tibble()

for(i in 1:length(metrics_sel)){
  dat_in <- filter(dat, metric == metrics_sel[i]) %>%
    select(!c(headwater_id, downstream_id, metric))
  
  set.seed(527)
  fit_rf <- partykit::cforest(
    obs ~ .,
    data = dat_in,
    ntree = 1000
  )
  predictions <- partykit::predict.cforest(fit_rf, OOB = T)
  
  rf_list[[i]] <- fit_rf
  rf_list[[i]]$comp <- data.frame(
    obs = dat_in$obs,
    pred = predictions
  )

  if(metric_trend_type %in% c('sig', 'sig0')){ 
    predictions <- factor(predictions, levels = levels(dat_in$obs))
    rf_performance_i <- class_performance(predictions, dat_in$obs, wide = T)}
  if(metric_trend_type == 'val'){
    rf_performance_i <- regress_performance(predictions, dat_in$obs, wide = T)}
  rf_performance <- rbind(rf_performance, rf_performance_i)
  
  print(paste0('model ',i,'/',length(metrics_sel),' done!!'))
}
names(rf_list) <- metrics_sel
rf_performance <- rf_performance %>%
  mutate(var = metrics_sel) %>%
  select(var, everything())
rf_performance


# plot results ------------------------------------------------------------

asdf <- rf_list$Q_mean$comp
ggplot(data = subset(asdf, obs^2 <= 25), aes(x = obs, y = pred)) +
  geom_point() +
  geom_abline(slope = 1)
