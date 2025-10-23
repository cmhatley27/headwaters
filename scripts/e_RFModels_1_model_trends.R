# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/Theme+Settings.R')
source('scripts/functions/utilities.R')

hw_gage_info <- read_gage_info('headwaters')
ds_gage_info <- read_gage_info('downstream')
connections <- read_gage_info('connections')

metrics <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv')
pred_trends <- read_csv('data/gages/predictors/pred_trends.csv')
pred_statics <- read_csv('data/gages/predictors/pred_statics.csv')

# prep RF input -----------------------------------------------------------
model_name <- 'all_senval'

site_sel <- unique(c(hw_gage_info$site_no, ds_gage_info$site_no))

#ranger or party
rf_method = 'ranger'

#pick tau, sen, or mw
metric_trend_var = 'sen'
#pick sig or val
metric_trend_type = 'val'

#pick tau, sen, or mw
pred_trend_var = 'sen'
#pick sig or val
pred_trend_type = 'val'

#filter out observations with non-significant trends. only has an effect if 
#metric_trend_type == 'sig'
#IF CALCULATING SHAP VALUES FOR RANGER CLASSIFICATION TREES, THIS MUST BE
#SET TO TRUE
filter_nonsig = T

#filter out trends that are >3 standard deviations from the mean for each metric
#only has an effect if metric_trend_type == 'val'
trim_outliers = T

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR',
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing',
                 'BFI', 'FlashinessIndex', 'FDC_slope', 'BaseflowRecessionK', 'Recession_a_Seasonality')
metrics_sel <- unique(metrics$var)

pred_trends_sel = c('precip_annual','temp_annual','pet_annual', #'ppet_annual',
                    'precip_jfm','temp_jfm','pet_jfm', #'ppet_jfm',
                    'precip_amj','temp_amj','pet_amj', #'ppet_amj',
                    'precip_jas','temp_jas','pet_jas', #'ppet_jas',
                    'precip_ond','temp_ond','pet_ond', #'ppet_ond',
                    'swe_annual','max_swe','max_swe_day', 'zero_swe_day', 'swe_persistence', 'melt_duration',
                    'ag','developed','forest','grass',
                    'water_use')

pred_statics_sel = c('drainage_area', 'elev', 'slope', 'twi',
                     'soil_perm', 'soil_awc',
                     'dist_index',
                     'age',
                     'precip_mean', 'temp_mean', 'pet_mean', 'q_norm_mean',
                     'water_use_mean')

trend_selector <- function(x, var, type){
  df <- select(x, all_of(contains(var)))
  if(type == 'sig') df <- select(df, ends_with('sig'))
  if(type == 'sig0') df <- select(df, contains('sig0'))
  if(type == 'sig2a') df <- select(df, contains('sig2a'))
  if(type == 'val') df <- select(df, !contains('sig'))
  df <- rename(df, obs = 1)
  return(df$obs)
}

metrics_in <- metrics %>%
  filter(var %in% metrics_sel) %>%
  filter(site_no %in% site_sel) %>%
  mutate(obs = trend_selector(., metric_trend_var, metric_trend_type)) %>%
  select(site_no, metric = var, obs) %>%
  if(filter_nonsig == TRUE) filter(., obs != 'none') else .

preds_in <- pred_trends %>%
  mutate(val = trend_selector(., pred_trend_var, pred_trend_type)) %>%
  select(site_no, var, val) %>%
  rbind(., pred_statics) %>%
  filter(var %in% c(pred_trends_sel, pred_statics_sel)) %>%
  filter(site_no %in% site_sel) %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)

dat <- left_join(metrics_in, preds_in)

dat <- dat %>%
  mutate(across(everything(), ~ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
  filter(!is.na(obs)) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

if(metric_trend_type %in% c('sig', 'sig0', 'sig2a')){
  if(rf_method == 'ranger'){
    dat <- mutate(dat,
                  obs = case_when(
                    obs == 'neg' ~ -1,
                    obs == 'pos' ~ 1,
                    obs == 'none' ~ 0))}
  if(rf_method == 'party'){
    dat$obs <- factor(dat$obs)}
} 
if(pred_trend_type %in% c('sig', 'sig0', 'sig2a')){
  dat <- mutate(dat, 
                across(all_of(c(pred_trends_sel)), ~factor(.x)),
                across(all_of(pred_statics_sel), ~as.numeric(.x)))
} 
  
if(trim_outliers == T & metric_trend_type == 'val'){
  dat_sd <- dat %>%
    group_by(metric) %>%
    summarise(u = mean(obs),
              sd = sqrt(var(obs)))
  dat <- left_join(dat, dat_sd) %>%
    filter(obs <= u+3*sd & obs >= u-3*sd) %>%
    select(!c(u,sd))
}

# run RF ------------------------------------------------------------------
# metrics_sel <- 'Q_mean'
save = T

rf_list <- list()
pred_list <- list()

rf_performance <- tibble()

for(i in 1:length(metrics_sel)){
  sites <- filter(dat, metric == metrics_sel[i])$site_no
  dat_in <- filter(dat, metric == metrics_sel[i]) %>%
    select(!c(site_no, metric))
  
  if(length(unique(dat_in$obs)) == 1){
    rf_list[[i]] <- NA
    rf_performance_i <- rep(NA, ncol(rf_performance))
    rf_performance <- rbind(rf_performance, rf_performance_i)
    next
  } 
  
  set.seed(527)
  if(rf_method == 'party'){
    require(party)
    fit_rf <- party::cforest(
      obs ~ .,
      data = dat_in,
      controls = cforest_control(
        ntree = 1000,
        mtry = round(sqrt(ncol(dat_in)))
      )
    )
    predictions <- predict(fit_rf, newdata = NULL, OOB = T)
  }
  
  if(rf_method == 'ranger'){
    require(ranger)
    fit_rf <- ranger(obs ~ .,
                     data = dat_in,
                     num.trees = 1000)
    predictions <- fit_rf$predictions
  }

  rf_list[[i]] <- fit_rf
  pred_list[[i]] <- data.frame(
    obs = dat_in$obs,
    pred = predictions,
    var = metrics_sel[i],
    site_no = sites
  )
  
  if(metric_trend_type %in% c('sig', 'sig0', 'sig2a')){
    if(rf_method == 'party'){
      predictions <- factor(predictions, levels = levels(dat_in$obs))
      rf_performance_i <- class_performance(predictions, dat_in$obs, wide = T)}
    if(rf_method == 'ranger'){
      predictions = ifelse(predictions >= 0, 1, -1)
      predictions = factor(predictions, levels = c(-1, 1), labels = c('neg', 'pos'))
      dat_in$obs = factor(dat_in$obs, levels = c(-1,1), labels = c('neg', 'pos'))
      rf_performance_i <- class_performance(predictions, dat_in$obs, wide = T)}
    }
  if(metric_trend_type == 'val'){
    rf_performance_i <- regress_performance(predictions, dat_in$obs, wide = T)}
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
if(save) write_csv(rf_performance, paste0('data/models/performance/',model_name,'_summary.csv'))

predictions <- list_rbind(pred_list)
if(save) write_csv(predictions, paste0('data/models/performance/',model_name,'_predictions.csv'))

# ggplot(data = subset(predictions, var == 'Q5'), aes(x = obs, y = pred))+
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   geom_point() +
#   geom_abline(slope = 1)


# shap --------------------------------------------------------------------
library(treeshap)
save = T

output <- data.frame()
for(i in 1:length(rf_list)){
  metric_sel <- names(rf_list)[i]

  rf_sel <- rf_list[[metric_sel]]
  sites <- filter(dat, metric == metric_sel)$site_no
  dat_in <- filter(dat, metric == metric_sel) %>%
    select(!c(site_no, metric))
  rf_in <- ranger.unify(rf_sel, dat_in)
  time_start <- Sys.time()
  shap <- treeshap(rf_in, dat_in, interactions = F)
  time_end <- Sys.time()

  if(save){
    shaps <- shap$shaps %>%
      mutate(metric = metric_sel,
             site_no = sites,
             type = 'shap')
    obs <- shap$observations %>%
      mutate(metric = metric_sel,
             site_no = sites,
             type = 'obs')
    output_i <- rbind(shaps, obs)
    output <- rbind(output, output_i)
  }
  print(paste0('model ',i,'/',length(rf_list),' done!!'))
}
if(save) write_csv(output, paste0('data/models/shaps/',model_name,'_shaps.csv'))

# plot_feature_importance(shap)
# plot_feature_dependence(shap, 'precip_mean')
# summary(dat_in$obs)


# variable importance -----------------------------------------------------
# library(permimp)
# 
# varimps <- list()
# for(i in 1:length(rf_list)){
#   dat_in <- filter(dat, metric == names(rf_list)[i]) %>%
#     select(!c(site_no, metric))
#   rf_party <- party::cforest(obs ~ .,
#                              data = dat_in,
#                              controls = cforest_control(
#                                ntree = 1000,
#                                mtry = round(sqrt(ncol(dat_in)))))
#   varimp <- permimp(rf_party)
#   varimp_df <- tibble(
#     metric = names(varimp$values),
#     imp = varimp$values) %>%
#     mutate(rank = rank(imp*-1),
#            imp_norm = imp/max(imp)) %>%
#     arrange(imp) %>%
#     mutate(metric = factor(metric, levels = metric))
#   
#   varimps[[i]] <- varimp_df
# }
# names(varimps) <- metrics_sel[1]
# 
# ggplot(data = subset(varimps$Q_mean), aes(x = imp_norm, y = metric)) +
#   geom_col()
