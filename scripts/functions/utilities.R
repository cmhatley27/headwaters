require(tidyverse)
require(zoo)
require(modifiedmk)

#negate %in%
`%nin%` <- negate(`%in%`)

#read info for selected gages. Headwaters selected in script 2_subset_headwaters,
#downstream gages and headwater-downstream connections selected in script 3_select_downstream_gages
read_gage_info <- function(type = 'headwaters'){
  #type = c('headwaters', 'downstream', 'connections)
  files <- c('headwaters' = 'hw_gage_info.csv',
             'downstream' = 'ds_gage_info.csv',
             'connections' = 'hw_ds_connections.csv')
  
  info_path <- file.path('data', 'gages', files[names(files) == type])
  
  gage_info <- read_csv(info_path)
  
  return(gage_info)
}

#quickly save the most recent plot
qsave <- function(w = 7,h = 3.5){
  return(ggsave('figures/temp.png',width = w, height = h,dpi = 700))
}

#gather individual .csv's for each gage into one. Assumes gages are identified
#by 'site_no'
gather_loose <- function(path){
  files <- list.files(path, full.names = T)
  out <- map(files, ~read_csv(.x, col_types = cols(site_no = 'c'))) %>%
    list_rbind()
  return(out)
}

#calculate modified trend statistics using the 'modifiedmk' library
trendinator <- function(x, length_thresh = 5){
  x_len <- sum(!is.na(x) & !is.nan(x))
  
  if(x_len < length_thresh){
    return(data.frame(tau = NA, sen = NA, p = NA, p0 = NA,
                      nn = NA, s = NA, s0 = NA))
  }
  
  trends <- tryCatch(mmkh3lag(x), error = function(e) NA) %>%
    round(., 5)
  
  if(length(trends) == 1){
    return(data.frame(tau = NA, sen = NA, p = NA, p0 = NA,
                      nn = NA, s = NA, s0 = NA))
  }

  out <- data.frame(t(trends)) %>%
    select(tau = Tau, sen = Sen.s.slope, p = new.P.value, p0 = old.P.value,
           nn = N.N., s = new.variance, s0 = old.variance)
  return(out)
}

#Classify trends based on significance and direction
trend_classifier <- function(x, p, alpha = 0.05){
  classes <- case_when(
    is.na(x) ~ NA,
    p > alpha ~ 'none',
    x > 0 & p <= alpha ~ 'pos',
    x < 0 & p <= alpha ~ 'neg',
    x == 0 ~ 'none',
    .default = NA)
  return(classes)
}

#classifier performance metrics
class_performance <- function(pred, obs, wide = F){
  m <- table(pred, obs)
  
  n <- sum(m)
  prevalence <- colSums(m)
  correct <- diag(m)
  
  acc <- sum(correct)/n
  recalls <- correct/prevalence
  macro_recall <- mean(recalls, na.rm = T)
  
  out <- data.frame(
    var = c('acc', 'mrc', paste0('rc_',names(recalls))),
    val = c(acc, macro_recall, recalls)
  )
  if(wide == T) out <- pivot_wider(out, names_from = var, values_from = val)
  
  return(out)
}

#KGE
kge <- function(x,y, modified = T){
  r = cor(x,y,use = 'pairwise.complete')
  ux = mean(x, na.rm = T)
  uy = mean(y, na.rm = T)
  sx = sqrt(var(x, na.rm = T))
  sy = sqrt(var(y, na.rm = T))
  
  b = uy/ux
  g = sy/sx
  if(modified == T){g = (sy/uy)/(sx/ux)}
  
  t1 = (r-1)^2
  t2 = (b-1)^2
  t3 = (g-1)^2
  kge = 1-sqrt(t1+t2+t3)
  return(kge)
}

#R2
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

#regression performance metrics
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

#holds all metric names for plot labelling
metric_labeller <- function(metrics, label_thresholds = F){
  labels <- c(
    'Mean Annual Q' = 'Q_mean',
    'Mean Jan Q' = 'Q_mean_monthly_1',
    'Mean Feb Q' = 'Q_mean_monthly_2',
    'Mean Mar Q' = 'Q_mean_monthly_3',
    'Mean Apr Q' = 'Q_mean_monthly_4',
    'Mean May Q' = 'Q_mean_monthly_5',
    'Mean Jun Q' = 'Q_mean_monthly_6',
    'Mean Jul Q' = 'Q_mean_monthly_7',
    'Mean Aug Q' = 'Q_mean_monthly_8',
    'Mean Sep Q' = 'Q_mean_monthly_9',
    'Mean Oct Q' = 'Q_mean_monthly_10',
    'Mean Nov Q' = 'Q_mean_monthly_11',
    'Mean Dec Q' = 'Q_mean_monthly_12',
    'Q5' = 'Q5',
    'Q10' = 'Q10',
    'Q90' = 'Q90',
    'Q95' = 'Q95',
    'Q95 - Q10' = 'Q95_minus_Q10',
    'Runoff Ratio' = 'TotalRR',
    '# High Flow Events (>3x)' = 'Q_frequency_high_1',
    '# High Flow Events (>6x)' = 'Q_frequency_high_2',
    '# High Flow Events (>9x)' = 'Q_frequency_high_3',
    '# High Flow Events (>12x)' = 'Q_frequency_high_4',
    '# High Flow Events (>15x)' = 'Q_frequency_high_5',
    '# Low Flow Events (<0.75x)' = 'Q_frequency_low_1',
    '# Low Flow Events (<0.67x)' = 'Q_frequency_low_2',
    '# Low Flow Events (<0.5x)' = 'Q_frequency_low_3',
    '# Low Flow Events (<0.33x)' = 'Q_frequency_low_4',
    '# Low Flow Events (<0.25x)' = 'Q_frequency_low_5',
    '# No Flow Events' = 'Q_frequency_noflow',
    'Duration High Flow (>3x)' = 'Q_totalduration_high_1',
    'Duration High Flow (>6x)' = 'Q_totalduration_high_2',
    'Duration High Flow (>9x)' = 'Q_totalduration_high_3',
    'Duration High Flow (>12x)' = 'Q_totalduration_high_4',
    'Duration High Flow (>15x)' = 'Q_totalduration_high_5',
    'Duration Low Flow (<0.75x)' = 'Q_totalduration_low_1',
    'Duration Low Flow (<0.67x)' = 'Q_totalduration_low_2',
    'Duration Low Flow (<0.5x)' = 'Q_totalduration_low_3',
    'Duration Low Flow (<0.33x)' = 'Q_totalduration_low_4',
    'Duration Low Flow (<0.25x)' = 'Q_totalduration_low_5',
    'Duration No Flow' = 'Q_totalduration_noflow',
    'Mean Duration High Flow Events (>3x)' = 'Q_meanduration_high_1',
    'Mean Duration High Flow Events (>6x)' = 'Q_meanduration_high_2',
    'Mean Duration High Flow Events (>9x)' = 'Q_meanduration_high_3',
    'Mean Duration High Flow Events (>12x)' = 'Q_meanduration_high_4',
    'Mean Duration High Flow Events (>15x)' = 'Q_meanduration_high_5',
    'Mean Duration Low Flow Events (<0.75x)' = 'Q_meanduration_low_1',
    'Mean Duration Low Flow Events (<0.67x)' = 'Q_meanduration_low_2',
    'Mean Duration Low Flow Events (<0.5x)' = 'Q_meanduration_low_3',
    'Mean Duration Low Flow Events (<0.33x)' = 'Q_meanduration_low_4',
    'Mean Duration Low Flow Events (<0.25x)' = 'Q_meanduration_low_5',
    'Mean Duration No Flow Events' = 'Q_meanduration_noflow',
    'Half Flow Date' = 'HFD_mean',
    'Half Flow Interval' = 'HFI_mean',
    'Max Q Date' = 'peakQ_timing',
    'Baseflow Index' = 'BFI',
    'Flashiness Index' = 'FlashinessIndex',
    'Rising Limb Density' = 'RLD',
    'FDC Midsection Slope' = 'FDC_slope',
    'Recession Constant' = 'BaseflowRecessionK',
    'Recession Seasonality' = 'Recession_a_Seasonality'
  )
  match_order <- match(metrics, labels)
  labels_sel <- names(labels)[match_order]
  return(labels_sel)
}

#holds all predictor names for plot labelling
pred_labeller <- function(preds){
  labels <- c(
    'Precip (annual)' = 'precip_annual',
    'Temp (annual)' = 'temp_annual',
    'ETo (annual)' = 'pet_annual',
    'Precip (winter)' = 'precip_jfm',
    'Temp (winter)' = 'temp_jfm',
    'ETo (winter)' = 'pet_jfm',
    'P/ETo (winter)' = 'ppet_jfm',
    'Precip (spring)' = 'precip_amj',
    'Temp (spring)' = 'temp_amj',
    'ETo (spring)' = 'pet_amj',
    'P/ETo (spring)' = 'ppet_amj',
    'Precip (summer)' = 'precip_jas',
    'Temp (summer)' = 'temp_jas',
    'ETo (summer)' = 'pet_jas',
    'P/ETo (summer)' = 'ppet_jas',
    'Precip (fall)' = 'precip_ond',
    'Temp (fall)' = 'temp_ond',
    'ETo (fall)' = 'pet_ond',
    'P/ETo (fall)' = 'ppet_ond',
    'SWE Max' = 'max_swe',
    'SWE Max Day' = 'max_swe_day',
    'SWE Annual Total' = 'swe_annual',
    'SWE Persistence' = 'swe_persistence',
    'SWE Zero Day' = 'zero_swe_day',
    'Snowmelt Duration' = 'melt_duration',
    '% Agriculture' = 'ag',
    '% Developed' = 'developed',
    '% Forest' = 'forest',
    '% Grassland' = 'grass',
    'Water Use' = 'water_use',
    'Drainage Area' = 'drainage_area',
    'Mean Elevation' = 'elev',
    'Mean Slope' = 'slope',
    'TWI' = 'twi',
    'Soil Permeability' = 'soil_perm',
    'Soil AWC' = 'soil_awc',
    'Disturbance Index' = 'dist_index',
    'Geologic Age' = 'age',
    'Mean Precip' = 'precip_mean',
    'Mean Temp' = 'temp_mean',
    'Mean ETo' = 'pet_mean',
    'Mean Q' = 'q_norm_mean',
    'Mean Water Use' = 'water_use_mean'
  )
  match_order <- match(preds, labels)
  labels_sel <- names(labels)[match_order]
  return(labels_sel)
}
