# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('./scripts/Theme+Settings.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
hw_gage_info_fil <- filter(hw_gage_info, order <= 3, dist_index <= 56)

ds_gage_info <- read_gage_info(type = 'downstream')

window_type <- 'window' #window (overlapping) or fixed (nonoverlapping)
window_size <- 3 #fixed can only take 3, window can take 1, 3, or 5

trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_',window_type,window_size,'.csv'))

#predictors:
# trends <- read_csv(paste0('./data/gages/predictors/pred_trends_window',window_size,'.csv'))
# metrics_sel <- c('precip_annual','temp_annual','pet_annual','ppet_annual',
#                   'precip_jfm','temp_jfm','pet_jfm','ppet_jfm',
#                   'precip_amj','temp_amj','pet_amj','ppet_amj',
#                   'precip_jas','temp_jas','pet_jas','ppet_jas',
#                   'precip_ond','temp_ond','pet_ond','ppet_ond',
#                   'ag','developed','forest','grass','barren','water',
#                   'water_use')


# plot # sigs by metric ---------------------------------------------------
site <- 'hw' #hw or ds
orders <- 1:3
sig <- 'sig2a' #sig for autocorrelation-corrected p<0.05, sig2a for corrected p<0.1, sig0 for non-corrected p<0.05
metric_set <- '16var'
save <- F
fig_height <- 7.5 #7.5 works for 16 metrics, 4 for 6 metrics
fig_width <- 6 #seems to be good for all metric sets
fig_title <- 'Trend significane at headwater gages'

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
# metrics_sel <- c('Q_mean', 'Q5', 'Q95',
#                  'Q_frequency_high_2', 'HFD_mean',  'FlashinessIndex'
metrics_sel <- c('Q_mean', 'Q5', 'Q95', trends$var[str_detect(trends$var, 'totalduration')], trends$var[str_detect(trends$var, 'frequency')])

fig_name <- paste0(site,'_','order',max(orders),'_',sig,'_',window_type,window_size,'_',metric_set,'_','trends.png')

site_sel <- if(site == 'hw') hw_gage_info else ds_gage_info
site_sel <- filter(site_sel, order %in% orders)$site_no

trends_fil <- filter(trends, site_no %in% site_sel & var %in% metrics_sel) %>%
  mutate(across(contains('_sig'), ~replace_na(.x, 'none')))

trends_by_metric <- as.data.frame(table(select(trends_fil, var, trend = paste0('sen_',sig)))) %>%
  mutate(var = factor(var, levels = metrics_sel, labels = metric_labeller(metrics_sel)),
         trend = factor(trend, levels = c('none','pos','neg'), labels = c('Nonsignificant', 'Positive', 'Negative')))
ggplot(trends_by_metric, aes(y = var, x = Freq/length(site_sel), fill = trend)) +
  geom_col(color = 'black') +
  scale_fill_manual(values = c('grey90','blue2','red2'), name = 'Mann-Kendall Trend') +
  scale_x_continuous(name = paste0('Percent of Gages (n = ',length(site_sel),')'), position = 'bottom',
                     breaks = c(0,0.25,0.5,0.75,1), labels = ~.x*100) +
  scale_y_discrete(name = NULL, limits = ~rev(.x)) +
  ggtitle(fig_title) +
  theme(#axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45),
        text = element_text(size = 14),
        legend.position = 'none')

if(save == T){
  ggsave(paste0('figures/bar_stacks/',fig_name), height = fig_height, width = fig_width, units = 'in')
}

# plot hw-ds differences --------------------------------------------------
sig_type = 'sig2a'
save = T

connections <- read_gage_info(type = 'connections') %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, hw_order = order)) %>%
  left_join(., select(ds_gage_info, downstream_id = site_no, ds_order = order)) %>%
  mutate(across(c(hw_order, ds_order), ~ifelse(.x == -99, 0, .x))) %>%
  mutate(connection_id = 1:nrow(.))
connections_fil <- filter(connections, headwater_id %in% hw_gage_info_fil$site_no)

connections_trends <- tibble()
for(i in 1:length(metrics_sel)){
  metric = metrics_sel[i]
  hw_dat <- filter(trends, var == metric & site_no %in% hw_gage_info$site_no)
  ds_dat <- filter(trends, var == metric & site_no %in% ds_gage_info$site_no)
  connection_i <- left_join(connections_fil, hw_dat, by = join_by(headwater_id == site_no)) %>%
    left_join(., ds_dat, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds'))
  
  connections_trends <- rbind(connections_trends, connection_i)
}
connections_trends <- mutate(connections_trends,
                             across(contains('_sig'), ~replace_na(.x, 'none')))

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
  if(type == '9class'){
    df <- select(df, contains('sig_')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = case_when(
        is.na(hw) | is.na(ds) ~ NA,
        hw == 'none' & ds == 'none' ~ 'none',
        hw == 'pos' & ds == 'pos' ~ 'both_pos',
        hw == 'neg' & ds == 'neg' ~ 'both_neg',
        hw == 'pos' & ds == 'none' ~ 'hw_pos',
        hw == 'neg' & ds == 'none' ~ 'hw_neg',
        hw == 'none' & ds == 'pos' ~ 'ds_pos',
        hw == 'none' & ds == 'neg' ~ 'ds_neg',
        hw == 'pos' & ds == 'neg' ~ 'opposite_hw_pos',
        hw == 'neg' & ds == 'pos' ~ 'opposite_hw_neg'
      ))
  }
  if(type == 'sig2a'){
    df <- select(df, contains('sig2a')) %>%
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
    df <- select(df, !contains('trend')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = hw/ds)
  }
  return(df$comp)
}

trend_diffs <- connections_trends %>%
  mutate(comp = trend_comparinator(connections_trends, 'sen', sig_type))

comp_by_metric <- as.data.frame(table(select(trend_diffs, var_hw, comp))) %>%
  mutate(comp = factor(comp, levels = c('none', 'opposite', 'same', 'ds_only', 'hw_only')),
         var_hw = factor(var_hw, levels = metrics_sel, labels = metric_labels))

#sig difference bars
ggplot(comp_by_metric, aes(y = var_hw, x = Freq/nrow(connections_fil), fill = comp)) +
  geom_col(color = 'black') +
  scale_y_discrete(limits = ~rev(.x), name = NULL) +
  scale_x_continuous(name = paste0('Percent of HW-DS Pairs, n = ',nrow(connections)),
                     labels = ~.x*100) +
  scale_fill_manual(values = c('grey90', 'yellow', '#00BA38', '#619CCf', '#F8766D')) +
  theme(legend.position = 'none',
        text = element_text(size = 14)) +
  ggtitle('Trend sig. comparision at HW-DS pairs')
#7.5 x 6.1 for full stack
#4 x 6 for trimmed metric set
if(save == T){
  ggsave(paste0('figures/bar_stacks/connection_diffs_',window_type,window_size,'_',sig,'.png'), height = 7.5, width = 6.1, units = 'in')
}
