# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('./scripts/Theme+Settings.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')

connections <- read_gage_info(type = 'connections') %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, hw_order = order)) %>%
  left_join(., select(ds_gage_info, downstream_id = site_no, ds_order = order)) %>%
  mutate(across(c(hw_order, ds_order), ~ifelse(.x == -99, 0, .x))) %>%
  mutate(connection_id = 1:nrow(.))
connections_fil <- filter(connections, headwater_id %in% hw_gage_info$site_no)

window_size <- 3
trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_window',window_size,'.csv'))

# trends_fil <- filter(trends, site_no %in% ds_gage_info_fil$site_no)

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 
                'Q_frequency_high_2', 'Q_frequency_noflow',
                'Q_totalduration_high_2', 'Q_totalduration_noflow',
                'HFD_mean', 'HFI_mean', 'peakQ_timing', 
                'BFI', 'FlashinessIndex', 'FDC_slope',
                'BaseflowRecessionK', 'Recession_a_Seasonality')
metrics_labels <- c('Mean Annual Q', 'Q5', 'Q95', 'Runoff Ratio',
                   '# High-Flow Peaks', '# No-Flow Periods',
                   'High-Flow Duration', 'No-Flow Duration',
                   'Half-Flow Date', 'Half-Flow Interval', 'Peak Q Date',
                   'Baseflow Index', 'Flashiness Index', 'FDC Mid-Section Slope',
                   'Recession Constant', 'Recession Seasonality')

# raw trends map ----------------------------------------------------------
# metric_sel = 'BaseflowRecessionK'
# metric_label = 'Recession Constant'
sig_var <- 'sen_sig2a'
sig_type <- 'sig2a'
set = 'hworder2'

if(set == 'hw'){gage_info <- hw_gage_info
                set_label <- 'Headwaters'}

if(set == 'hworder2'){gage_info <- filter(hw_gage_info, order %in% 1:2)
                      set_label <- 'Headwaters, Order <= 2'}

if(set == 'ds'){gage_info <- ds_gage_info
                set_label <- 'Downstream'}

if(set == 'dsorder2'){connections_order2 <- filter(connections, hw_order %in% 1:2)
                      gage_info <- filter(ds_gage_info, site_no %in% connections_order2$downstream_id)
                      set_label <- 'Downstream, HW Order <= 2'}

for(i in 1:length(metrics_sel)){
  plot_dat <- trends %>%
    filter(var == metrics_sel[i] & site_no %in% gage_info$site_no) %>%
    left_join(., gage_info) %>%
    st_as_sf(coords = c('lon','lat'), crs = 4269) %>%
    st_transform(., 5070) %>%
    mutate(across(contains('sig'), ~replace_na(.x, 'none'))) %>%
    mutate(across(contains('sig'), ~factor(.x, levels = c('none','neg','pos')))) %>%
    rename(trend = any_of(sig_var)) %>%
    arrange(trend)
  
  ggplot() +
    geom_sf(data = states, linewidth = 0.5) +
    geom_sf(data = plot_dat, aes(color = trend), size = 1.2) +
    scale_color_manual(values = c('black','red','blue'),
                       limits = c('none','neg','pos'),
                       labels = c('Non-significant','Negative','Positive'),
                       name = NULL, guide = NULL) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = 'none',
          text = element_text(size = 12)) +
    ggtitle(paste0(metrics_labels[i],', ',set_label))
  
  ggsave(paste0('figures/data_maps/metric_trends/metrics_',sig_type,'/',set,'/',metrics_sel[i],'_sig.png'), height = 3.6, width = 6, units = 'in')
}


# calc trend diffs --------------------------------------------------------
sig_type <- 'sig2a'

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

# trend diffs map ---------------------------------------------------------
# metric_sel = 'Q_mean'
# metric_label = 'Mean Annual Q'

for(i in 1:length(metrics_sel)){
map_dat <- trend_diffs %>%
  filter(var_hw == metrics_sel[i]) %>%
  rename(site_no = headwater_id) %>%
  left_join(., hw_gage_info) %>%
  st_as_sf(coords = c('lon','lat'), crs = 4269) %>%
  st_transform(., 5070) %>%
  mutate(comp = factor(comp, levels = c('none', 'opposite', 'same', 'ds_only', 'hw_only'))) %>%
  arrange(comp) 

ggplot() +
  geom_sf(data = states, linewidth = 0.5) +
  geom_sf(data = map_dat, aes(color = comp), size = 1.6) +
  # geom_sf_label(data = map_dat, aes(label = connection_id), size = 3) +
  scale_color_manual(limits = c('none', 'opposite', 'same', 'ds_only', 'hw_only'),
                     values = c('black', 'yellow', '#00BA38', '#619CCf', '#F8766D')) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        legend.position = 'none',
        text = element_text(size = 12)) +
  ggtitle(paste0(metrics_labels[i]))

ggsave(paste0('figures/data_maps/metric_trend_diffs/',sig_type,'/',metrics_sel[i],'_diffs.png'), height = 3.6, width = 6, units = 'in')
}
