# data --------------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
library(sf)
source('scripts/functions/load_states.R')
source('scripts/functions/utilities.R')
source('scripts/Theme+Settings.R')
options(scipen = 999) #disable scientific notation

model_name <- 'hw_order2_senval'
performance_metric = 'r2'
set_label <- 'Headwaters'

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
gage_coords <- rbind(hw_gage_info, ds_gage_info) %>%
  select(site_no, lat, lon) %>% filter(!duplicated(.))

mod_dir <- paste0('figures/model_output/shap/',model_name,'/')
dir.create(mod_dir, showWarnings = F)

performance_summary <- read_csv(paste0('data/models/performance/',model_name,'_summary.csv')) %>%
  select(var, all_of(performance_metric)) %>% rename(performance = 2)
shaps <- read_csv(paste0('data/models/shaps/',model_name,'_shaps.csv'))

global_dat <- shaps %>%
  filter(type == 'shap') %>%
  select(!c(site_no, type)) %>%
  pivot_longer(!metric, names_to = 'var', values_to = 'shap')

local_dat <- shaps %>%
  pivot_longer(!c(metric, site_no, type), names_to = 'var', values_to = 'val') %>%
  pivot_wider(id_cols = c(metric, site_no, var), names_from = type, values_from = val)

metrics_sel = performance_summary$var
metric_labels <- c('Mean Annual Q', 'Q5', 'Q95', 'Runoff Ratio',
                   '# High-Flow Peaks', '# No-Flow Periods',
                   'High-Flow Duration', 'No-Flow Duration',
                   'Half-Flow Date', 'Half-Flow Interval', 'Peak Q Date',
                   'Baseflow Index', 'Flashiness Index', 'FDC Mid-Section Slope',
                   'Recession Constant', 'Recession Seasonality')

vars_sel = unique(local_dat$var)


# plot --------------------------------------------------------------------
fig_dir <- paste0(mod_dir,'shap_maps/')
dir.create(fig_dir, showWarnings = F)

for(i in 1:length(metrics_sel)){
  metric_sel =  metrics_sel[i]
  metric_dir = paste0(fig_dir,metric_sel,'/')
  dir.create(metric_dir, showWarnings = F)
  metric_label = metric_labels[i]
  
  global_importance <- filter(global_dat, metric == metric_sel) %>%
    mutate(shap = abs(shap)) %>%
    group_by(var) %>%
    summarise(shap = round(mean(shap), 6)) %>%
    arrange(desc(shap)) %>%
    mutate(var = fct_reorder(var, shap))
  
  #all vars
  plot_dat_all <- filter(local_dat, metric == metric_sel) %>%
    mutate(var = factor(var, levels = rev(levels(global_importance$var)))) %>%
    arrange(abs(shap)) %>%
    left_join(gage_coords) %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
    st_transform(5070)
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = plot_dat_all, aes(color = shap)) +
    scale_color_gradient2(low = 'red', mid = 'grey70', high = 'blue',
                          name = 'SHAP') +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = 'right',
          text = element_text(size = 12)) +
    facet_wrap(vars(var)) +
    ggtitle(paste0(metric_label,', ',
                   performance_metric,' = ',performance_summary$performance[performance_summary$var == metric_sel]))
  ggsave(paste0(metric_dir,'0_all.png'), height = 9, width = 14, units = 'in')
  
  #individual vars
  for(j in 1:length(vars_sel)){
    var_sel <- vars_sel[j]
    var_label <- pred_labeller(var_sel)
    
    plot_dat_ind <- filter(local_dat, metric == metric_sel & var == var_sel) %>%
      arrange(abs(shap)) %>%
      left_join(gage_coords) %>%
      st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
      st_transform(5070)
    
    ggplot() +
      geom_sf(data = states) +
      geom_sf(data = plot_dat_ind, aes(color = shap)) +
      scale_color_gradient2(low = 'red', mid = 'grey70', high = 'blue',
                            limits = c(min(plot_dat_all$shap), max(plot_dat_all$shap)),
                            name = 'SHAP') +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            legend.position = 'right',
            text = element_text(size = 12)) +
      ggtitle(paste0(metric_label,', ',
                     var_label,' SHAP, ',
                     set_label))
    ggsave(paste0(metric_dir,var_sel,'.png'), height = 3.6, width = 6)
  }
}
