# data --------------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
source('scripts/Theme+Settings.R')
options(scipen = 999) #disable scientific notation

hw_gages <- read_csv('data/gages/hw_gage_info.csv')$site_no
ds_gages <- read_csv('data/gages/ds_gage_info.csv')$site_no

model_name <- 'all_senval_ds'
performance_metric = 'r2'

beeswarm_nvars <- 20

site_sel <- ds_gages
hw_comp <- 'hw_senval' #set to NA if no comparison needed

mod_dir <- paste0('figures/model_output/shap/',model_name,'/')
dir.create(mod_dir, showWarnings = F)

performance_summary <- read_csv(paste0('data/models/performance/all_senval_summary.csv')) %>%
  select(var, all_of(performance_metric)) %>% rename(performance = 2)
shaps <- read_csv(paste0('data/models/shaps/all_senval_shaps.csv'))

global_dat <- shaps %>%
  filter(type == 'shap' & site_no %in% site_sel) %>%
  select(!c(site_no, type)) %>%
  pivot_longer(!metric, names_to = 'var', values_to = 'shap')

local_dat <- shaps %>%
  filter(site_no %in% site_sel) %>%
  pivot_longer(!c(metric, site_no, type), names_to = 'var', values_to = 'val') %>%
  pivot_wider(id_cols = c(metric, site_no, var), names_from = type, values_from = val)

metrics_sel = performance_summary$var
vars_sel = unique(local_dat$var)

# global importance -------------------------------------------------------
fig_dir <- paste0(mod_dir,'global_importance/')
dir.create(fig_dir, showWarnings = F)
dir.create(paste0(fig_dir,'trim/'), showWarnings = F)

for(i in 1:length(metrics_sel)){
  metric_sel =  metrics_sel[i]
  metric_label = metric_labels[i]
  
  plot_dat <- filter(global_dat, metric == metric_sel) %>%
    mutate(shap = abs(shap)) %>%
    group_by(var) %>%
    summarise(shap = mean(shap)) %>%
    arrange(desc(shap)) %>%
    mutate(var = fct_reorder(var, shap))
  nvar = length(plot_dat$var)
  
  ggplot(plot_dat, aes(x = shap, y = var)) +
    geom_col() +
    xlab('Mean Abs SHAP') +
    ylab(NULL) +
    scale_y_discrete(labels = pred_labeller) +
    ggtitle(paste0(metric_label,', ',
                   performance_metric,' = ',performance_summary$performance[performance_summary$var == metric_sel])) +
    theme(text = element_text(size = 14))
  ggsave(paste0(fig_dir,metric_sel,'.png'), height = 8, width = 6)
  
  #trimmed
  ggplot(plot_dat[1:8,], aes(x = shap, y = var)) +
    geom_col() +
    xlab('Mean Abs SHAP') +
    ylab(NULL) +
    scale_y_discrete(labels = pred_labeller) +
    ggtitle(paste0(metric_label,', ',
                   performance_metric,' = ',performance_summary$performance[performance_summary$var == metric_sel])) +
    theme(text = element_text(size = 14))
  ggsave(paste0(fig_dir,'trim/',metric_sel,'.png'), height = 4, width = 5)
}


# local importance --------------------------------------------------------
fig_dir <- paste0(mod_dir,'local_importance/')
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
  plot_dat <- filter(local_dat, metric == metric_sel) %>%
    mutate(var = factor(var, levels = rev(levels(global_importance$var))))
  ggplot(plot_dat, aes(x = obs, y = shap)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_point() +
    xlab('Feature Value') +
    ylab('SHAP') +
    facet_wrap(vars(var), scales = 'free_x') +
    ggtitle(paste0(metric_label,', ',
                   performance_metric,' = ',performance_summary$performance[performance_summary$var == metric_sel]))
  ggsave(paste0(metric_dir,'0_all.png'), height = 9, width = 14, units = 'in')
  
  #individual vars
  for(j in 1:length(vars_sel)){
    var_sel <- vars_sel[j]
    var_label <- pred_labeller(var_sel)
    
    plot_dat <- filter(local_dat, metric == metric_sel & var == var_sel)
    
    ggplot(plot_dat, aes(x = obs, y = shap)) +
      geom_hline(yintercept = 0, linetype = 'dashed') +
      geom_point() +
      xlab('Feature Value') +
      ylab('SHAP') +
      ggtitle(paste0('Metric = ',metric_label,', ',
                     'Feature = ',var_label,', ',
                     'Global SHAP = ',global_importance$shap[global_importance$var == var_sel]))
    ggsave(paste0(metric_dir,var_sel,'.png'), height = 5, width = 6)
  }
}

# beeswarm ----------------------------------------------------------------
fig_dir <- paste0(mod_dir,'beeswarm/')
dir.create(fig_dir, showWarnings = F)
dir.create(paste0(fig_dir,'trim/'), showWarnings = F)

for(i in 1:length(metrics_sel)){
  metric_sel =  metrics_sel[i]
  metric_label = metric_labels[i]
  
  global_importance <- filter(global_dat, metric == metric_sel) %>%
    mutate(shap = abs(shap)) %>%
    group_by(var) %>%
    summarise(shap = round(mean(shap), 6)) %>%
    arrange(desc(shap)) %>%
    mutate(var = fct_reorder(var, shap))
  
  plot_dat <- filter(local_dat, metric == metric_sel) %>%
    mutate(var = factor(var, levels = levels(global_importance$var)), ordered = T) %>%
    group_by(var) %>%
    mutate(obs_scale = (obs-min(obs))/(max(obs)-min(obs)),
           obs_scale2 = ifelse(obs >= 0, obs/max(obs), -1*obs/min(obs)))
  
  ggplot(subset(plot_dat, var %in% levels(var)[-1:(beeswarm_nvars - length(levels(var)))]), aes(x = shap, y = var, color = obs_scale2)) +
    geom_vline(xintercept = 0) +
    geom_quasirandom(size = 1) +
    ylab(NULL) +
    xlab('SHAP') +
    scale_y_discrete(labels = pred_labeller) +
    scale_color_gradient2(mid = 'grey70', low = 'red', high = 'blue',
                          breaks = c(-1, 0, 1), labels = c('Neg', '0', 'Pos'),
                          name = 'Feature Value') +
    ggtitle(paste0(metric_label,', ',
                   performance_metric,' = ',performance_summary$performance[performance_summary$var == metric_sel])) +
    theme(text = element_text(size = 14))
  ggsave(paste0(fig_dir,metric_sel,'.png'), height = 8, width = 6, units = 'in')
  
  #trimmed
  ggplot(subset(plot_dat, var %in% levels(var)[-1:(8 - length(levels(var)))]), aes(x = shap, y = var, color = obs_scale2)) +
    geom_vline(xintercept = 0) +
    geom_quasirandom(size = 1) +
    ylab(NULL) +
    xlab('SHAP') +
    scale_y_discrete(labels = pred_labeller) +
    scale_color_gradient2(mid = 'grey70', low = 'red', high = 'blue',
                          breaks = c(-1, 0, 1), labels = c('Neg', '0', 'Pos'),
                          name = 'Feature Value') +
    ggtitle(paste0(metric_label,', ',
                   performance_metric,' = ',performance_summary$performance[performance_summary$var == metric_sel])) +
    theme(text = element_text(size = 14))
  ggsave(paste0(fig_dir,'trim/',metric_sel,'.png'), height = 5, width = 6, units = 'in')
}


# reordered global and beeswarm -------------------------------------------
if(!is.na(hw_comp)){
  fig_dir <- paste0(mod_dir,'hw_comp/')
  dir.create(fig_dir, showWarnings = F)
  dir.create(paste0(fig_dir,'global_reordered/'), showWarnings = F)
  dir.create(paste0(fig_dir,'beeswarm_reordered/'), showWarnings = F)
  dir.create(paste0(fig_dir,'rank_scatter/'), showWarnings = F)
  dir.create(paste0(fig_dir,'importance_scatter/'), showWarnings = F)
  
  hw_global_dat <- read_csv(paste0('data/models/shaps/',hw_comp,'_shaps.csv')) %>%
    filter(type == 'shap') %>%
    select(!c(site_no, type)) %>%
    pivot_longer(!metric, names_to = 'var', values_to = 'shap')
  
  for(i in 1:length(metrics_sel)){
    metric_sel =  metrics_sel[i]
    
    hw_global_importance <- filter(hw_global_dat, metric == metric_sel) %>%
      mutate(shap = abs(shap)) %>%
      group_by(var) %>%
      summarise(shap = mean(shap)) %>%
      arrange(desc(shap)) %>%
      mutate(var = fct_reorder(var, shap))
    
    #global importance reordered
    global_plot_dat <- filter(global_dat, metric == metric_sel) %>%
      mutate(shap = abs(shap)) %>%
      group_by(var) %>%
      summarise(shap = mean(shap)) %>%
      arrange(desc(shap)) %>%
      mutate(var = factor(var, levels = levels(hw_global_importance$var)))
    
    ggplot(global_plot_dat, aes(x = shap, y = var)) +
      geom_col() +
      xlab('Mean Abs SHAP') +
      ylab(NULL) +
      ggtitle(paste0(metric_sel,', ',
                     performance_metric,' = ',performance_summary$performance[performance_summary$var == metric_sel]))
    ggsave(paste0(fig_dir,'global_reordered/',metric_sel,'.png'), height = 5, width = 6)
    
    
    #importance comparison scatter
    rank_comp <- left_join(hw_global_importance, global_plot_dat, by = 'var', suffix = c('_hw', '_ds')) %>%
      mutate(across(c('shap_hw','shap_ds'), ~rank(.x), .names = '{.col}_rank'),
             across(c('shap_hw','shap_ds'), ~((.x-min(.x))/(max(.x)-min(.x))), .names = '{.col}_rescale'))
    pcor = round(cor(rank_comp$shap_hw, rank_comp$shap_ds, method = 'pearson'),3)
    scor = round(cor(rank_comp$shap_hw_rank, rank_comp$shap_ds_rank, method = 'spearman'), 3)
    
    #ranks
    ggplot(rank_comp, aes(x = shap_hw_rank, y = shap_ds_rank)) +
      geom_point() +
      xlab('Headwater Importance Rank') +
      ylab('Downstream Importance Rank') +
      ggtitle(paste0(metric_sel,', Spearman Cor = ',scor))
    ggsave(paste0(fig_dir,'rank_scatter/',metric_sel,'.png'), height = 4, width = 4)
    
    #values
    ggplot(rank_comp, aes(x = shap_hw, y = shap_ds)) +
      geom_point() +
      xlab('Headwater Importance') +
      ylab('Downstream Importance') +
      ggtitle(paste0(metric_sel,', Pearson Cor = ',pcor))
    ggsave(paste0(fig_dir,'importance_scatter/',metric_sel,'.png'), height = 4, width = 4)
    
    #beeswarm reordered
    beeswarm_plot_dat <- filter(local_dat, metric == metric_sel) %>%
      mutate(var = factor(var, levels = levels(hw_global_importance$var), ordered = T)) %>%
      group_by(var) %>%
      mutate(obs_scale = (obs-min(obs))/(max(obs)-min(obs)),
             obs_scale2 = ifelse(obs >= 0, obs/max(obs), -1*obs/min(obs)))
    
    ggplot(subset(beeswarm_plot_dat, var %in% levels(var)[-1:(beeswarm_nvars - length(levels(var)))]), aes(x = shap, y = var, color = obs_scale2)) +
      geom_vline(xintercept = 0) +
      geom_quasirandom(size = 1) +
      ylab(NULL) +
      xlab('SHAP') +
      scale_color_gradient2(mid = 'grey70', low = 'red', high = 'blue',
                            breaks = c(-1, 0, 1), labels = c('Neg', '0', 'Pos'),
                            name = 'Feature Value') +
      ggtitle(paste0(metric_sel,', ',
                     performance_metric,' = ',performance_summary$performance[performance_summary$var == metric_sel]))
    ggsave(paste0(fig_dir,'beeswarm_reordered/',metric_sel,'.png'), height = 5, width = 6, units = 'in')
  }
}
