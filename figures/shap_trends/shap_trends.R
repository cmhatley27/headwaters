# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/site_sets.R')
source('scripts/functions/load_gages.R')
source('scripts/functions/load_states.R')

metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
site_set <- 'hw'
site_set_name <- 'S. Rockies'

model_names <- paste0(tolower(metrics_sel),'_annual_w3_emp')
model_dirs <- paste0('data/models/',model_names,'/')

trends <- read_csv(paste0(model_dirs,'trends.csv')) %>%
  filter(site_no %in% all_gage_info$site_no[all_gage_info$order <= 3]) 

sites_retain <- filter(trends, type == 'obs', var == metric) %>%
  group_by(metric) %>%
  filter(!is.na(sen)) %>%
  filter(abs(sen) <= quantile(abs(sen), 0.99)) %>%
  select(metric, site_no) %>%
  filter(site_no %in% site_sets[[site_set]])

imp_dat <- left_join(sites_retain, trends) %>%
  filter(type == 'shap') %>%
  group_by(metric, var) %>%
  summarise(mean_sen = mean(abs(sen)),
            median_sen = median(abs(sen)),
            std_sen = sqrt(var(abs(sen))),
            max_sen = max(abs(sen))) %>%
  mutate(upper_sen = mean_sen + std_sen,
         lower_sen = mean_sen - std_sen) %>%
  mutate(var_name = factor(var, labels = labelinator(var, pred_labels)),
         var_cat = labelinator(var, pred_cats))


var1_dat <- left_join(sites_retain, trends) %>%
  # filter(type == 'shap') %>%
  # mutate(var_cat = labelinator(var, pred_cats)) %>%
  # group_by(metric, site_no, var_cat) %>%
  # summarise(cat_sum = sum(abs(sen), na.rm = T)) %>%
  # group_by(metric, site_no) %>%
  # arrange(desc(abs(cat_sum)), .by_group = T) %>%
  # summarise(var1 = var_cat[1],
  #           var1_cat = var1) %>%
  filter(type == 'shap') %>%
  group_by(metric, site_no) %>%
  arrange(desc(abs(sen)), .by_group = T) %>%
  summarise(var1 = var[1],
            var1_sen = sen[var == var[1]],
            var1_pct = abs(sen[var == var[1]])/sum(abs(sen)),
            var2 = var[2],
            var2_pct = abs(sen[var == var[2]])/sum(abs(sen)),
            sen_sum = sum(sen)) %>%
  mutate(var1_name = labelinator(var1, pred_labels),
         var1_cat = factor(labelinator(var1, pred_cats), levels = names(pred_cat_colors)),
         var2_name = labelinator(var2, pred_labels),
         var2_cat = factor(labelinator(var2, pred_cats), levels = names(pred_cat_colors))) %>%
  left_join(select(filter(trends, var %in% metrics_sel, type == 'obs'), metric, site_no, sen, p)) %>%
  left_join(select(filter(trends, var %in% metrics_sel, type == 'prediction'), metric, site_no, pred_sen = sen)) %>%
  left_join(select(all_gage_info, site_no, lat, lon, region2, region3)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(var1_dat) <- 4326
plot_dat <- st_transform(var1_dat, 5070)

swarm_dat <- left_join(sites_retain, trends) %>%
  filter(var != metric) %>%
  pivot_wider(id_cols = c(metric, site_no, var), names_from = type, values_from = sen) %>%
  mutate(var_name = labelinator(var, pred_labels),
         var_cat = labelinator(var, pred_cats)) %>%
  group_by(var) %>%
  mutate(obs_scale = scale(obs, center = F)[,1]) %>%
  ungroup(.)

# maps --------------------------------------------------------------------
for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m) %>%
    arrange(desc(p))
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = var1_dat_m, aes(color = var1_cat, shape = p <= 0.1),
            size = 1, fill = NA) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    scale_color_discrete(limits = names(pred_cat_colors), name = 'Category', guide = NULL) +
    scale_shape_manual(limits = c(F,T), values = c(21,19), guide = NULL) +
    ggtitle(paste(labelinator(m, metric_labels),'Trend Drivers')) +
    theme(panel.border = element_blank(),
          legend.box.spacing = unit(-2,'mm'),
          legend.key.size = unit(4,'mm'))
  ggsave(paste0('figures/shap_trends/var1_maps/',m,'_',site_label,'.png'), 
         width = 76, height = 52.5, units = 'mm',
         dpi = 600)
  
}

# bars --------------------------------------------------------------------
for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m)
  
  ggplot(filter(var1_dat_m, !is.na(p)), aes(x = var1_cat, fill = var1_cat, alpha = p <= 0.1)) +
    geom_bar(color = 'black') +
    scale_x_discrete(limits = names(pred_cat_colors)) +
    scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
    scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.5, 1), na.value = 0.3, name = 'Metric Trend\nSignificance') +
    xlab('') +
    ylab('# Gages') +
    ggtitle(paste(labelinator(m, metric_labels), 'Category of Top Trend Predictor')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste0('figures/shap_trends/var1_bars/',m,'_',site_label,'.png'), 
         height = 3, width = 6, units = 'in')
}


# mini bars ---------------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(var1_dat, metric == m)
  
  ggplot(filter(plot_dat_m, !is.na(p)), aes(x = var1_cat, fill = var1_cat, alpha = p <= 0.1)) +
    geom_bar(color = 'black') +
    scale_x_discrete(limits = names(pred_cat_colors)) +
    scale_y_continuous(n.breaks = 4) +
    scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
    scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.5, 1), na.value = 0.3, 
                       name = 'Metric Trend\nSignificance', guide = NULL) +
    xlab(NULL) +
    ylab(NULL) +
    theme(panel.border = element_blank(),
          axis.line = element_line(color = 'black', linewidth = 0.25),
          axis.text = element_text(size = rel(0.8)),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste0('figures/shap_trends/var1_bars_mini/',m,'_',site_label,'.png'), 
         height = 25, width = 32, units = 'mm')
}


# global imp --------------------------------------------------------------
for(m in metrics_sel){
  imp_dat_m <- filter(imp_dat, metric == m) %>%
    mutate(var_name = fct_reorder(var_name, mean_sen))
  
  #SHAP TREND BARS with colored CATEGORIES
  ggplot(filter(imp_dat_m), aes(y = var_name, x = mean_sen, fill = var_cat)) +
    geom_col(color = 'black', linewidth = 0.15) +
    # geom_errorbar(aes(xmin = mean_sen, xmax = max_sen)) +
    ylab('') +
    xlab('Mean |SHAP trend|') +
    scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors), guide = NULL) +
    theme(axis.text = element_text(size = rel(0.8)),
          legend.position = 'left',
          legend.key.size = unit(2,'mm'),
          legend.box.spacing = unit(0,'mm'),
          legend.key.spacing.y = unit(1,'mm')) +
    ggtitle(paste0(labelinator(m),' Global Imp.'))
  ggsave(paste0('figures/shap_trends/global_imp/',m,'_',site_label,'.png'),  
         height = 75, width = 65, units = 'mm',
         dpi = 600)
}


# global imp mini --------------------------------------------------------------
for(m in metrics_sel){
  imp_dat_m <- filter(imp_dat, metric == m) %>%
    mutate(var_name = fct_reorder(var_name, mean_sen)) %>%
    mutate(keep = mean_sen >= quantile(mean_sen, 0.33)) %>%
    arrange(mean_sen)
  
  #SHAP TREND BARS with colored CATEGORIES
  ggplot(imp_dat_m, aes(y = var_name, x = mean_sen, fill = var_cat)) +
    geom_col(color = 'black', linewidth = 0.15) +
    scale_y_discrete(limits = imp_dat_m$var_name[imp_dat_m$keep]) +
    # geom_errorbar(aes(xmin = mean_sen, xmax = max_sen)) +
    ylab('') +
    xlab('Mean |SHAP trend|') +
    scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors), guide = NULL) +
    theme(axis.text = element_text(size = rel(0.8)),
          legend.position = 'left',
          legend.key.size = unit(2,'mm'),
          legend.box.spacing = unit(4,'mm'),
          legend.key.spacing.y = unit(1,'mm'))

  ggsave(paste0('figures/shap_trends/global_imp_mini/',m,'_',site_label,'.png'), 
         height = 52.5, width = 100, units = 'mm')
}


# beeswarm ----------------------------------------------------------------
library(ggbeeswarm)
for(m in metrics_sel){
  var_order <- filter(imp_dat, metric == m) %>%
    arrange(desc(median_sen))
  swarm_dat_m <- filter(swarm_dat, metric == m) %>%
    mutate(var_name = factor(var_name, levels = rev(var_order$var_name))) %>%
    filter(var_name %in% var_order$var_name[1:33]) %>%
    group_by(var) %>%
    arrange(abs(obs_scale), by.group = T)
  obs_scale_lims <- quantile(swarm_dat_m$obs_scale, c(0.05,0.95), na.rm = T)
  
  ggplot(swarm_dat_m, aes(y = var_name, x = shap, color = obs_scale)) +
    geom_vline(xintercept = 0) +
    geom_hline(aes(yintercept = var_name), alpha = 0.15) +
    geom_quasirandom(width = 0.2, size = 1) +
    scale_x_continuous(limits = quantile(swarm_dat_m$shap, c(0.01,0.99)),
                       n.breaks = 4) +
    scale_color_gradient2(limits = obs_scale_lims,
                          oob = scales::squish,
                          # high = 'blue',
                          high = "#4575b4",
                          mid = "#ffffcf",
                          low = "#d73027",
                          # low = 'red',
                          name = 'Var Trend',
                          breaks = c(obs_scale_lims[1]+0.25,0,obs_scale_lims[2]-0.25), labels = c('Neg', '0', 'Pos')) +
    ylab(NULL) +
    xlab('SHAP Trend') +
    ggtitle(paste0(labelinator(m), ' Var Imp. at ',site_set_name)) +
    theme(axis.text = element_text(size = rel(0.8)),
          legend.position = 'right',
          legend.box.spacing = unit(2,'mm'),
          legend.key.height = unit(7, 'mm'),
          legend.key.width = unit(3, 'mm'))
  
  ggsave(paste0('figures/shap_trends/regional/',site_set,'/beeswarm/',m,'.png'), 
         height = 55, width = 70, units = 'mm')
}


