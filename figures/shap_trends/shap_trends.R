# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('scripts/functions/load_states.R')

metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')

model_names <- paste0(tolower(metrics_sel),'_annual_w3_emp')
model_dirs <- paste0('data/models/',model_names,'/')

site_label <- 'hw'
sites_sel <- all_gage_info$site_no[all_gage_info$order <= 3]

trends <- read_csv(paste0(model_dirs,'trends.csv')) %>%
  filter(site_no %in% sites_sel) 

sites_retain <- filter(trends, type == 'obs', var == metric) %>%
  group_by(metric) %>%
  filter(!is.na(sen)) %>%
  filter(abs(sen) <= quantile(abs(sen), 0.99)) %>%
  select(metric, site_no)

imp_dat <- left_join(sites_retain, trends) %>%
  filter(type == 'shap') %>%
  group_by(metric, var) %>%
  summarise(mean_sen = mean(abs(sen)),
            std_sen = sqrt(var(abs(sen))),
            max_sen = max(abs(sen))) %>%
  mutate(upper_sen = mean_sen + std_sen,
         lower_sen = mean_sen - std_sen) %>%
  mutate(var_name = factor(var, labels = labelinator(var, pred_labels)),
         var_cat = labelinator(var, pred_cats))


var1_dat <- left_join(sites_retain, trends) %>%
  filter(type == 'shap') %>%
  group_by(metric, site_no) %>%
  arrange(desc(abs(sen)), .by_group = T) %>%
  summarise(var1 = var[1],
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

# maps --------------------------------------------------------------------
for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m)
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = var1_dat_m, aes(color = var1_cat),
            size = 1, fill = NA) +
    scale_color_discrete(limits = names(pred_cat_colors), name = 'Category', guide = NULL) +
    # scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    ggtitle(paste(labelinator(m, metric_labels),'Category of Top Trend Predictor'))
  ggsave(paste0('figures/shap_trends/var1_maps/',m,'_',site_label,'.png'), 
         height = 2.5, width = 4, units = 'in')
  
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
    scale_x_discrete(limits = names(pred_cat_colors), labels = NULL) +
    scale_y_continuous(labels = NULL, breaks = NULL) +
    scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
    scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.5, 1), na.value = 0.3, 
                       name = 'Metric Trend\nSignificance', guide = NULL) +
    xlab(NULL) +
    ylab(NULL) +
    theme_classic(base_line_size = 0.25, base_size = 8) +
    theme(plot.margin = margin(l=1),
          plot.background = element_rect(fill = fill_alpha('white',0), color = alpha('white',0)),
          panel.background = element_rect(fill = fill_alpha('white',0)))
  ggsave(paste0('figures/shap_trends/var1_bars_mini/',m,'_',site_label,'.png'), 
         height = 0.67, width = 1.15, units = 'in')
}


# global imp --------------------------------------------------------------
for(m in metrics_sel){
  imp_dat_m <- filter(imp_dat, metric == m) %>%
    mutate(var_name = fct_reorder(var_name, mean_sen))
  
  #SHAP TREND BARS with colored CATEGORIES
  ggplot(filter(imp_dat_m), aes(y = var_name, x = mean_sen, fill = var_cat)) +
    geom_col(color = 'black') +
    # geom_errorbar(aes(xmin = mean_sen, xmax = max_sen)) +
    ylab('') +
    xlab('Mean |SHAP trend|') +
    scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors)) +
    ggtitle(paste(labelinator(m), 'Global SHAP trends'))
  ggsave(paste0('figures/shap_trends/global_imp/',m,'_',site_label,'.png'),  
         height = 6, width = 6.5, units = 'in')
}


# global imp mini --------------------------------------------------------------
for(m in metrics_sel){
  imp_dat_m <- filter(imp_dat, metric == m) %>%
    mutate(var_name = fct_reorder(var_name, mean_sen))
  
  #SHAP TREND BARS with colored CATEGORIES
  ggplot(filter(imp_dat_m, mean_sen >= quantile(mean_sen, 0.33)), aes(y = var_name, x = mean_sen, fill = var_cat)) +
    geom_col(color = 'black') +
    # geom_errorbar(aes(xmin = mean_sen, xmax = max_sen)) +
    ylab('') +
    xlab('Mean |SHAP trend|') +
    scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors), guide = NULL) +
    # ggtitle(paste(metric_name, 'Global SHAP trends')) +
    theme(text = element_text(size = 7))
  ggsave(paste0('figures/shap_trends/global_imp_mini/',m,'_',site_label,'.png'), 
         height = 2.5, width = 3, units = 'in')
}
