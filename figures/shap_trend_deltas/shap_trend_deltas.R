# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/site_sets.R')
source('scripts/functions/load_gages.R')
source('scripts/functions/load_states.R')

metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
site_set <- 'hw'
site_set_name <- 'Mid-Atlantic'

model_names <- paste0(tolower(metrics_sel),'_annual_w3_emp')
model_dirs <- paste0('data/models/',model_names,'/')

metric_trends <- read_csv(paste0(model_dirs,'trends.csv')) %>%
  filter(var %in% metrics_sel, type == 'obs') %>%
  select(metric, site_no, sen, p)
shap_trends <- read_csv(paste0(model_dirs,'trends.csv')) %>%
  filter(type == 'shap') %>%
  select(metric, site_no, var, sen)
pred_trends <- read_csv(paste0(model_dirs,'trends.csv')) %>%
  filter(type == 'obs' & var %nin% metrics_sel) %>%
  select(metric, site_no, var, sen)
self_lms <- read_csv(paste0(model_dirs,'lms.csv')) %>%
  filter(type == 'self') %>%
  select(metric, site_no, var, lm_b, lm_r2)
connection_trends <- read_csv(paste0(model_dirs,'connection_difference_trends.csv')) %>%
  filter(type == 'obs') %>%
  select(metric, connection_id, metric_diff_p = p)


cn_trend_diffs <- left_join(connections, rename(shap_trends, headwater_id = site_no, up_shap = sen)) %>%
  left_join(rename(shap_trends, downstream_id = site_no, down_shap = sen)) %>%
  left_join(rename(pred_trends, headwater_id = site_no, up_pred = sen)) %>%
  left_join(rename(pred_trends, downstream_id = site_no, down_pred = sen)) %>%
  left_join(rename(metric_trends, headwater_id = site_no, up_metric = sen, up_metric_p = p)) %>%
  left_join(rename(metric_trends, downstream_id = site_no, down_metric = sen, down_metric_p = p)) %>%
  left_join(rename(self_lms, headwater_id = site_no, up_lm = lm_b, up_lm_r2 = lm_r2)) %>%
  left_join(rename(self_lms, downstream_id = site_no, down_lm = lm_b, down_lm_r2 = lm_r2)) %>%
  mutate(var_name = labelinator(var, pred_labels),
         var_cat = factor(labelinator(var, pred_cats), levels = names(pred_cat_colors)),
         shap_diff = up_shap - down_shap,
         abs_shap_diff = abs(up_shap) - abs(down_shap),
         pred_diff = up_pred - down_pred,
         pred_rat = up_pred/down_pred,
         abs_pred_diff = abs(up_pred) - abs(down_pred),
         pred_diff_pct = abs(up_pred - down_pred)/abs(up_pred),
         metric_diff = up_metric - down_metric,
         abs_metric_diff = abs(up_metric) - abs(down_metric),
         metric_diff_pct = abs(up_metric-down_metric)/abs(up_metric),
         lm_rat = up_lm/down_lm,
         lm_diff = abs(up_lm) - abs(down_lm),
         lm_diff_pct = abs(up_lm - down_lm)/abs(up_lm)) %>%
  left_join(connection_trends) %>%
  filter(!is.na(up_metric), !is.na(down_metric)) %>%
  group_by(metric) %>%
  filter(abs(up_metric) <= quantile(abs(up_metric[var == 'ag']), 0.99, na.rm = T),
         abs(down_metric) <= quantile(abs(down_metric[var == 'ag']), 0.99, na.rm = T)) %>%
  mutate(
    diff_sig = metric_diff_p <= 0.1,
    sig_cat = case_when(
      up_metric >= 0 & down_metric >= 0 & up_metric >= down_metric ~ 'up_pos',
      up_metric >= 0 & down_metric >= 0 & up_metric < down_metric ~ 'down_pos',
      up_metric <= 0 & down_metric <= 0 & up_metric <= down_metric ~ 'up_neg',
      up_metric <= 0 & down_metric <= 0 & up_metric > down_metric ~ 'down_neg',
      up_metric >= 0 & down_metric <= 0 ~ 'up_pos_opp',
      up_metric <= 0 & down_metric >= 0 ~ 'up_neg_opp'
    ),
    sig_cat = factor(sig_cat, levels = c('up_neg_opp', 'down_pos', 'up_pos', 'up_neg', 'down_neg', 'up_pos_opp')),
    sig_cat_simple = case_when(
      up_metric >= 0 & down_metric >= 0 & abs(up_metric) > abs(down_metric) ~ 'up',
      up_metric <= 0 & down_metric <= 0 & abs(up_metric) > abs(down_metric) ~ 'up',
      up_metric >= 0 & down_metric >= 0 & abs(up_metric) < abs(down_metric) ~ 'down',
      up_metric <= 0 & down_metric <= 0 & abs(up_metric) < abs(down_metric) ~ 'down',
      sign(up_metric) != sign(down_metric) ~ 'opposite',
      .default = 'none'
    ),
    sig_cat_simple = factor(sig_cat_simple, levels = c('none', 'opposite', 'down', 'up'))
  ) %>%
  filter(!is.na(shap_diff),
         headwater_id %in% site_sets[[site_set]]) %>%
  ungroup(.)

imp_dat <- cn_trend_diffs %>%
  group_by(metric, var) %>%
  summarise(mean_shap_diff = mean(abs(shap_diff)),
            median_shap_diff = median(abs(shap_diff)),
            std_shap_diff = sqrt(var(abs(shap_diff))),
            max_shap_diff = max(abs(shap_diff))) %>%
  mutate(upper_shap_diff = mean_shap_diff + std_shap_diff,
         lower_shap_diff = mean_shap_diff - std_shap_diff) %>%
  mutate(var_name = factor(var, labels = labelinator(var, pred_labels)),
         var_cat = labelinator(var, pred_cats))
  
var1_dat <- cn_trend_diffs %>%
  # mutate(var_cat = labelinator(var, pred_cats)) %>%
  # group_by(metric, connection_id, headwater_id, var_cat) %>%
  # summarise(cat_sum = sum(abs(shap_diff)),
  #           across(contains('metric'), ~mean(.x))) %>%
  # group_by(metric, connection_id, headwater_id) %>%
  # arrange(desc(abs(cat_sum)), .by_group = T) %>%
  # summarise(var1 = var_cat[1],
  #           var1_cat = var1,
  #           across(contains('metric'), ~mean(.x))) %>%
  group_by(metric, connection_id, headwater_id) %>%
  arrange(desc(abs(shap_diff)), .by_group = T) %>%
  summarise(var1 = var[1],
            var1_pct = abs(shap_diff[var == var[1]])/sum(abs(shap_diff)),
            var2 = var[2],
            var2_pct = abs(shap_diff[var == var[2]])/sum(abs(shap_diff)),
            shap_sum = sum(shap_diff),
            across(contains('metric'), ~mean(.x)),
            across(contains('sig'), ~.x[1])
            ) %>%
  mutate(var1_name = labelinator(var1, pred_labels),
         var1_cat = factor(labelinator(var1, pred_cats), levels = names(pred_cat_colors)),
         var2_name = labelinator(var2, pred_labels),
         var2_cat = factor(labelinator(var2, pred_cats), levels = names(pred_cat_colors))) %>%
  ungroup(.) %>%
  left_join(select(all_gage_info, headwater_id = site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(var1_dat) <- 4326
var1_dat <- st_transform(var1_dat,5070)

# maps --------------------------------------------------------------------

for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m) %>%
    arrange(diff_sig)
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = var1_dat_m, aes(color = var1_cat, shape = diff_sig),
            size = 1, fill = NA) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    scale_color_discrete(limits = names(pred_cat_colors), name = 'Category', guide = NULL) +
    scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    ggtitle(paste(labelinator(m, metric_labels),'Up/Down Difference Drivers')) +
    theme(panel.border = element_blank())
  ggsave(paste0('figures/shap_trend_deltas/var1_maps/',m,'.png'), 
         width = 76, height = 52.5, units = 'mm',
         dpi = 600)
  
}

# bars --------------------------------------------------------------------
for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m)
  
  ggplot(filter(var1_dat_m), aes(x = var1_cat, fill = var1_cat, alpha = diff_sig)) +
    geom_bar(color = 'black') +
    scale_x_discrete(limits = names(pred_cat_colors)) +
    scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
    scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.5, 1), na.value = 0.3, name = 'Metric Trend\nSignificance') +
    xlab('') +
    ylab('# Gages') +
    ggtitle(paste(labelinator(m, metric_labels), 'Category of Top \u0394Trend Predictor')) +
    # facet_wrap(vars(sig_cat), drop = F) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  ggsave(paste0('figures/shap_trend_deltas/var1_bars/',m,'.png'), 
         height = 3, width = 6, units = 'in')
}


# mini bars ---------------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(var1_dat, metric == m)
  
  ggplot(filter(plot_dat_m), aes(x = var1_cat, fill = var1_cat, alpha = diff_sig)) +
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
  ggsave(paste0('figures/shap_trend_deltas/var1_bars_mini/',m,'.png'), 
         height = 25, width = 32, units = 'mm')
}


# global imp --------------------------------------------------------------
for(m in metrics_sel){
  imp_dat_m <- filter(imp_dat, metric == m) %>%
    mutate(var_name = fct_reorder(var_name, mean_shap_diff))
  
  #SHAP TREND BARS with colored CATEGORIES
  ggplot(filter(imp_dat_m), aes(y = var_name, x = mean_shap_diff, fill = var_cat)) +
    geom_col(color = 'black', linewidth = 0.15) +
    # geom_errorbar(aes(xmin = mean_sen, xmax = max_sen)) +
    ylab('') +
    xlab('Mean |\u0394SHAP trend|') +
    scale_x_continuous(n.breaks = 4) +
    scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors), guide = NULL) +
    ggtitle(paste(labelinator(m), 'Global \u0394 Imp.')) +
    theme(axis.text = element_text(size = rel(0.8)))
  ggsave(paste0('figures/shap_trend_deltas/global_imp/',m,'.png'), 
         height = 75, width = 65, units = 'mm')
}


# global imp mini --------------------------------------------------------------
for(m in metrics_sel){
  imp_dat_m <- filter(imp_dat, metric == m) %>%
    mutate(var_name = fct_reorder(var_name, mean_sen_diff))
  
  #SHAP TREND BARS with colored CATEGORIES
  ggplot(filter(imp_dat_m, mean_sen_diff >= quantile(mean_sen_diff, 0.33, na.rm = T)), aes(y = var_name, x = mean_sen_diff, fill = var_cat)) +
    geom_col(color = 'black') +
    # geom_errorbar(aes(xmin = mean_sen, xmax = max_sen)) +
    ylab('') +
    xlab('Mean |SHAP trend|') +
    scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors), guide = NULL) +
    # ggtitle(paste(metric_name, 'Global SHAP trends')) +
    theme(text = element_text(size = 7))
  ggsave(paste0('figures/shap_trend_deltas/global_imp_mini/',m,'.png'), 
         height = 2.5, width = 3, units = 'in')
}


# hists -------------------------------------------------------------------

for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m)
  # 
  # table(var1_dat_m$abs_sen_metric_diff >= 0)
  # quantile(var1_dat_m$abs_sen_metric_diff)
  # median(var1_dat_m$abs_sen_metric_diff)
  # IQR(var1_dat_m$abs_sen_metric_diff)
  # low_thresh <- quantile(var1_dat_m$abs_sen_metric_diff, 0.25)
  # high_thresh <- quantile(var1_dat_m$abs_sen_metric_diff, 0.75)
  
  wilcox_p <- round(wilcox.test((var1_dat_m$abs_metric_diff))$p.value, 3)
  wilcox.test(var1_dat_m$abs_sen_metric_diff)
  
  percents <- round(table(var1_dat_m$abs_metric_diff >= 0)/nrow(var1_dat_m)*100)
  
  p <- ggplot(var1_dat_m, aes(x = abs_metric_diff, fill = var1_cat)) +
    geom_histogram(color = 'black',
                   bins = 20,
                   boundary = 0) +
    geom_vline(xintercept = 0, linewidth = 0.75) +
    # annotate('text', label = paste('Wil. p:',wilcox_p),
    #          x = -Inf, y = Inf, hjust = -.1, vjust = 2) +
    annotate('text', label = paste0(percents[1],'%'),
             x= -Inf, y = Inf, hjust = -1, vjust = 8) +
    annotate('text', label = paste0(percents[2],'%'),
             x= Inf, y = Inf, hjust = 2, vjust = 8) +
    scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
    xlab('|Sen\'s Slope| Difference') +
    ylab('') +
    ggtitle(paste(labelinator(m, metric_labels),'Connection \u0394Trend Differences'))
  p
  ggsave(paste0('figures/shap_trend_deltas/hists/',m,'.png'), 
         plot = p,
         width = 4, height = 3, units = 'in')
}

# scatter -----------------------------------------------------------------
for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m) %>%
    arrange(diff_sig)
  
  p <- ggplot(filter(var1_dat_m), aes(x = up_metric, y = down_metric, color = var1_cat, shape = diff_sig, text = paste(connection_id))) +
    geom_point() +
    geom_abline(slope = 1) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    scale_color_discrete(limits = names(pred_cat_colors), name = 'Category') +
    scale_shape_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(21,19), 
                       name = 'Metric Trend\nSignificance', guide = NULL) +
    xlab('Upstream Sen\'s Slope') +
    ylab('Downstream Sen\'s Slope') +
    theme(legend.position = 'left',
          legend.box.spacing = unit(11.85,'mm'),
          legend.spacing.y = unit(0,'mm'),
          legend.key.spacing.y = unit(-2,'mm'))
  p
  ggsave(paste0('figures/shap_trend_deltas/scatters/',m,'.png'), 
         width = 100, height = 52.5, units = 'mm',
         dpi = 600)
}
library(plotly)
ggplotly(p, tooltip = 'text')


# scatter bars ---------------------------------------------------------------
for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m) %>%
    st_drop_geometry(.)
  counts <- table(select(var1_dat_m, sig_cat_simple, var1_cat))
  
  for(cat in unique(var1_dat_m$sig_cat_simple)){
    var1_dat_m_cat <- filter(var1_dat_m, sig_cat_simple == cat)
    
    ggplot(filter(var1_dat_m_cat), aes(x = var1_cat, fill = var1_cat, alpha = diff_sig)) +
      geom_bar(color = 'black') +
      scale_x_discrete(limits = names(pred_cat_colors), labels = NULL) +
      scale_y_continuous(limits = c(0,max(counts))) +
      scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
      scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.5, 1), na.value = 0.3, name = 'Metric Trend\nSignificance',
                         guide = NULL) +
      xlab(NULL) +
      ylab(NULL) +
      theme(text = element_text(size = 8),
            plot.background = element_rect(fill = fill_alpha('white',0), color = alpha('white',0)),
            panel.background = element_rect(fill = fill_alpha('white',0)))
    
    ggsave(paste0('figures/shap_trend_deltas/var1_bars_cats/',m,'/',cat,'.png'), 
           height = 0.67, width = 1.15, units = 'in')
  }
}



# category lms ------------------------------------------------------------
for(m in metrics_sel){
  dat_m <- filter(cn_trend_diffs, metric == m) %>%
    left_join(select(var1_dat, metric, connection_id, var1, var1_name, var1_cat)) %>%
    mutate(sig_cat_simple = factor(sig_cat_simple, levels = c('none', 'down', 'opposite', 'up'))) %>%
    filter(sig_cat_simple != 'none') %>%
    mutate(sig_cat_simple2 = ifelse(diff_sig, as.character(sig_cat_simple), 'none'),
           sig_cat_simple2 = factor(sig_cat_simple2, levels = c('none','down','opposite', 'up'))) %>%
    st_drop_geometry(.) %>%
    group_by(var) %>%
    filter(abs(lm_rat) <= quantile(abs(lm_rat), 0.99, na.rm = T),
           abs(pred_rat) <= quantile(abs(pred_rat), 0.99, na.rm = T),
           abs(lm_diff) <= quantile(abs(lm_diff), 0.99, na.rm = T),
           abs(pred_diff) <= quantile(abs(pred_diff), 0.99, na.rm = T))
  
  var_sel <- 'precip_annual'
  p <- ggplot(filter(dat_m, var == var_sel), aes(x = pred_diff, y = lm_diff, color = sig_cat_simple2)) +
    geom_point(size = 1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_color_manual(values = c('grey60', '#4575b4','purple','#d73027'), guide = NULL) +
    xlab(paste(labelinator(var_sel, pred_labels), '|Trend| Difference')) +
    ylab(paste(labelinator(var_sel, pred_labels), '|Sensitivity| Difference'))
  pp <- ggMarginal(p, groupFill = T, type = 'boxplot', alpha = 1)
  pp
  ggsave(paste0('figures/shap_trend_deltas/sensitivity/',m,'_',var_sel,'.png'),
         pp,
         width = 4.5, height = 4, units = 'in')
  
  asdf <- filter(dat_m, var == var_sel)
  pairwise.wilcox.test(asdf$pred_diff, asdf$sig_cat_simple2, p.adjust.method = 'BH')
  pairwise.wilcox.test(asdf$lm_diff, asdf$sig_cat_simple2, p.adjust.method = 'BH')
  
  diffs <- dat_m %>%
    filter(diff_sig) %>%
    group_by(var) %>%
    summarise(lm_diff_p = wilcox.test(lm_diff[sig_cat_simple == 'up'], lm_diff[sig_cat_simple == 'down'])$p.value,
              pred_diff_p = wilcox.test(pred_diff[sig_cat_simple == 'up'], pred_diff[sig_cat_simple == 'down'])$p.value)
  
  var_sel <- 'precip_annual'
  ggplot(filter(dat_m, var == var_sel, diff_sig), aes(x = sig_cat_simple, y = lm_diff, fill = sig_cat_simple)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    annotate(geom = 'text', x = -Inf, y = Inf, hjust = -0.5, vjust = 3,
             label = paste('M-W p:', round(diffs$lm_diff_p[diffs$var == var_sel], 3))) +
    scale_x_discrete(limits = c('down', 'up'),
                     labels = c('Down > Up', 'Up > Down'),
                     name = NULL) +
    scale_fill_manual(limits = c('down', 'opposite', 'up'),
                      values = c('#4575b4','purple','#d73027'),
                      guide = NULL) +
    ylab(paste(labelinator(var_sel, pred_labels), 'Sensitivity Diff.')) +
    geom_hline(yintercept = 0)
  ggplot(filter(dat_m, var == var_sel, diff_sig), aes(x = sig_cat_simple, y = pred_diff, fill = sig_cat_simple)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    annotate(geom = 'text', x = -Inf, y = Inf, hjust = -0.5, vjust = 3,
             label = paste('M-W p:', round(diffs$pred_diff_p[diffs$var == var_sel], 3))) +
    scale_x_discrete(limits = c('down', 'up'),
                     labels = c('Down > Up', 'Up > Down'),
                     name = NULL) +
    scale_fill_manual(limits = c('down', 'opposite', 'up'),
                      values = c('#4575b4','purple','#d73027'),
                      guide = NULL) +
    ylab(paste(labelinator(var_sel, pred_labels), 'Trend Diff.')) +
    geom_hline(yintercept = 0)
  
  p <- ggplot(filter(dat_m, var == var_sel), aes(x = pred_diff, y = lm_diff, color = sig_cat_simple2)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_color_manual(values = c('grey60', '#4575b4','purple','#d73027'), guide = NULL) +
    xlab('|Trend| Difference') +
    ylab('|Sensitivity| Difference')
  ggMarginal(p, groupFill = T, type = 'boxplot', alpha = 1)
  
  
  
  dat_m_cats <- dat_m %>%
    mutate(var_cat2 = case_when(
      var %in% c('precip_annual', 'precip_jfm', 'precip_amj', 'precip_jas', 'precip_ond') ~ 'Precip',
      var %in% c('pet_annual', 'pet_jfm', 'pet_amj', 'pet_jas', 'pet_ond') ~ 'PET',
      var %in% c('max_swe', 'swe_annual') ~ 'Snow',
      var %in% c('ag', 'developed', 'forest', 'grass') ~ 'Land Cover'
    )) %>%
    filter(!is.na(var_cat2)) %>%
    mutate(var_cat2 = factor(var_cat2, levels = c('Precip', 'PET', 'Snow', 'Land Cover'))) %>%
    group_by(metric, connection_id, diff_sig, sig_cat_simple, sig_cat_simple2, var_cat2) %>%
    summarise(across(c(up_lm, down_lm, up_pred, down_pred),
                     ~sum(abs(.x)))) %>%
    mutate(lm_diff = up_lm - down_lm,
           pred_diff = up_pred - down_pred) %>%
    left_join(select(var1_dat, metric, connection_id, var1_name, var1_cat)) %>%
    ungroup(.)
  
  cat_sel = 'Land Cover'
  p <- ggplot(filter(dat_m_cats, var_cat2 == cat_sel, sig_cat_simple2 %in% c('down','opposite','up')), aes(x = pred_diff, y = lm_diff, color = sig_cat_simple2)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_color_manual(values = c('#4575b4','purple','#d73027'), guide = NULL) +
    xlab('|Trend| Difference') +
    ylab('|Sensitivity| Difference')
  ggMarginal(p, groupFill = T, type = 'boxplot', alpha = 1)
  
  ggplot(filter(dat_m_cats, var_cat2 == cat_sel, diff_sig), aes(x = sig_cat_simple, y = lm_diff, fill = sig_cat_simple)) +
    geom_boxplot() +
    scale_fill_manual(values = c('#4575b4','purple','#d73027')) +
    geom_hline(yintercept = 0)
  ggplot(filter(dat_m_cats, var_cat2 == cat_sel), aes(x = sig_cat_simple2, y = pred_diff, fill = sig_cat_simple2)) +
    geom_boxplot() +
    scale_fill_manual(values = c('grey90', '#4575b4','purple','#d73027')) +
    geom_hline(yintercept = 0)
  
  ggplot(dat_m_cats, aes(x = up_pred, y = down_pred, color = sig_cat_simple2)) +
    geom_point() +
    geom_abline(slope = 1) +
    scale_color_manual(values = c('grey60', '#4575b4','purple','#d73027')) +
    facet_wrap(vars(var_cat2), scales = 'free')
  
  
ggplot(filter(dat_m_cats, diff_sig, sig_cat_simple %in% c('down', 'up')), aes(x = sig_cat_simple, fill = sig_cat_simple, y= lm_diff)) +
  geom_boxplot() +
  scale_fill_manual(limits = c('down', 'up'),
                    values = c('#4575b4','#d73027')) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(var_cat2), scales = 'free_y')
ggplot(filter(dat_m_cats, diff_sig, sig_cat_simple %in% c('down', 'up')), aes(x = sig_cat_simple, fill = sig_cat_simple, y= pred_diff)) +
  geom_boxplot() +
  scale_fill_manual(limits = c('down', 'up'),
                    values = c('#4575b4','#d73027')) +
  # scale_fill_manual(values = c('#4575b4','purple','#d73027')) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(var_cat2), scales = 'free_y')

  cat_diffs <- dat_m_cats %>%
    filter(diff_sig) %>%
    group_by(var_cat2) %>%
    summarise(lm_diff_p = wilcox.test(lm_diff[sig_cat_simple == 'up'], lm_diff[sig_cat_simple == 'down'])$p.value,
              pred_diff_p = wilcox.test(pred_diff[sig_cat_simple == 'up'], pred_diff[sig_cat_simple == 'down'])$p.value)
  
  
  dat_m_fil <- filter(dat_m, var %in% c('precip_annual','developed'))
  var_sel <- 'developed'
  ggplot(filter(dat_m, var == var_sel), aes(x = (up_pred), y = (down_pred))) +
    geom_point() +
    geom_abline(slope = 1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    # scale_color_manual(values = c('grey60', '#4575b4','purple','#d73027')) +
    xlab('Upstream Trend') +
    ylab('Downstream Trend') +
    ggtitle(labelinator(var_sel, pred_labels))
  ggsave(paste0('figures/shap_trend_deltas/sensitivity/',m,'_',var_sel,'_upvdown.png'),
         width = 3, height = 3, units = 'in')
    
    # facet_wrap(vars(var_name), scales = 'free')
  
  pred_diffs <- dat_m_fil %>%
    group_by(var) %>%
    summarise(r2 = r2(up_pred, down_pred),
              wil = wilcox.test(abs(up_pred), abs(down_pred), paired = T)$p.value,
              med = median(abs(up_pred)-abs(down_pred)))
  
  

}



# mini beeswarms ----------------------------------------------------------
library(ggbeeswarm)
for(m in metrics_sel){
  cn_trend_diffs_m <- filter(cn_trend_diffs, metric == m)
  # counts <- table(select(var1_dat_m, sig_cat, var1_cat))
  
  for(cat in unique(cn_trend_diffs_m$sig_cat)){
    cn_trend_diffs_m_cat <- filter(cn_trend_diffs_m) %>%
      group_by(var) %>%
      mutate(abs_pred_diff_scale = scale(abs_pred_diff, center = F)[,1]) %>%
      ungroup(.)
    
    var_order <- cn_trend_diffs_m_cat %>%
      group_by(metric, var_name) %>%
      summarise(median_shap_diff = median(abs(shap_diff))) %>%
      arrange(desc(median_shap_diff))

    swarm_dat <- cn_trend_diffs_m_cat %>%
      mutate(var_name = factor(var_name, levels = rev(var_order$var_name))) %>%
      filter(var_name %in% var_order$var_name[1:16]) %>%
      group_by(var) %>%
      arrange(abs(abs_pred_diff_scale), by.group = T)
    
    pred_scale_lims <- quantile(swarm_dat$abs_pred_diff_scale, c(0.05,0.95), na.rm = T)
    
    ggplot(filter(swarm_dat), aes(x = shap_diff, y = var_name, color = abs_pred_diff_scale)) +
      geom_vline(xintercept = 0) +
      geom_hline(aes(yintercept = var_name), alpha = 0.15) +
      geom_quasirandom(width = 0.2, size = 1) +
      scale_x_continuous(limits = quantile(swarm_dat$abs_shap_diff, c(0.01,0.99)),
                         n.breaks = 4) +
      scale_color_gradient2(limits = pred_scale_lims,
                            oob = scales::squish,
                            # high = 'blue',
                            high = "#d73027",
                            mid = "#ffffcf",
                            low = "#4575b4",
                            # low = 'red',
                            name = '\u0394|Var Trend|',
                            breaks = c(pred_scale_lims[1]+0.25,0,pred_scale_lims[2]-0.25), labels = c('Neg', '0', 'Pos')) +
      ylab(NULL) +
      xlab('\u0394|SHAP Trend|') +
      ggtitle(paste0(labelinator(m), ' Diff Var Imp.')) +
      theme(axis.text = element_text(size = rel(0.8)),
            legend.position = 'right',
            legend.box.spacing = unit(2,'mm'),
            legend.key.height = unit(7, 'mm'),
            legend.key.width = unit(3, 'mm'))
    ggsave(paste0('figures/shap_trend_deltas/beeswarms/cats/',m,'_',cat,'.png'), 
           height = 55, width = 70, units = 'mm')
  }
}



# tictactoe -----------------------------------------------------------------
for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m)
  
  ggplot(filter(var1_dat_m), aes(x = up_sig, y = down_sig, color = var1_cat)) +
    geom_jitter(width = 0.25,
                height = 0.25) +
    geom_hline(yintercept = c(1.5,2.5)) +
    geom_vline(xintercept = c(1.5,2.5)) +
    # scale_x_continuous(limits = quantile(var1_dat_m$up_sen_metric, c(0.01,0.99))) +
    # scale_y_continuous(limits = quantile(var1_dat_m$down_sen_metric, c(0.01,0.99))) +
    scale_color_discrete(limits = names(pred_cat_colors), guide = NULL) +
    # scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.5, 1), na.value = 0.3, name = 'Metric Trend\nSignificance') +
    xlab('Upstream Trend Sig.') +
    ylab('Downstream Trend Sig.') +
    ggtitle(paste(labelinator(m, metric_labels), 'Category of Top \u0394Trend Predictor')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(paste0('figures/shap_trend_deltas/tictactoes/',m,'.png'), 
         height = 3, width = 6, units = 'in')
}
