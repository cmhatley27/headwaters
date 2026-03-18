# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('scripts/functions/load_states.R')

metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')

model_names <- paste0(tolower(metrics_sel),'_annual_w3_emp')
model_dirs <- paste0('data/models/',model_names,'/')

metric_trends <- read_csv(paste0(model_dirs,'trends.csv')) %>%
  filter(var %in% metrics_sel, type == 'obs') %>%
  select(metric, site_no, sen, p)
shap_trends <- read_csv(paste0(model_dirs,'trends.csv')) %>%
  filter(type == 'shap') %>%
  select(metric, site_no, var, sen) 

cn_trend_diffs <- left_join(connections, rename(shap_trends, headwater_id = site_no, up_sen = sen)) %>%
  left_join(rename(shap_trends, downstream_id = site_no, down_sen = sen)) %>%
  left_join(rename(metric_trends, headwater_id = site_no, up_sen_metric = sen, up_sen_metric_p = p)) %>%
  left_join(rename(metric_trends, downstream_id = site_no, down_sen_metric = sen, down_sen_metric_p = p)) %>%
  mutate(sen_diff = up_sen - down_sen,
         sen_metric_diff = up_sen_metric - down_sen_metric,
         abs_sen_metric_diff = abs(up_sen_metric) - abs(down_sen_metric)) %>%
  filter(!is.na(up_sen_metric), !is.na(down_sen_metric)) %>%
  group_by(metric) %>%
  filter(abs(up_sen_metric) <= quantile(abs(up_sen_metric[var == 'ag']), 0.99, na.rm = T),
         abs(down_sen_metric) <= quantile(abs(down_sen_metric[var == 'ag']), 0.99, na.rm = T)) %>%
  filter(!is.na(sen_diff)) %>%
  ungroup(.)

imp_dat <- cn_trend_diffs %>%
  group_by(metric, var) %>%
  summarise(mean_sen_diff = mean(abs(sen_diff)),
            std_sen_diff = sqrt(var(abs(sen_diff))),
            max_sen_diff = max(abs(sen_diff))) %>%
  mutate(upper_sen_diff = mean_sen_diff + std_sen_diff,
         lower_sen_diff = mean_sen_diff - std_sen_diff) %>%
  mutate(var_name = factor(var, labels = labelinator(var, pred_labels)),
         var_cat = labelinator(var, pred_cats))
  
var1_dat <- cn_trend_diffs %>%
  group_by(metric, connection_id, headwater_id) %>%
  arrange(desc(abs(sen_diff)), .by_group = T) %>%
  summarise(var1 = var[1],
            var1_pct = abs(sen_diff[var == var[1]])/sum(abs(sen_diff)),
            var2 = var[2],
            var2_pct = abs(sen_diff[var == var[2]])/sum(abs(sen_diff)),
            shap_sum = sum(sen_diff),
            across(contains('sen_metric'), ~mean(.x))) %>%
  mutate(var1_name = labelinator(var1, pred_labels),
         var1_cat = factor(labelinator(var1, pred_cats), levels = names(pred_cat_colors)),
         var2_name = labelinator(var2, pred_labels),
         var2_cat = factor(labelinator(var2, pred_cats), levels = names(pred_cat_colors))) %>%
  mutate(up_sig = case_when(
    up_sen_metric_p > 0.1 ~ 'none',
    up_sen_metric_p <= 0.1 & up_sen_metric > 0 ~ 'pos',
    up_sen_metric_p <= 0.1 & up_sen_metric < 0 ~ 'neg',
    .default = 'none'),
    down_sig = case_when(
      down_sen_metric_p > 0.1 ~ 'none',
      down_sen_metric_p <= 0.1 & down_sen_metric > 0 ~ 'pos',
      down_sen_metric_p <= 0.1 & down_sen_metric < 0 ~ 'neg',
      .default = 'none')) %>%
  mutate(sig_diff = up_sig != down_sig,
         diff_group = ifelse(abs(up_sen_metric) > abs(down_sen_metric), 'up', 'down')) %>%
  group_by(metric) %>%
  mutate(
         diff_group2 = case_when(
           abs_sen_metric_diff >= quantile(abs_sen_metric_diff, 0.75) ~ 'up',
           abs_sen_metric_diff <= quantile(abs_sen_metric_diff, 0.25) ~ 'down',
           .default = 'none'
         ),
         diff_group3 = case_when(
           up_sig == 'neg' & down_sig == 'pos' ~ 1,
           up_sig == 'none' & down_sig == 'pos' ~ 2,
           up_sig == 'pos' & down_sig == 'pos' ~ 3,
           up_sig == 'neg' & down_sig == 'none' ~ 4,
           up_sig == 'none' & down_sig == 'none' ~ 5,
           up_sig == 'pos' & down_sig == 'none' ~ 6,
           up_sig == 'neg' & down_sig == 'neg' ~ 7,
           up_sig == 'none' & down_sig == 'neg' ~ 8,
           up_sig == 'pos' & down_sig == 'neg' ~ 9,
           .default = 5
         )) %>%
  ungroup(.) %>%
  left_join(select(all_gage_info, headwater_id = site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(var1_dat) <- 4326
var1_dat <- st_transform(var1_dat,5070)

# maps --------------------------------------------------------------------

for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m)
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = var1_dat_m, aes(color = var1_cat),
            size = 1, fill = NA) +
    scale_color_discrete(limits = names(pred_cat_colors), name = 'Category', guide = NULL) +
    # scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    ggtitle(paste(labelinator(m, metric_labels),'Category of Top \u0394Trend Predictor'))
  ggsave(paste0('figures/shap_trend_deltas/var1_maps/',m,'.png'), 
         height = 2.5, width = 4, units = 'in')
  
}

# bars --------------------------------------------------------------------
for(m in metrics_sel){
  var1_dat_m <- filter(var1_dat, metric == m)
  
  ggplot(filter(var1_dat_m, diff_group2 != 'none'), aes(x = var1_cat, fill = var1_cat, alpha = sig_diff)) +
    geom_bar(color = 'black') +
    scale_x_discrete(limits = names(pred_cat_colors)) +
    scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
    scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.5, 1), na.value = 0.3, name = 'Metric Trend\nSignificance') +
    xlab('') +
    ylab('# Gages') +
    ggtitle(paste(labelinator(m, metric_labels), 'Category of Top \u0394Trend Predictor')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(vars(diff_group2))
  ggsave(paste0('figures/shap_trend_deltas/var1_bars_split/',m,'.png'), 
         height = 3, width = 6, units = 'in')
}


# mini bars ---------------------------------------------------------------
for(m in metrics_sel){
  plot_dat_m <- filter(var1_dat, metric == m)
  
  ggplot(filter(plot_dat_m), aes(x = var1_cat, fill = var1_cat, alpha = sig_diff)) +
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
  ggsave(paste0('figures/shap_trend_deltas/var1_bars_mini/',m,'.png'), 
         height = 0.67, width = 1.15, units = 'in')
}



# global imp --------------------------------------------------------------
for(m in metrics_sel){
  imp_dat_m <- filter(imp_dat, metric == m) %>%
    mutate(var_name = fct_reorder(var_name, mean_sen_diff))
  
  #SHAP TREND BARS with colored CATEGORIES
  ggplot(filter(imp_dat_m), aes(y = var_name, x = mean_sen_diff, fill = var_cat)) +
    geom_col(color = 'black') +
    # geom_errorbar(aes(xmin = mean_sen, xmax = max_sen)) +
    ylab('') +
    xlab('Mean |SHAP trend|') +
    scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors)) +
    ggtitle(paste(labelinator(m), 'Global \u0394SHAP trends'))
  ggsave(paste0('figures/shap_trend_deltas/global_imp/',m,'.png'), 
         height = 6, width = 6.5, units = 'in')
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
  
  wilcox_p <- round(wilcox.test((var1_dat_m$abs_sen_metric_diff))$p.value, 3)
  wilcox.test(var1_dat_m$abs_sen_metric_diff)
  
  percents <- round(table(var1_dat_m$abs_sen_metric_diff >= 0)/nrow(var1_dat_m)*100)
  
  p <- ggplot(var1_dat_m, aes(x = abs_sen_metric_diff, fill = var1_cat)) +
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
  var1_dat_m <- filter(var1_dat, metric == m)
  
  ggplot(filter(var1_dat_m), aes(x = up_sen_metric, y = down_sen_metric, color = var1_cat)) +
    geom_point() +
    geom_abline(slope = 1) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(limits = quantile(var1_dat_m$up_sen_metric, c(0.01,0.99))) +
    scale_y_continuous(limits = quantile(var1_dat_m$down_sen_metric, c(0.01,0.99))) +
    scale_color_discrete(limits = names(pred_cat_colors), guide = NULL) +
    # scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.5, 1), na.value = 0.3, name = 'Metric Trend\nSignificance') +
    xlab('Upstream Trend') +
    ylab('Downstream Trend') +
    ggtitle(paste(labelinator(m, metric_labels), 'Category of Top \u0394Trend Predictor')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+
  facet_wrap(vars(diff_group3))
  ggsave(paste0('figures/shap_trend_deltas/scatters/',m,'.png'), 
         height = 4, width = 4, units = 'in')
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