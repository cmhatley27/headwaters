# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'Q95'
metric_name <- labelinator(metric_sel)

model_name <- paste0(tolower(metric_sel),'_annual_w3_emp')
model_dir <- paste0('data/models/',model_name,'/')

#central plains - region2 8.2
#north/northeast - region3 c(58,59,60,62)
#front range - region3 c(21,26)
#pnw - region3 1:4, %nin% c('12035400', '12143700', '12143900', '14314500')
site_label <- 'hw'
sites_sel <- all_gage_info$site_no[all_gage_info$order <= 3]
# sites_sel <- sites_sel[sites_sel %nin% c('12035400', '12143700', '12143900', '14314500')]

shaps <- read_csv(paste0(model_dir,'shaps.csv')) %>%
  filter(site_no %in% sites_sel)

predictions <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  filter(site_no %in% sites_sel)

trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(site_no %in% sites_sel)

#gage counts
ggplot(filter(all_gage_info, site_no %in% sites_sel), aes(x = factor(order))) +
  geom_bar()


# global shap VALUES ------------------------------------------------------

global_imp <- shaps %>%
  group_by(var) %>%
  summarise(shap = mean(abs(shap))) %>%
  mutate(var_name = factor(var, labels = labelinator(var, pred_labels)),
         var_cat = labelinator(var, pred_cats))


#SHAP with colored CATEGORIES
ggplot(global_imp, aes(y = fct_reorder(var_name, shap), x = shap, fill = var_cat)) +
  geom_col(color = 'black') +
  ylab('') +
  xlab('Mean |SHAP|') +
  scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors)) +
  ggtitle(paste(metric_name, 'Global SHAPs'))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_global_SHAPS_cats.png'), height = 6, width = 6.5, units = 'in')




# global shap TRENDS ------------------------------------------------------

global_trend_imp <- trends %>%
  filter(type == 'shap') %>%
  filter(abs(sen) <= quantile(abs(sen), 0.99)) %>%
  group_by(var) %>%
  summarise(sen = mean(abs(sen))) %>%
  mutate(var_name = factor(var, labels = labelinator(var, pred_labels)),
         var_cat = labelinator(var, pred_cats)) %>%
  mutate(var_name = fct_reorder(var_name, sen))

#SHAP TREND BARS with colored CATEGORIES
ggplot(global_trend_imp, aes(y = fct_reorder(var_name, sen), x = sen, fill = var_cat)) +
  geom_col(color = 'black') +
  ylab('') +
  xlab('Mean |SHAP trend|') +
  scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors)) +
  ggtitle(paste(metric_name, 'Global SHAP trends'))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_global_SHAP_trends_cats.png'), height = 6, width = 6.5, units = 'in')


#SHAP TREND BEESWARM
library(ggbeeswarm)

plot_dat <- trends %>%
  filter(var != metric_sel) %>%
  pivot_wider(id_cols = c(site_no, var), names_from = type, values_from = sen) %>%
  mutate(var_name = factor(labelinator(var, pred_labels), levels = levels(global_trend_imp$var_name))) %>%
  filter(abs(shap) <= quantile(abs(shap), 0.99)) %>%
  group_by(var) %>%
  mutate(obs_scale = obs/max(abs(obs), na.rm = T)) %>%
  arrange((abs(obs_scale))) %>%
  left_join(select(all_gage_info, site_no, region))

ggplot(plot_dat, aes(y = var_name, x = shap, color = obs_scale)) +
  geom_vline(xintercept = 0) +
  geom_hline(aes(yintercept = var_name), alpha = 0.1, lty = 'longdash') +
  geom_quasirandom(width = 0.25) +
  scale_color_gradient2(high = "#4575b4",
                        mid = "#ffffcf",
                        low = "#d73027",
                        breaks = c(-2/3, 0, 2/3),
                        labels = c('Negative', '0', 'Positive'),
                        name = 'Var. Trend') +
  ylab(NULL) +
  xlab('SHAP Trend') +
  ggtitle(paste(metric_name, 'Local SHAP trends'))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_local_SHAP_trends_beeswarm.png'), height = 6, width = 6, units = 'in')

pred_sel <- 'swe_persistence'
ggplot(filter(plot_dat, var == pred_sel), aes(x = obs, y = shap)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = 'lm') +
  xlab(paste(labelinator(pred_sel, pred_labels),'trend')) +
  ylab(paste(labelinator(pred_sel, pred_labels),'SHAP trend')) +
  scale_color_discrete(name = 'Region',
                       labels = c('App Mtns', 'Ctl Plains', 'E. Forest', 'Grt Plains', 'Mtn West', 'N. Forest', 'Desert SW'))
ggsave(paste0('figures/model_output/',model_name,'/',pred_sel,'_local_SHAP_trends_scatter.png'), height = 3, width = 5, units = 'in')

ggplot(filter(shaps, var == 'developed'), aes(x = val, y = shap)) +
  geom_point() +
  geom_smooth(method = 'lm')

# global shap TREND DIFFS -------------------------------------------------
shap_trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(type == 'shap') %>%
  select(site_no, var, sen)

cn_trend_diffs <- left_join(connections, rename(shap_trends, headwater_id = site_no, up_sen = sen)) %>%
  left_join(rename(shap_trends, downstream_id = site_no, down_sen = sen)) %>%
  mutate(sen_diff = up_sen - down_sen) %>%
  filter(!is.na(sen_diff)) %>%
  filter(abs(sen_diff) <= quantile(abs(sen_diff), 0.999))

global_trend_diff_imp <- cn_trend_diffs %>%
  group_by(var) %>%
  summarise(sen_diff = mean(abs(sen_diff))) %>%
  mutate(var_name = factor(var, labels = labelinator(var, pred_labels)),
         var_cat = labelinator(var, pred_cats)) %>%
  mutate(var_name = fct_reorder(var_name, sen_diff))


#shap TREND DIFFS with CATEGORIES
ggplot(global_trend_diff_imp, aes(y = var_name, x = sen_diff, fill = var_cat)) +
  geom_col(color = 'black') +
  ylab('') +
  xlab('Mean |\u0394SHAP trend|') +
  scale_fill_discrete(name = 'Category', limits = names(pred_cat_colors)) +
  ggtitle(paste(metric_sel, 'Global \u0394SHAP trends'))
ggsave(paste0('figures/model_output/',model_name,'/connection_global_SHAP_trend_diffs_cats.png'), height = 6, width = 7, units = 'in')




# LOCAL shap trends -------------------------------------------------------
library(ggExtra)
cn_key <- pivot_longer(connections, c(headwater_id, downstream_id), names_to = 'type', values_to = 'site_no') %>%
  select(site_no, type) %>%
  mutate(type = factor(ifelse(type == 'headwater_id', 'upstream', 'downstream')))

pred_trends <- filter(trends, var != metric_sel) %>%
  pivot_wider(id_cols = c(site_no, var), names_from = type, values_from = sen) %>%
  filter(!is.na(obs)) %>%
  left_join(cn_key) %>%
  filter(!is.na(type))

p <- ggplot(filter(pred_trends, var == 'precip_annual'), aes(x = obs, y = shap, color = type)) +
  geom_point() +
  scale_color_discrete(limits = c('upstream', 'downstream'), labels = c('Upstream', 'Downstream'))
ggMarginal(p, groupColour = T, groupFill = T)





