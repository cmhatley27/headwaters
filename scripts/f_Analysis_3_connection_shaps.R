# data --------------------------------------------------------------------
library(tidyverse)
library(ggExtra)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'FlashinessIndex'

model_name <- 'flashinessindex_annual'

model_dir <- paste0('data/models/',model_name,'/')

region_sel <- 'all'

sites_sel <- all_gage_info$site_no
if(region_sel != 'all') sites_sel <- all_gage_info$site_no[all_gage_info$region %in% region_sel]
#remove sites with abnormally high SHAP MSE
sites_sel <- sites_sel[sites_sel %nin% '12143700']

predictor_statics <- read_csv('data/gages/predictors/pred_statics.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = val)
predictor_temporals <- read_csv('data/gages/predictors/pred_timeseries.csv')
predictor_variables <- left_join(predictor_temporals, predictor_statics) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val')

shaps <- read_csv(list.files(paste0(model_dir,'shaps/'), full.names = T)) %>%
  select(!c(explain_id, none)) %>%
  left_join(select(all_gage_info, site_no, order)) %>%
  pivot_longer(!c(metric, site_no, order, wateryear), names_to = 'var', values_to = 'shap') %>%
  mutate(
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  ) %>%
  left_join(predictor_variables) %>%
  filter(site_no %in% sites_sel)

shaps_trends <- shaps %>%
  group_by(site_no, order, order_lump2, var) %>%
  summarise(across(shap, ~trendinator(.x), .unpack = '{inner}'),
            int = median(shap)-(sen*median(wateryear)))
predictors_trends <- shaps %>%
  group_by(site_no, order, order_lump2, var) %>%
  summarise(across(val, ~trendinator(.x), .unpack = '{inner}'),
            int = median(val)-(sen*median(wateryear)))

metric_values <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  rename(val = obs) %>%
  left_join(select(all_gage_info, site_no, order)) %>%
  filter(site_no %in% sites_sel) %>%
  mutate(
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  )

metric_trends <- metric_values %>%
  group_by(site_no) %>%
  summarise(across(c(val, pred), ~trendinator(.x), .unpack = '{outer}_{inner}'),
            val_int = median(val)-(val_sen*median(wateryear)),
            pred_int = median(pred)-(pred_sen*median(wateryear))) %>%
  select(site_no, val_sen, pred_sen, val_int, pred_int, val_p)
connection_metric_diffs <- connections %>%
  left_join(metric_trends, by = join_by(headwater_id == site_no)) %>%
  left_join(metric_trends, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds')) %>%
  mutate(val_diff = val_sen_hw - val_sen_ds,
         obs_full_diff = obs_full_sen_hw - obs_full_sen_ds,
         pred_diff = pred_sen_hw - pred_sen_ds,
         sig_cat_hw = trend_classifier(val_sen_hw, val_p_hw, 0.1),
         sig_cat_ds = trend_classifier(val_sen_ds, val_p_ds, 0.1),
         sig_diff = sig_cat_hw != sig_cat_ds) %>%
  filter(headwater_id %in% shaps$site_no, downstream_id %in% shaps$site_no)


cor(connection_metric_diffs$obs_full_diff, connection_metric_diffs$pred_diff)^2
ggplot(connection_metric_diffs, aes(x = obs_full_diff, y = pred_diff)) +
  geom_abline(slope = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point()

# loop through connections ------------------------------------------------
connections_sel <- connection_metric_diffs$connection_id

connection_diffs <- tibble()
for(c in seq_along(connections_sel)){
  connection_sel <- connections_sel[c]
  
  gages_sel <- c(connections$headwater_id[connection_sel], connections$downstream_id[connection_sel])
  names(gages_sel) <- c('Upstream', 'Downstream')
  
  connection_shaps_diffs_c <- shaps_trends %>%
    filter(site_no %in% gages_sel) %>%
    mutate(site_no = factor(site_no, levels = gages_sel, labels = names(gages_sel))) %>%
    pivot_wider(id_cols = var, names_from = site_no, values_from = sen) %>%
    rename(shap_upstream = Upstream, shap_downstream = Downstream) %>%
    mutate(shap_diff = shap_upstream - shap_downstream,
           shap_diff_sum = sum(shap_diff),
           shap_diff_pct = abs(shap_diff)/sum(abs(shap_diff)),
           shap_diff_scale = scale(shap_diff, center = F)[,1],
           connection_id = connection_sel,
           var = factor(var, levels = unique(shaps$var), labels = pred_labeller(unique(shaps$var))))
  connection_predictors_diffs_c <- predictors_trends %>%
    filter(site_no %in% gages_sel) %>%
    mutate(site_no = factor(site_no, levels = gages_sel, labels = names(gages_sel))) %>%
    pivot_wider(id_cols = var, names_from = site_no, values_from = sen) %>%
    rename(pred_upstream = Upstream, pred_downstream = Downstream) %>%
    mutate(pred_diff = pred_upstream - pred_downstream,
           connection_id = connection_sel,
           var = factor(var, levels = unique(shaps$var), labels = pred_labeller(unique(shaps$var)))) %>%
    mutate(across(contains('pred'), ~ifelse(is.na(.x),0,.x)))
  connection_diffs_c <- left_join(connection_shaps_diffs_c, connection_predictors_diffs_c) %>%
    mutate(metric_diff = connection_metric_diffs$val_diff[c])
  
  connection_diffs <- rbind(connection_diffs, connection_diffs_c)
}

shaps_performance <- connection_diffs %>%
  group_by(connection_id) %>%
  summarise(shap_trend_diff = sum(shap_diff),
            metric_trend_diff = mean(metric_diff)) %>%
  filter(connection_id != 175)
cor(shaps_performance$metric_trend_diff, shaps_performance$shap_trend_diff)^2
ggplot(shaps_performance, aes(x = metric_trend_diff, y = shap_trend_diff)) +
  geom_abline(slope = 1) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_vline(xintercept = 0, color = 'grey50') +
  geom_point() +
  xlab(paste('Difference in',metric_sel,'trend')) +
  ylab('Sum of differences in SHAP trends')
# ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/shap_trend_diffs_performance.png'), height = 3.5, width = 4, units = 'in')


connection_diffs <- connection_diffs %>%
  left_join(select(connections, connection_id, site_no = headwater_id)) %>%
  left_join(select(all_gage_info, site_no, region)) %>%
  left_join(select(connection_metric_diffs, connection_id, sig_cat_hw, sig_cat_ds, sig_diff))

ggplot(connection_diffs, aes(x = fct_reorder(var, abs(shap_diff_scale), .desc = T), y = shap_diff_scale)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  xlab('') +
  ylab('Difference in SHAP trends') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/shap_trend_diffs.png'), height = 5, width = 8, units = 'in')

connection_diffs_var_summary <- connection_diffs %>%
  group_by(var) %>%
  summarise(shap_diff = mean(abs(shap_diff)))
ggplot(connection_diffs_var_summary, aes(y = fct_reorder(var, shap_diff), x = shap_diff)) +
  geom_col() +
  ylab('') +
  xlab('Mean |SHAP trend diff|')
# ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/shap_trend_diffs.png'), height = 5, width = 4, units = 'in')

connection_diffs_cn_summary <- connection_diffs %>%
  group_by(connection_id, site_no) %>%
  summarise(top_var = var[abs(shap_diff_scale) == max(abs(shap_diff_scale))][1],
            var_pct = shap_diff_pct[var == top_var]) %>%
  left_join(connection_metric_diffs)
table(connection_diffs_cn_summary$top_var)




# map plotting -------------------------------------------------------------
library(sf)
source('scripts/functions/load_states.R')

point_dat <- left_join(connection_diffs_cn_summary,
                       select(all_gage_info, site_no, lat, lon)) %>%
  mutate(top_var_cat = labelinator(inv_labelinator(top_var, pred_labels), pred_cats),
         top_var_cat = factor(top_var_cat, levels = unique(names(pred_cats)))) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(point_dat) <- 4326
  
#color connections by top shap var
ggplot() +
  geom_sf(data = ext) +
  geom_sf(data = point_dat, aes(color = top_var, size = sig_diff)) +
  scale_size_discrete(name = 'Significant difference in Q95 trends?') +
  scale_color_discrete(name = 'Predictor with largest SHAP trend diff')
ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/connection_top_diffs_map.png'), height = 7.5, width = 10, units = 'in')

#color connections by top shap var CATEGORY
ggplot() +
  geom_sf(data = ext) +
  geom_sf(data = point_dat, aes(color = top_var_cat, size = sig_diff)) +
  scale_size_discrete(name = 'Significant difference in Q95 trends?') +
  scale_color_discrete(name = 'Predictor with largest SHAP trend diff')
ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/connection_top_diffs_cat_map.png'), height = 7.5, width = 10, units = 'in')
#CATEGORY bars
ggplot(point_dat, aes(x = top_var_cat, fill = top_var_cat, alpha = sig_diff)) +
  geom_bar(color = 'black') +
  scale_fill_discrete(guide = NULL) +
  scale_alpha_discrete(name = 'Significant difference in Q95 trends?') +
  xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom')
ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/connection_top_diffs_cat_map.png'), height = 4, width = 3.5, units = 'in')




# clustering of connection diffs ------------------------------------------
library(cluster)
library(factoextra)

hclust_dat <- connection_diffs %>%
  pivot_wider(id_cols = connection_id, names_from = var, values_from = shap_diff_scale) %>%
  select(!connection_id)

dist <- daisy(hclust_dat, stand = F)
clus <- hclust(dist, method = 'ward.D2')
plot(clus)

fviz_nbclust(hclust_dat, hcut, k.max = 24, diss = dist, method = 'sil')

groups <- tibble(
  connection_id = connection_metric_diffs$connection_id,
  group = cutree(clus, k=7))
table(groups$group)

plot_dat <- left_join(connection_diffs, groups) %>%
  filter(connection_id != 175)
ggplot(plot_dat, aes(x = fct_reorder(var, abs(shap_diff_scale), .desc = T), y = shap_diff_scale, fill = factor(group))) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  facet_wrap(vars(factor(group)), ncol = 2, scales = 'free_y') +
  scale_fill_discrete(guide = NULL) +
  xlab('') +
  ylab('SHAP trend difference') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0('figures/annual_model/',metric_sel,'/all/shap_trend_diffs_cluster.png'), height = 7.5, width = 10, units = 'in')


ggplot(filter(plot_dat, var == 'Precip (annual)'), aes(x = pred_diff, y = shap_diff_scale, color = factor(group))) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_vline(xintercept = 0, color = 'grey50') +
  geom_point() +
  facet_wrap(vars(factor(group)))

table(select(plot_dat, group, region))/33
ggplot(plot_dat, aes(x = region, fill = factor(group))) +
  geom_bar(position = 'dodge2', color = 'black') +
  facet_wrap(vars(factor(group)))


library(FactoMineR)
pca <- FactoMineR::PCA(hclust_dat)

