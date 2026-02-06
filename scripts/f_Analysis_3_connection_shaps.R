# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'Q95'

model_name <- 'q95_annual'

model_dir <- paste0('data/models/',model_name,'/')

region_sel <- 'all'
if(region_sel == 'all') region_sel <- unique(all_gage_info$region)

sites_sel <- all_gage_info$site_no[all_gage_info$region %in% region_sel]
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
    order_lump3 = case_when(
      order <= 2 ~ 'hw',
      order == 3 ~ 'order3',
      order >= 4 ~ 'ds'
    ),
    order_lump2 = case_when(
      order <= 3 ~ 'hw',
      order > 3 ~ 'ds'
    )
  ) %>%
  left_join(predictor_variables) %>%
  filter(site_no %in% sites_sel)

shaps_trends <- shaps %>%
  group_by(site_no, order, order_lump2, order_lump3, var) %>%
  summarise(across(shap, ~trendinator(.x), .unpack = '{inner}'),
            int = median(shap)-(sen*median(wateryear)))
predictors_trends <- shaps %>%
  group_by(site_no, order, order_lump2, order_lump3, var) %>%
  summarise(across(val, ~trendinator(.x), .unpack = '{inner}'),
            int = median(val)-(sen*median(wateryear)))

metric_values <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  rename(val = obs) %>%
  left_join(select(all_gage_info, site_no, order)) %>%
  filter(site_no %in% sites_sel) %>%
  mutate(
    order_lump3 = case_when(
      order <= 2 ~ 'hw',
      order == 3 ~ 'order3',
      order >= 4 ~ 'ds'
    ),
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
  select(site_no, val_sen, pred_sen, val_int, pred_int)
connection_trends <- connections %>%
  left_join(metric_trends, by = join_by(headwater_id == site_no)) %>%
  left_join(metric_trends, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds')) %>%
  mutate(val_diff = val_sen_hw - val_sen_ds,
         pred_diff = pred_sen_hw - pred_sen_ds) %>%
  filter(headwater_id %in% shaps$site_no, downstream_id %in% shaps$site_no)

ggplot(connection_trends, aes(x = val_diff, y = pred_diff)) +
  geom_abline(slope = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point()

# loop through connections ------------------------------------------------
connections_sel <- connection_trends$connection_id

connection_shaps_diffs <- tibble()
for(c in seq_along(connections_sel)){
  connection_sel <- connections_sel[c]
  
  gages_sel <- c(connections$headwater_id[connection_sel], connections$downstream_id[connection_sel])
  names(gages_sel) <- c('Upstream', 'Downstream')
  
  connection_shaps_diffs_c <- shaps_trends %>%
    filter(site_no %in% gages_sel) %>%
    mutate(site_no = factor(site_no, levels = gages_sel, labels = names(gages_sel))) %>%
    pivot_wider(id_cols = var, names_from = site_no, values_from = sen) %>%
    mutate(shap_diff = Upstream-Downstream,
           shap_diff_sum = sum(shap_diff),
           shap_diff_pct = shap_diff/shap_diff_sum,
           shap_diff_scale = scale(shap_diff, center = F)[,1],
           connection_id = connection_sel,
           var = factor(var, levels = unique(shaps$var), labels = pred_labeller(unique(shaps$var))))
  
  connection_shaps_diffs <- rbind(connection_shaps_diffs, connection_shaps_diffs_c)
}

shaps_performance <- connection_shaps_diffs %>%
  group_by(connection_id) %>%
  summarise(shap_trend_diff = sum(shap_diff)) %>%
  mutate(metric_trend_diff = connection_trends$val_diff)

ggplot(shaps_performance, aes(x = metric_trend_diff, y = shap_trend_diff)) +
  geom_abline(slope = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point()

connection_shaps_diffs <- connection_shaps_diffs %>%
  left_join(select(connections, connection_id, site_no = headwater_id)) %>%
  left_join(select(all_gage_info, site_no, region))

ggplot(connection_shaps_diffs, aes(x = var, y = abs(shap_diff_scale), fill = region)) +
  geom_boxplot() +
  facet_wrap(vars(region)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# zoom into single connection ---------------------------------------------

connection_sel <- 165
gages_sel <- c(connections$headwater_id[connection_sel], connections$downstream_id[connection_sel])
names(gages_sel) <- c('Upstream', 'Downstream')

#plot metric
ggplot(filter(metric_values, site_no %in% gages_sel), aes(x = wateryear, y = val, color = site_no)) +
  geom_line(alpha = 0.5) +
  geom_abline(data = filter(metric_trends, site_no %in% gages_sel), aes(slope = val_sen, intercept = val_int, color = site_no)) +
  ylab(metric_sel) +
  xlab('') +
  scale_color_discrete(labels = names(gages_sel), name = NULL)
# ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/connection_metric_timeseries.png'), height = 4, width = 8, units = 'in')
metric_trends$val_sen[metric_trends$site_no %in% gages_sel]

#local global shaps
connection_global_shaps <- shaps %>%
  filter(site_no %in% gages_sel) %>%
  group_by(var, site_no) %>%
  summarise(shap = mean(abs(shap))) %>%
  arrange(desc(shap)) %>%
  mutate(site_no = factor(site_no, levels = gages_sel, labels = names(gages_sel)),
         var = factor(var, labels = pred_labeller(unique(var))))
ggplot(connection_global_shaps, aes(y = fct_reorder(var, shap), x = shap, fill = site_no)) +
  geom_col() +
  facet_wrap(vars(site_no)) +
  scale_fill_discrete(guide = NULL) +
  ylab('') +
  xlab('Mean |SHAP|')
# ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/connection_global_shap_byorder.png'), height = 5, width = 8, units = 'in')
preds_sel <- c('precip_annual', 'twi', 'ag')

#plot predictors and shaps
connection_shaps <- shaps %>%
  filter(site_no %in% gages_sel,
         var %in% preds_sel) %>%
  mutate(site_no = factor(site_no, levels = gages_sel, labels = names(gages_sel)),
         var = factor(var, levels = preds_sel, labels = pred_labeller(preds_sel)))
connection_predictors_trend <- filter(predictors_trends, site_no %in% gages_sel, var %in% preds_sel) %>%
  mutate(site_no = factor(site_no, levels = gages_sel, labels = names(gages_sel)),
         var = factor(var, levels = preds_sel, labels = pred_labeller(preds_sel)))
connection_shaps_trend <- filter(shaps_trends, site_no %in% gages_sel, var %in% preds_sel) %>%
  mutate(site_no = factor(site_no, levels = gages_sel, labels = names(gages_sel)),
         var = factor(var, levels = preds_sel, labels = pred_labeller(preds_sel)))

#plot predictors time series
ggplot(connection_shaps, aes(x = wateryear, y = val, color = site_no)) +
  geom_line(alpha = 0.5) +
  geom_abline(data = connection_predictors_trend, aes(slope = sen, intercept = int, color = site_no)) +
  facet_wrap(vars(var), scales = 'free_y', ncol = 3) +
  ylab('') +
  xlab('') +
  scale_color_discrete(guide = NULL)
# ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/connection_predictor_timeseries.png'), height = 6, width = 4, units = 'in')

#plot shaps time series
ggplot(connection_shaps, aes(x = wateryear, y = shap, color = site_no)) +
  geom_line(alpha = 0.5) +
  geom_abline(data = connection_shaps_trend, aes(slope = sen, intercept = int, color = site_no)) +
  facet_wrap(vars(var), ncol = 3) +
  ylab('') +
  xlab('') +
  scale_color_discrete(guide = NULL)
# ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/connection_shap_timeseries.png'), height = 6, width = 4, units = 'in')

#plot differences in predictor and shap slopes
ggplot(filter(connection_shaps_diffs, connection_id == connection_sel), aes(x = fct_reorder(var, shap_diff, .desc = T), y = shap_diff)) +
  geom_hline(yintercept = 0) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('') +
  ylab('Difference in SHAP trends')
# ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/connection_shap_trend_diffs.png'), height = 5, width = 8)
sum(connection_shaps_diffs$shap_diff[connection_shaps_diffs$connection_id == connection_sel])
connection_trends$val_diff[connection_trends$connection_id == connection_sel]

#shaps vs values
ggplot(filter(shaps, site_no %in% gages_sel), aes(x = val, y = shap, color = site_no)) +
  geom_point() +
  facet_wrap(vars(var), scales = 'free_x')

#compare to trend model
trend_model_shaps_diff <- read_csv('data/models/all_senval/shaps.csv') %>%
  filter(site_no %in% gages_sel, metric == metric_sel, type == 'shap') %>%
  select(!c(metric, type)) %>%
  pivot_longer(!site_no, names_to = 'var', values_to = 'shap') %>%
  mutate(site_no = factor(site_no, levels = gages_sel, labels = names(gages_sel))) %>%
  pivot_wider(id_cols = var, names_from = site_no, values_from = shap) %>%
  mutate(shap_diff = Upstream-Downstream,
         var = factor(var, levels = unique(shaps$var), labels = pred_labeller(unique(shaps$var))))
ggplot(filter(trend_model_shaps_diff, !is.na(var)), aes(x = fct_reorder(var, shap_diff, .desc = T), y = shap_diff)) +
  geom_hline(yintercept = 0) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('') +
  ylab('Difference in SHAP trends')
sum(trend_model_shaps_diff$shap_diff)
