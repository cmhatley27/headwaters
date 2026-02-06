# data --------------------------------------------------------------------
library(tidyverse)
library(ggExtra)
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
  left_join(mses) %>%
  filter(site_no %in% sites_sel)

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

#gage counts
gage_counts <- filter(all_gage_info, site_no %in% sites_sel) %>%
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
ggplot(gage_counts, aes(x = order)) +
  geom_bar() +
  scale_x_continuous(breaks = 1:6)


# SHAPs -------------------------------------------------------------------

#global importance
global_imp <- shaps %>%
  group_by(var) %>%
  summarise(shap = mean(abs(shap)))
ggplot(global_imp, aes(y = fct_reorder(factor(var), shap), x = shap)) +
  geom_col()

#global importance by order
global_imp_byorder <- shaps %>%
  group_by(var, order_lump2) %>%
  summarise(shap = mean(abs(shap))) %>%
  mutate(order_lump2 = factor(order_lump2, levels = c('hw','ds'), 
                              labels = c('Order 1-3','Order 4-6')),
         var = factor(var, labels = pred_labeller(unique(var))))
ggplot(global_imp_byorder, aes(y = fct_reorder(var, shap), x = shap, fill = order_lump2)) +
  geom_col() +
  scale_fill_discrete(guide = NULL) +
  facet_wrap(vars(order_lump2)) +
  ylab('') +
  xlab('Mean |SHAP|')
ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/global_shap_byorder.png'), height = 5, width = 8, units = 'in')

#local importance
ggplot(subset(shaps, var == 'forest'), aes(x = val, y = shap, color = mse)) +
  geom_point()

shaps_wide <- pivot_wider(shaps, id_cols = c(site_no, wateryear),
                          names_from = var, values_from = shap)
vals_wide <- pivot_wider(shaps, id_cols = c(site_no, wateryear),
                         names_from = var, values_from = val)
dat_wide <- left_join(shaps_wide, vals_wide, by = c('site_no', 'wateryear'), suffix = c('_shap','_val'))
ggplot(dat_wide, aes(x = precip_ond_val, y = max_swe_val, color = elev_val)) +
  geom_point()


# Large sample trends -------------------------------------------------
library(modifiedmk)


### metric ------------------------------------------------------------------

#load data and calculate trend lines
metric_trend_sen <- metric_values %>%
  group_by(site_no, order, order_lump2, order_lump3) %>%
  summarise(across(val, ~trendinator(.x), .unpack = '{inner}'),
            int = median(val)-(sen*median(wateryear)))
metric_sen_lines <- metric_trend_sen %>%
  group_by(order_lump2) %>%
  summarise(slope = median(sen, na.rm = T),
            int = median(int,na.rm = T))

#trend box plot by order
ggplot(metric_trend_sen, aes(x = factor(order), y = sen, fill = factor(order))) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  xlab('Stream Order') +
  ylab('Sen\'s Slope') +
  scale_fill_discrete(guide = 'none')
#by order grouping
ggplot(metric_trend_sen, aes(x = order_lump2, y = sen, fill = order_lump2)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  scale_x_discrete(limits = c('hw', 'ds'), labels = c('Order 1-3', 'Order 4-6')) +
  scale_fill_discrete(limits = c('hw', 'ds'), guide = 'none')  +
  xlab('') +
  ylab('Q95 Sen\'s Slope')
ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/raw_trend_boxes.png'), height = 3, width = 3, units = 'in')

#metric timeseries
ggplot(metric_values, aes(x = wateryear, y = val, color = order_lump2, group = site_no)) +
  geom_line(alpha = 0.3) +
  geom_abline(data = metric_sen_lines, aes(slope = slope, intercept = int, color = order_lump2),
              linewidth = 1) +
  scale_color_discrete(limits = c('hw', 'ds'), labels = c('Order 1-3', 'Order 4-6'), name = '') +
  xlab('') +
  ylab('Q95')
ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/raw_value_timeseries.png'), height = 4, width = 8, units = 'in')



### predictors --------------------------------------------------------------

#load data and calculate trend lines
predictors_trend_sen <- shaps %>%
  group_by(site_no, order, order_lump2, order_lump3, var) %>%
  summarise(across(val, ~trendinator(.x), .unpack = '{inner}'),
            int = median(val)-(sen*median(wateryear)))
predictors_sen_lines <- predictors_trend_sen %>%
  group_by(var, order_lump2) %>%
  summarise(slope = median(sen, na.rm = T),
            int = median(int, na.rm = T))

#predictor trend box plots by order
ggplot(predictors_trend_sen, aes(x = factor(order), y = sen, fill = factor(order))) +
  geom_boxplot() +
  facet_wrap(vars(var), scales = 'free_y')
#by order grouping
ggplot(predictors_trend_sen, aes(x = order_lump2, y = sen, fill = order_lump2)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  scale_x_discrete(limits = c('hw', 'ds')) +
  scale_fill_discrete(limits = c('hw', 'ds')) +
  facet_wrap(vars(var), scales = 'free_y')

#predictor timeseries
ggplot(shaps, aes(x = wateryear, y = val, color = order_lump2, group = site_no)) +
  geom_line(alpha = 0.15) +
  geom_abline(data = predictors_sen_lines, aes(slope = slope, intercept = int, color = order_lump2),
              linewidth = 1) +
  scale_color_discrete(limits = c('hw', 'ds'), labels = c('Order 1-3', 'Order 4-6'), name = NULL) +
  facet_wrap(vars(pred_labeller(var)), scales = 'free_y', ncol = 7) +
  xlab('') +
  ylab('') +
  theme(legend.position = 'bottom')
ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/all_predictors_timeseries.png'), height = 6, width = 13, units = 'in')

#calculate trend differences between upstream and downstream sites
predictors_trend_diffs <- predictors_trend_sen %>%
  group_by(var) %>%
  mutate(sen_z = scale(sen)[,1]) %>%
  group_by(var, order_lump2) %>%
  summarise(slope = median(sen_z, na.rm = T)) %>%
  pivot_wider(id_cols = var, names_from = order_lump2, values_from = slope) %>%
  mutate(slope_diff = hw-ds)


### SHAPS -------------------------------------------------------------------

#load data and calculate trend lines
shaps_trend_sen <- shaps %>%
  group_by(site_no, order, order_lump2, order_lump3, var) %>%
  summarise(across(shap, ~trendinator(.x), .unpack = '{inner}'),
            int = median(shap)-(sen*median(wateryear)))
shaps_sen_lines <- shaps_trend_sen %>%
  group_by(var, order_lump2) %>%
  summarise(slope = median(sen, na.rm = T),
            int = median(int, na.rm = T))

#shap trend boxplots by order
ggplot(shaps_trend_sen, aes(x = factor(order), y = sen, fill = factor(order))) +
  geom_boxplot() +
  facet_wrap(vars(var))
#by order grouping
ggplot(shaps_trend_sen, aes(x = order_lump2, y = sen, fill = order_lump2)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  scale_x_discrete(limits = c('hw', 'ds')) +
  scale_fill_discrete(limits = c('hw', 'ds')) +
  facet_wrap(vars(var))

#shap timeseries
ggplot(shaps, aes(x = wateryear, y = shap, color = order_lump2, group = site_no)) +
  geom_line(alpha = 0.3) +
  geom_abline(data = shaps_sen_lines, aes(slope = slope, intercept = int, color = order_lump2),
              linewidth = 1) +
  scale_color_discrete(limits = c('hw', 'ds')) +
  facet_wrap(vars(var), scales = 'free_y')

#test differences in shap trends between upstream and downstream sites
shap_trend_diffs <- data.frame(
  var = unique(shaps_trend_sen$var),
  ks = NA,
  ks_p = NA,
  w = NA,
  w_p = NA,
  median_diff = NA
)
for(v in seq_along(unique(shaps_trend_sen$var))){
  var_sel = unique(shaps_trend_sen$var)[v]
  kstest <- wilcox.test(shaps_trend_sen$sen[shaps_trend_sen$var == var_sel & shaps_trend_sen$order_lump2 == 'hw'],
                        shaps_trend_sen$sen[shaps_trend_sen$var == var_sel & shaps_trend_sen$order_lump2 == 'ds'])
  wtest <- ks.test(shaps_trend_sen$sen[shaps_trend_sen$var == var_sel & shaps_trend_sen$order_lump2 == 'hw'],
                   shaps_trend_sen$sen[shaps_trend_sen$var == var_sel & shaps_trend_sen$order_lump2 == 'ds'])
  median_hw <- median(shaps_trend_sen$sen[shaps_trend_sen$var == var_sel & shaps_trend_sen$order_lump2 == 'hw'])
  median_ds <- median(shaps_trend_sen$sen[shaps_trend_sen$var == var_sel & shaps_trend_sen$order_lump2 == 'ds'])
  shap_trend_diffs$ks[v] <- kstest$statistic
  shap_trend_diffs$ks_p[v] <- kstest$p.value
  shap_trend_diffs$w[v] <- wtest$statistic
  shap_trend_diffs$w_p[v] <- wtest$p.value
  shap_trend_diffs$median_diff[v] <- median_hw - median_ds
}
shap_trend_diffs$var <- factor(shap_trend_diffs$var, labels = pred_labeller(shap_trend_diffs$var))

#sum of predictor shap trend differences
sum(shaps_sen_lines$slope[shaps_sen_lines$order_lump2 == 'hw']) - sum(shaps_sen_lines$slope[shaps_sen_lines$order_lump2 == 'ds'])
#actual metric trend difference
metric_sen_lines$slope[metric_sen_lines$order_lump2 == 'hw'] - metric_sen_lines$slope[metric_sen_lines$order_lump2 == 'ds']

#SHAP trend differences
ggplot(shap_trend_diffs, aes(x = fct_reorder(var, median_diff, .desc = T), y = median_diff)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('') +
  ylab('Difference in median SHAP trends')
ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/all_shap_trend_diffs.png'), height = 5, width = 8)


#SHAP trend vs predictor trend scatter plots
predictor_shap_sen_comparison <- left_join(predictors_trend_sen, shaps_trend_sen,
                                           by = c('site_no', 'order', 'order_lump2', 'var'),
                                           suffix = c('_val', '_shap')) %>%
  mutate(sen_val = ifelse(is.na(sen_val), 0, sen_val))

#all variables
ggplot(filter(predictor_shap_sen_comparison, !is.na(sen_val)), aes(x = sen_val, y = sen_shap, color = order_lump2)) +
  geom_point() +
  scale_color_discrete(limits = c('hw', 'ds'), guide = 'none') +
  facet_wrap(vars(var), scales = 'free') +
  xlab('Value Sen') +
  ylab('SHAP Sen')

#select a variable
predictor_sel <- 'precip_amj'
p <- ggplot(filter(predictor_shap_sen_comparison, var == predictor_sel), aes(x = sen_val, y = sen_shap, color = order_lump2)) +
  geom_hline(yintercept = 0, color = 'grey50') +
  geom_vline(xintercept = 0, color = 'grey50') +
  geom_point() +
  scale_color_discrete(limits = c('hw', 'ds'), guide = NULL) +
  xlab('Predictor trend') +
  ylab('Predictor SHAP trend')
psave <- ggMarginal(p, type = 'boxplot', groupColour = F, groupFill = T)
psave
ggsave(paste0('figures/annual_model/',region_sel,'/',metric_sel,'/shap_trend_scatter_',predictor_sel,'.png'), plot = psave, height = 4, width = 5, units = 'in')

#shap values instead of trends
p2 <- ggplot(filter(shaps, var == predictor_sel), aes(x = val, y = shap, color = order_lump2)) +
  geom_hline(yintercept = 0, color = 'grey50') +
  # geom_vline(xintercept = 0, color = 'grey50') +
  geom_point() +
  scale_color_discrete(limits = c('hw', 'ds'), guide = 'none') +
  xlab('Value') +
  ylab('SHAP')
ggMarginal(p2, type = 'density', groupColour = F, groupFill = T)

#calculate SHAP value differences between upstream and downstream sites
#(value, not trend) to compare to the trend differences
shaps_global <- shaps %>%
  group_by(var, order_lump2) %>%
  summarise(shap = mean(abs(shap))) %>%
  pivot_wider(id_cols = var, names_from = order_lump2, values_from = shap) %>%
  mutate(diff = hw-ds)
ggplot(shaps_global, aes(x = fct_reorder(var, diff, .desc = T), y = diff)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
