# data --------------------------------------------------------------------
library(tidyverse)
library(ggExtra)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'FlashinessIndex'

model_name <- paste0(tolower(metric_sel),'_annual')
model_dir <- paste0('data/models/',model_name,'/')

region_sel <- 'all'

sites_sel <- all_gage_info$site_no
if(region_sel != 'all') sites_sel <- all_gage_info$site_no[all_gage_info$region %in% region_sel]

shaps <- read_csv(paste0(model_dir,'shaps.csv')) %>%
  filter(site_no %in% sites_sel)

predictions <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  filter(site_no %in% sites_sel)

trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(site_no %in% sites_sel)

#gage counts
ggplot(filter(all_gage_info, site_no %in% sites_sel), aes(x = factor(order))) +
  geom_bar()

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
# ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/global_shap_byorder.png'), height = 5, width = 8, units = 'in')

#local importance
ggplot(subset(shaps, var == 'water_use_mean'), aes(x = val, y = shap)) +
  geom_point()


# SHAP trends -------------------------------------------------------------









# Plotting outputs ---------------------------------------------------------
#time series and box plots and such

### metric ------------------------------------------------------------------

#load data and calculate trend lines
metric_trend_lines <- filter(trends, var == metric_sel) %>%
  group_by(order_lump2) %>%
  summarise(slope = median(sen, na.rm = T),
            int = median(int,na.rm = T))

#trend box plot by order
ggplot(filter(trends, var == metric_sel), aes(x = factor(order), y = sen, fill = factor(order))) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outliers = F) +
  xlab('Stream Order') +
  ylab('Sen\'s Slope') +
  scale_fill_discrete(guide = 'none')
#by order grouping
ggplot(filter(trends, var == metric_sel), aes(x = order_lump2, y = sen, fill = order_lump2)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outliers = F) +
  scale_x_discrete(limits = c('hw', 'ds'), labels = c('Order 1-3', 'Order 4-6')) +
  scale_fill_discrete(limits = c('hw', 'ds'), guide = 'none')  +
  xlab('') +
  ylab(paste(metric_sel,'Sen\'s Slope'))
# ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/raw_trend_boxes.png'), height = 3, width = 3, units = 'in')

#metric timeseries
ggplot(predictions, aes(x = wateryear, y = obs, color = order_lump2, group = site_no)) +
  geom_line(alpha = 0.3) +
  geom_abline(data = metric_trend_lines, aes(slope = slope, intercept = int, color = order_lump2),
              linewidth = 1) +
  scale_color_discrete(limits = c('hw', 'ds'), labels = c('Order 1-3', 'Order 4-6'), name = '') +
  xlab('') +
  ylab(metric_sel)
# ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/raw_value_timeseries.png'), height = 4, width = 8, units = 'in')


### predictors --------------------------------------------------------------

predictor_trend_lines <- filter(trends, var != metric_sel, type == 'obs') %>%
  group_by(var, order_lump2) %>%
  summarise(slope = median(sen, na.rm = T),
            int = median(int, na.rm = T))

#predictor trend box plots by order
ggplot(filter(trends, var != metric_sel, type == 'obs'), aes(x = factor(order), y = sen, fill = factor(order))) +
  geom_boxplot() +
  facet_wrap(vars(var), scales = 'free_y')
#by order grouping
ggplot(filter(trends, var != metric_sel, type == 'obs'), aes(x = order_lump2, y = sen, fill = order_lump2)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  scale_x_discrete(limits = c('hw', 'ds')) +
  scale_fill_discrete(limits = c('hw', 'ds')) +
  facet_wrap(vars(var), scales = 'free_y')

#predictor timeseries
ggplot(shaps, aes(x = wateryear, y = val, color = order_lump2, group = site_no)) +
  geom_line(alpha = 0.15) +
  geom_abline(data = predictor_trend_lines, aes(slope = slope, intercept = int, color = order_lump2),
              linewidth = 1) +
  scale_color_discrete(limits = c('hw', 'ds'), labels = c('Order 1-3', 'Order 4-6'), name = NULL) +
  facet_wrap(vars(pred_labeller(var)), scales = 'free_y', ncol = 7) +
  xlab('') +
  ylab('') +
  theme(legend.position = 'bottom')
# ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/all_predictors_timeseries.png'), height = 6, width = 13, units = 'in')



### SHAPS -------------------------------------------------------------------

shap_trend_lines <- filter(trends, type == 'shap') %>%
  group_by(var, order_lump2) %>%
  summarise(slope = median(sen, na.rm = T),
            int = median(int, na.rm = T))

#shap trend boxplots by order
ggplot(filter(trends, type == 'shap'), aes(x = factor(order), y = sen, fill = factor(order))) +
  geom_boxplot() +
  facet_wrap(vars(var))
#by order grouping
ggplot(filter(trends, type == 'shap'), aes(x = order_lump2, y = sen, fill = order_lump2)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  scale_x_discrete(limits = c('hw', 'ds')) +
  scale_fill_discrete(limits = c('hw', 'ds')) +
  facet_wrap(vars(var))

#shap timeseries
ggplot(shaps, aes(x = wateryear, y = shap, color = order_lump2, group = site_no)) +
  geom_line(alpha = 0.3) +
  geom_abline(data = shap_trend_lines, aes(slope = slope, intercept = int, color = order_lump2),
              linewidth = 1) +
  scale_color_discrete(limits = c('hw', 'ds')) +
  facet_wrap(vars(var), scales = 'free_y')







shap_trend_diffs$var <- factor(shap_trend_diffs$var, labels = pred_labeller(shap_trend_diffs$var))

shap_trend_diffs

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
ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/all_shap_trend_diffs.png'), height = 5, width = 8)


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
ggsave(paste0('figures/annual_model/',metric_sel,'/',region_sel,'/shap_trend_scatter_',predictor_sel,'.png'), plot = psave, height = 4, width = 5, units = 'in')

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
