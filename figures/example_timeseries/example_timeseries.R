# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'FlashinessIndex'

model_name <- paste0(tolower(metric_sel),'_annual_w3_emp')
model_dir <- paste0('data/models/',model_name,'/')

#Indiana ag/precip Q95 example: '05536255'
#Long Island Flashiness example: '01311500'
#Konza land cover '06879650'
#KSRB headwaters '06823500'
#Front range impact of elev '09126000'
#Snow in the desert for Flashiness? '09430600', '09497980'
sites_sel <- '09497980'

predictions <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  filter(site_no %in% sites_sel)

trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(site_no %in% sites_sel) %>%
  mutate(var_name = labelinator(var, pred_labels))

shaps <- read_csv(paste0(model_dir,'shaps.csv')) %>%
  filter(site_no %in% sites_sel) %>%
  mutate(var_name = labelinator(var, pred_labels))
shap_sum_ts <- shaps %>%
  group_by(wateryear) %>%
  summarise(shap_sum = sum(shap) + mean(predictions$obs))
shap_sum_trend <- trendinator(shap_sum_ts$shap_sum) %>%
  mutate(int = median(shap_sum_ts$shap_sum) - (sen*median(shap_sum_ts$wateryear)))

shaps_wide <- pivot_wider(shaps, id_cols = wateryear, names_from = var, values_from = shap) %>%
  left_join(select(predictions, wateryear, obs)) %>%
  select(!wateryear)
shaps_cor <- cor(shaps_wide, use = 'pairwise.complete', method = 'spearman')



# single site timeseries --------------------------------------------------

#metric
ggplot(predictions, aes(x = wateryear, y = obs)) +
  geom_line(alpha = 1) +
  # geom_abline(data = filter(trends, var == metric_sel, type == 'obs'),
  #             aes(slope = sen, intercept = int),
  #             lty = 'dashed') +
  geom_line(data = shap_sum_ts, aes(x = wateryear, y = shap_sum), color = 'blue', alpha = 1) +
  # geom_abline(slope = shap_sum_trend$sen, intercept = shap_sum_trend$int,
  #             lty = 'dashed', color = 'blue') +
  ylab(metric_sel) +
  xlab('') +
  ggtitle(paste(metric_sel,'at site',sites_sel))
ggsave(paste0('figures/example_timeseries/',metric_sel,'_',sites_sel[1],'_','metric.png'),
       height = 2.5, width = 3.5, units = 'in')


pred_sel <- unique(shaps$var)#'melt_duration'

#predictor
ggplot(filter(shaps, var %in% pred_sel), aes(x = wateryear, y = val)) +
  geom_line() +
  geom_abline(data = filter(trends, var %in% pred_sel, type == 'obs'),
              aes(slope = sen, intercept = int),
              lty = 'dashed') +
  facet_wrap(vars(var_name), ncol = 5, scales = 'free_y') +
  ylab(labelinator(pred_sel, pred_labels)) +
  xlab('') +
  ggtitle(paste('Predictor values at site',sites_sel))
# ggsave(paste0('figures/example_timeseries/',metric_sel,'_',sites_sel[1],'_allvars_predictors.png'),
#        height = 2.5, width = 3.5, units = 'in')

#shap
ggplot(filter(shaps, var %in% pred_sel), aes(x = wateryear, y = shap)) +
  geom_line(alpha = 0.25, color = 'blue') +
  geom_abline(data = filter(trends, var %in% pred_sel, type == 'shap'),
              aes(slope = sen, intercept = int),
              lty = 'dashed', color = 'blue') +
  facet_wrap(vars(var_name), ncol = 5) +
  ylab('SHAP') +
  xlab('') +
  ggtitle(paste('SHAP values at site',sites_sel)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave(paste0('figures/example_timeseries/',metric_sel,'_',sites_sel[1],'_allvars_SHAP.png'),
#        height = 7, width = 8, units = 'in')

#shap trends
ggplot(filter(trends, var %in% pred_sel, type == 'shap'), aes(x = fct_reorder(var_name, sen, .desc = T), y = sen)) +
  geom_col() +
  ylab('SHAP trend (Sen\'s slope)') +
  xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ggsave(paste0('figures/example_timeseries/',metric_sel,'_',sites_sel[1],'_allvars_SHAP_trends.png'),
#        height = 5, width = 8, units = 'in')
sum(filter(trends, var %in% pred_sel, type == 'shap')$sen)
trends$sen[trends$var == metric_sel & trends$type == 'obs']

#local shap importance scatters
ggplot(filter(shaps, var %in% pred_sel), aes(x = val, y = shap, color = wateryear)) +
  geom_point() +
  facet_wrap(vars(var_name), scales = 'free', ncol = 5)



# connection dual timeseries ----------------------------------------------

metric_sel <- 'FlashinessIndex'

model_name <- paste0(tolower(metric_sel),'_annual_w3_emp')
model_dir <- paste0('data/models/',model_name,'/')
connection_sel <- 169

#flashiness drainage area - 193, 169, 129, 130, 131

sites_sel <- c(connections$headwater_id[connections$connection_id == connection_sel],
               connections$downstream_id[connections$connection_id == connection_sel])

shaps <- read_csv(paste0(model_dir,'shaps.csv')) %>%
  filter(site_no %in% sites_sel) %>%
  mutate(gage = ifelse(site_no == sites_sel[1], 'upstream', 'downstream')) %>%
  mutate(var_name = labelinator(var, pred_labels))

predictions <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  filter(site_no %in% sites_sel) %>%
  mutate(gage = ifelse(site_no == sites_sel[1], 'upstream', 'downstream'))

trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(site_no %in% sites_sel) %>%
  mutate(gage = ifelse(site_no == sites_sel[1], 'upstream', 'downstream')) %>%
  mutate(var_name = labelinator(var, pred_labels))


#metric
ggplot(predictions, aes(x = wateryear, y = obs, color = gage)) +
  geom_line(alpha = 0.35) +
  geom_abline(data = filter(trends, var == metric_sel, type == 'obs'),
              aes(slope = sen, intercept = int, color = gage),
              lty = 'dashed') +
  ylab(metric_sel) +
  xlab('') +
  scale_color_discrete(limits = c('upstream', 'downstream'), guide = NULL) +
  ggtitle(paste(metric_sel,'at connection',connection_sel))
ggsave(paste0('figures/example_timeseries/',metric_sel,'_connection_',connection_sel,'_metric.png'),
       height = 2.5, width = 3.5, units = 'in')


pred_sel <- unique(shaps$var)

#predictor
ggplot(filter(shaps, var %in% pred_sel), aes(x = wateryear, y = val, color = gage)) +
  geom_line(alpha = 0.5) +
  geom_abline(data = filter(trends, var %in% pred_sel, type == 'obs'),
              aes(slope = sen, intercept = int, color = gage),
              lty = 'dashed') +
  facet_wrap(vars(var_name), scales = 'free_y') +
  ylab(labelinator(pred_sel, pred_labels)) +
  xlab('') +
  scale_color_discrete(limits = c('upstream', 'downstream'), guide = NULL) +
  ggtitle(paste(labelinator(pred_sel, pred_labels),'at connection',connection_sel))
ggsave(paste0('figures/example_timeseries/',metric_sel,'_connection_',connection_sel,'_',pred_sel,'.png'),
       height = 2.5, width = 3.5, units = 'in')

#shap
ggplot(filter(shaps, var %in% pred_sel), aes(x = wateryear, y = shap, color = gage)) +
  geom_line(alpha = 0.35) +
  geom_abline(data = filter(trends, var %in% pred_sel, type == 'shap'),
              aes(slope = sen, intercept = int, color = gage),
              lty = 'dashed') +
  facet_wrap(vars(var_name), ncol = 5) +
  ylab(paste(labelinator(pred_sel, pred_labels),'SHAP')) +
  xlab('') +
  scale_color_discrete(limits = c('upstream', 'downstream'), guide = NULL) +
  ggtitle(paste('SHAPs at connection',connection_sel))
ggsave(paste0('figures/example_timeseries/',metric_sel,'_connection_',connection_sel,'_allvars_SHAP.png'),
       height = 7, width = 8, units = 'in')


#all shap trend diffs
shap_trend_diffs <- filter(trends, type == 'shap') %>%
  pivot_wider(id_cols = var, names_from = gage, values_from = sen) %>%
  mutate(diff = upstream-downstream,
         abs_diff = abs(upstream) - abs(downstream),
         var_name = labelinator(var, pred_labels))

ggplot(shap_trend_diffs, aes(x = fct_reorder(var_name, diff, .desc = T), y = diff)) +
  geom_col() +
  ylab('\u0394 SHAP trend') +
  xlab('') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0('figures/example_timeseries/',metric_sel,'_connection_',connection_sel,'_allvars_SHAP_trend_diffs.png'),
       height = 5, width = 8, units = 'in')
sum(shap_trend_diffs$diff)
(trends$sen[trends$type == 'obs' & trends$var == metric_sel & trends$gage == 'upstream']) - (trends$sen[trends$type == 'obs' & trends$var == metric_sel & trends$gage == 'downstream'])

