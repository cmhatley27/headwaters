# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')

metric_sel <- 'Q95'

model_name <- paste0(tolower(metric_sel),'_annual_w3_emp')
model_dir <- paste0('data/models/',model_name,'/')

sites_sel <- '05536255'

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
  geom_line(data = shap_sum_ts, aes(x = wateryear, y = shap_sum), color = 'red', alpha = 1) +
  # geom_abline(slope = shap_sum_trend$sen, intercept = shap_sum_trend$int,
  #             lty = 'dashed', color = 'blue') +
  scale_y_continuous(name = paste(metric_sel, '[mm]'),
                     labels = NULL,
                     breaks = NULL) +
  xlab(NULL) +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.title.y = element_text(margin = margin(r = 2.5, unit = 'mm')))
ggsave(paste0('figures/methods/rf_prediction.png'),
       height = 25, width = 40, units = 'mm', dpi = 900)

#predictors
preds_sel <- c('precip_annual', 'ag', 'slope')
preds_cols <- c('#d55e00', '#009e73', '#cc79a7')
shap_lims <- range(filter(shaps, var %in% preds_sel)$shap)

#values
for(p in 1:length(preds_sel)){
  pred_sel <- preds_sel[p]
  
  ggplot(filter(shaps, var == pred_sel), aes(x = wateryear, y = shap)) +
    geom_line(alpha = 1, color = preds_cols[p]) +
    xlab(NULL) +
    scale_y_continuous(name = ifelse(p == 1, 'SHAP [mm]', ''),
                       limits = shap_lims,
                       labels = NULL,
                       breaks = NULL) +
    ggtitle(paste('Predictor',p)) +
    theme(panel.border = element_blank(),
          axis.line = element_line(linewidth = 0.25),
          plot.title = element_text(face = 'plain', hjust = 0.5),
          axis.title.y = element_text(margin = margin(r = 2.5, unit = 'mm')))
  ggsave(paste0('figures/methods/shap_ts_',p,'.png'),
         height = 25, width = 40, units = 'mm', dpi = 900)
}

#trends
for(p in 1:length(preds_sel)){
  pred_sel <- preds_sel[p]
  
  ggplot(filter(shaps, var == pred_sel), aes(x = wateryear, y = shap)) +
    geom_line(alpha = 0.25, color = preds_cols[p]) +
    geom_abline(data = filter(trends, var %in% pred_sel, type == 'shap'),
                aes(slope = sen, intercept = int),
                lty = 'dashed', color = preds_cols[p]) +
    xlab(NULL) +
    scale_y_continuous(name = ifelse(p == 1, 'SHAP [mm]', ''),
                       limits = shap_lims,
                       labels = NULL,
                       breaks = NULL) +
    ggtitle('') +
    theme(panel.border = element_blank(),
          axis.line = element_line(linewidth = 0.25),
          plot.title = element_text(face = 'plain', hjust = 0.5),
          axis.title.y = element_text(margin = margin(r = 2.5, unit = 'mm')))
  ggsave(paste0('figures/methods/shap_trend_',p,'.png'),
         height = 25, width = 40, units = 'mm', dpi = 900)
}

#trend bars
ggplot(filter(trends, var %in% preds_sel, type == 'shap'), aes(y = var, x = abs(sen), fill = var)) +
  geom_col(color = 'black', linewidth = 0.25) +
  scale_x_continuous(n.breaks = 4) +
  scale_y_discrete(limits = rev(preds_sel),
                   labels = rev(paste('Predictor',1:length(preds_sel)))) +
  scale_fill_manual(limits = preds_sel, values = preds_cols, guide = NULL) +
  ylab(NULL) +
  xlab('|SHAP Trend|') +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.25))
ggsave(paste0('figures/methods/shap_trend_bars.png'),
       height = 30, width = 40, units = 'mm', dpi = 900)





# data --------------------------------------------------------------------
connection_sel <- 165
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


# metric timeseries -------------------------------------------------------
ggplot(filter(predictions, gage == 'upstream', var == metric_sel), aes(x = wateryear, y = obs)) +
  geom_line() +
  # geom_abline(data = filter(trends, gage == 'upstream', type == 'obs'),
  #             aes(slope = sen, intercept = int), lty = 'dashed') +
  geom_line(aes(y = pred), color = 'red', alpha = 1) +
  # geom_abline(data = filter(trends, gage == 'upstream', type == 'prediction'),
  #             aes(slope = sen, intercept = int), color  ='red', lty = 'dashed') +
  scale_y_continuous(name = paste(metric_sel, '[mm]'),
                     labels = NULL,
                     breaks = NULL) +
  xlab(NULL) +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.title.y = element_text(margin = margin(r = 2.5, unit = 'mm')))
ggsave(paste0('figures/methods/rf_prediction_trend_pred_only.png'),
       height = 25, width = 40, units = 'mm', dpi = 900)

#predictors
preds_sel <- c('precip_annual', 'ag', 'slope')
preds_cols <- c('#0077BB', '#EE7733', '#009988')
shap_lims <- range(filter(shaps, var %in% preds_sel)$shap)

#values
for(p in 1:length(preds_sel)){
  pred_sel <- preds_sel[p]
  
  ggplot(filter(shaps, gage == 'upstream', var == pred_sel), aes(x = wateryear, y = shap)) +
    geom_line(alpha = 1, color = preds_cols[p]) +
    xlab(NULL) +
    scale_y_continuous(name = ifelse(p == 1, 'SHAP [mm]', ''),
                       limits = shap_lims,
                       labels = NULL,
                       breaks = NULL) +
    ggtitle(paste('Predictor',p)) +
    theme(panel.border = element_blank(),
          axis.line = element_line(linewidth = 0.25),
          plot.title = element_text(face = 'plain', hjust = 0.5),
          axis.title.y = element_text(margin = margin(r = 2.5, unit = 'mm')))
  ggsave(paste0('figures/methods/shap_ts_',p,'.png'),
         height = 25, width = 40, units = 'mm', dpi = 900)
}

#trends
for(p in 1:length(preds_sel)){
  pred_sel <- preds_sel[p]
  
  ggplot(filter(shaps, gage == 'upstream', var == pred_sel), aes(x = wateryear, y = shap)) +
    geom_line(alpha = 0.25, color = preds_cols[p]) +
    geom_abline(data = filter(trends, gage == 'upstream', var %in% pred_sel, type == 'shap'),
                aes(slope = sen, intercept = int),
                lty = 'dashed', color = preds_cols[p]) +
    xlab(NULL) +
    scale_y_continuous(name = ifelse(p == 1, 'SHAP [mm]', ''),
                       limits = shap_lims,
                       labels = NULL,
                       breaks = NULL) +
    ggtitle('') +
    theme(panel.border = element_blank(),
          axis.line = element_line(linewidth = 0.25),
          plot.title = element_text(face = 'plain', hjust = 0.5),
          axis.title.y = element_text(margin = margin(r = 2.5, unit = 'mm')))
  ggsave(paste0('figures/methods/shap_trend_',p,'.png'),
         height = 25, width = 40, units = 'mm', dpi = 900)
}

#trend bars
ggplot(filter(trends, gage == 'upstream', var %in% preds_sel, type == 'shap'), aes(y = var, x = abs(sen), fill = var)) +
  geom_col(color = 'black', linewidth = 0.25) +
  scale_x_continuous(breaks = NULL) +
  scale_y_discrete(limits = rev(preds_sel),
                   labels = rev(paste('Predictor',1:length(preds_sel)))) +
  scale_fill_manual(limits = preds_sel, values = preds_cols, guide = NULL) +
  ylab(NULL) +
  xlab('|SHAP Trend|') +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.25))
ggsave(paste0('figures/methods/shap_trend_bars.png'),
       height = 30, width = 40, units = 'mm', dpi = 900)
  


# paired stuff ------------------------------------------------------------

#metric timeseries
ggplot(filter(predictions, var == metric_sel), aes(x = wateryear, y = obs, color = gage)) +
  # geom_line() +
  # geom_abline(data = filter(trends, gage == 'upstream', type == 'obs'),
  #             aes(slope = sen, intercept = int), lty = 'dashed') +
  geom_line(aes(y = pred), alpha = 0.25) +
  geom_abline(data = filter(trends, type == 'prediction'),
              aes(slope = sen, intercept = int, color = gage, lty = gage)) +
  scale_y_continuous(name = paste(metric_sel, '[mm]'),
                     labels = NULL,
                     breaks = NULL) +
  xlab(NULL) +
  scale_color_manual(limits = c('upstream', 'downstream'),
                     values = c('red', 'blue'), guide = NULL) +
  scale_linetype_manual(limits = c('upstream', 'downstream'),
                        values = c('dashed', 'twodash'), guide = NULL) +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.title.y = element_text(margin = margin(r = 2.5, unit = 'mm')))
ggsave(paste0('figures/methods/paired_prediction.png'),
       height = 25, width = 40, units = 'mm', dpi = 900)
  
#trends
for(p in 1:length(preds_sel)){
  pred_sel <- preds_sel[p]
  
  ggplot(filter(shaps, var == pred_sel), aes(x = wateryear, y = shap, color = gage)) +
    geom_line(alpha = 0.25) +
    geom_abline(data = filter(trends, var %in% pred_sel, type == 'shap'),
                aes(slope = sen, intercept = int, lty = gage, color = gage)) +
    xlab(NULL) +
    scale_y_continuous(name = ifelse(p == 1, 'SHAP [mm]', ''),
                       # limits = shap_lims,
                       labels = NULL,
                       breaks = NULL) +
    scale_color_manual(limits = c('upstream', 'downstream'),
                       values = c('red', 'blue'), guide = NULL) +
    scale_linetype_manual(limits = c('upstream', 'downstream'),
                          values = c('dashed', 'twodash'), guide = NULL) +
    ggtitle(paste('Predictor',p)) +
    # coord_cartesian(xlim = range(shaps$wateryear), ylim = c(-0.3,0.3)) +
    # facet_wrap(vars(gage), ncol = 1) +
    theme(panel.border = element_blank(),
          axis.line = element_line(linewidth = 0.25),
          plot.title = element_text(face = 'plain', hjust = 0.5),
          axis.title.y = element_text(margin = margin(r = 2.5, unit = 'mm')),
          strip.text = element_blank()) 
    
  ggsave(paste0('figures/methods/paired_shap_trend_',p,'.png'),
         height = 25, width = 40, units = 'mm', dpi = 900)
}


sen_diffs <- filter(trends, var %in% preds_sel, type == 'shap') %>%
  group_by(var) %>%
  summarise(diff = abs(sen[gage == 'upstream'] - sen[gage == 'downstream']))
#trend bars
ggplot(sen_diffs, aes(y = var, x = abs(diff), fill = var)) +
  geom_col(color = 'black', linewidth = 0.25) +
  scale_x_continuous(breaks = NULL) +
  scale_y_discrete(limits = rev(fct_reorder(sen_diffs$var, sen_diffs$diff)),
                   labels = rev(paste('Predictor',c(2,1,3)))) +
  scale_fill_manual(limits = preds_sel, values = preds_cols, guide = NULL) +
  ylab(NULL) +
  xlab('|\u0394SHAP Trend|') +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.25))
ggsave(paste0('figures/methods/paired_shap_trend_bars.png'),
       height = 30, width = 40, units = 'mm', dpi = 900)
  
  