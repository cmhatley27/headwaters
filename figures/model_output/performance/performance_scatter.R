# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/Theme+Settings.R')

model_name <- 'hw_order2_senval'
fig_dir <- paste0('figures/model_output/performance/',model_name,'/scatter/')
if(!dir.exists(fig_dir)) dir.create(fig_dir)

performance_summary <- read_csv(paste0('data/models/performance/',model_name,'_summary.csv'))
predictions <- read_csv(paste0('data/models/performance/',model_name,'_predictions.csv'))

metrics_sel = performance_summary$var

# single plot -------------------------------------------------------------
metric_sel =  metrics_sel[2]
ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(data = subset(predictions, var == metric_sel), aes(x = obs, y = pred)) +
  geom_abline(slope = 1) +
  xlab('Predicted') +
  ylab('Observed') +
  ggtitle(paste0(metric_sel,', R2 = ',performance_summary$r2[performance_summary$var == metric_sel]))
# ggsave(paste0(fig_dir,metric,'.png'), height = 6, width = 8, units = 'in')


# all plots ---------------------------------------------------------------
for(i in 1:length(metrics_sel)){
  metric_sel = metrics_sel[i]
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_point(data = subset(predictions, var == metric_sel), aes(x = obs, y = pred)) +
    geom_abline(slope = 1) +
    xlab('Observed') +
    ylab('Predicted') +
    ggtitle(paste0(metric_sel,', R2 = ',performance_summary$r2[performance_summary$var == metric_sel])) +
    theme(text = element_text(size = 14))
  ggsave(paste0(fig_dir,metric_sel,'.png'), height = 5, width = 6, units = 'in')
}
