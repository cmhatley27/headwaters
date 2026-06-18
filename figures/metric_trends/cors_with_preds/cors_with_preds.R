library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/Theme+Settings.R')
source('scripts/functions/var_names.R')

metric_trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  select(site_no, var, tau, sen, p)

pred_trends <- read_csv('data/gages/predictors/pred_trends_window3.csv') %>%
  select(site_no, var, tau, sen, p)
preds_sel <- c('precip_annual','pet_annual','ppet_annual',
               'precip_jfm','pet_jfm','ppet_jfm',
               'precip_amj','pet_amj','ppet_amj',
               'precip_jas','pet_jas','ppet_jas', 
               'precip_ond','pet_ond','ppet_ond',
               'si',
               'swe_annual','max_swe','max_swe_day', 'zero_swe_day', 'swe_persistence', 'melt_duration',
               'ag','developed','forest','grass')
metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')

for(m in metrics_sel){
  metric_sel <- m
  metric_name <- labelinator(metric_sel)
  
  trends <- filter(metric_trends, var == metric_sel) %>%
    rbind(pred_trends) %>%
    filter(var %in% c(metric_sel, preds_sel)) %>%
    pivot_wider(id_cols = site_no, names_from = var, values_from = tau)
  
  cors <- select(trends, !site_no) %>%
    cor(., use = 'pairwise.complete', method = 'spearman')
  cors_df <- as.data.frame(cors) %>%
    mutate(var = rownames(.)) %>%
    filter(var != metric_sel) %>%
    select(var, all_of(metric_sel)) %>%
    rename(cor = metric_sel) %>%
    mutate(var_cat = labelinator(var, pred_cats),
           var_name = labelinator(var, pred_labels))
  
  ggplot(cors_df, aes(x = abs(cor), y = fct_reorder(var_name, abs(cor)), fill = var_cat)) +
    geom_col(color = 'black') +
    scale_fill_discrete(limits = names(pred_cat_colors), name = 'Category') +
    ylab(NULL) +
    xlab('|Spearman \u03C1|') +
    ggtitle(paste(metric_name,'trend correlations'))
  ggsave(paste0('figures/metric_trends/cors_with_preds/bars/',metric_sel,'.png'), height = 4.5, width = 6.5, units = 'in')
  
  for(p in preds_sel){
    cor_p <- round(cors[metric_sel,p],3)
    ggplot(trends, aes(x = .data[[p]], y = .data[[metric_sel]])) +
      geom_point() +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_smooth(method = 'lm', se = T) +
      annotate(geom = 'text', label = paste('\u03C1:',cor_p), 
               x = -Inf, y = Inf, hjust = -0.25, vjust = 1.5) +
      xlab(paste(labelinator(p, pred_labels), 'trend')) +
      ylab(paste(metric_name, 'trend'))
    ggsave(paste0('figures/metric_trends/cors_with_preds/',metric_sel,'/',p,'.png'), height = 4, width = 4.5, units = 'in')
    
  }
}








  
  

