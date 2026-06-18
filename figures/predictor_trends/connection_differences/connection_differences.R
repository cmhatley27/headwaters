library(tidyverse)
source('scripts/functions/load_gages.R')
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')

preds_sel <- c('precip_annual','developed', 'precip_amj','ag', 'precip_ond','forest')

pred_trends <- read_csv('data/gages/predictors/pred_trends_window3.csv') %>%
  select(site_no, var, sen)

cn_trends <- left_join(connections, rename(pred_trends, headwater_id = site_no, up = sen)) %>%
  left_join(rename(pred_trends, downstream_id = site_no, down = sen)) %>%
  filter(var %in% preds_sel) %>%
  mutate(var_name = factor(labelinator(var, pred_labels), levels = labelinator(preds_sel, pred_labels)))

cors <- cn_trends %>%
  group_by(var_name) %>%
  summarise(cor = cor(up,down, use = 'pairwise.complete'))

ggplot(filter(cn_trends, var %in% preds_sel), aes(x = up, y = down)) +
  geom_abline(slope = 1) +
  geom_point() +
  geom_text(data = cors, aes(label = paste0('\u03c1: ',round(cor, 3))),
            x = -Inf, y = Inf, hjust = -0.2, vjust = 2.5) +
  xlab('Upstream Trend') +
  ylab('Downstream Trend') +
  facet_wrap(vars(var_name), scales = 'free', ncol = 2)
ggsave('figures/predictor_trends/connection_differences/precip_lc.png',
       height = 150, width = 125, units = 'mm')
