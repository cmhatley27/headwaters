library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')
library(reshape)

metrics <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv')
preds <- read_csv('data/gages/predictors/pred_trends.csv') %>%
  mutate(set = ifelse(site_no %in% hw_gage_info$site_no, 'hw', 'ds')) %>%
  

trends <- rbind(metrics, preds) %>%
  left_join(., select(all_gage_info, site_no, region2)) %>%
  mutate(region = region_recoder(region2),
         set = ifelse(site_no %in% hw_gage_info$site_no, 'hw', 'ds'))

reg_sel <- 'cp'
metric_sel <- 'Q95'
pred_sel <- unique(preds$var)

cor_dat <- filter(trends, region == reg_sel, var %in% c(metric_sel, pred_sel)) %>%
  select(site_no, set, var, sen)

cor_dat_wide <- pivot_wider(cor_dat, id_cols = site_no, names_from = var, values_from = sen) %>%
  mutate(set = ifelse(site_no %in% hw_gage_info$site_no, 'hw', 'ds'))

cors <- cor_dat_wide %>%
  select(!c(site_no, set)) %>%
  cor(., use = 'pairwise.complete', method = 'spearman') %>%
  melt(.) %>%
  filter(X1 == metric_sel & X2 != metric_sel)

cors_abs <- mutate(cors, value = abs(value)) %>%
  arrange(desc(value)) %>%
  mutate(pred = factor(X2)) %>%
  mutate(pred = fct_reorder(pred, value, .desc = T))

ggplot(subset(cors_abs, value >= 0.1), aes(x = pred, y = value)) +
  geom_col(color = 'black', fill = '#c69b00') +
  ylab('|Spearman Cor|') +
  xlab(NULL) +
  scale_x_discrete(labels = pred_labeller) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('figures/trends/box_plots/preds_Q95cor_centralplains.png', width = 7, height = 5, units = 'in')

pred_sel <- arrange(cors_abs, desc(value))
plot_dat <- trends %>%
  filter(var %in% cors_abs$pred[1:8], region == reg_sel) %>%
  mutate(var = factor(var, levels = cors_abs$pred[1:8], labels = pred_labeller(cors_abs$pred[1:8]))) %>%
  mutate(set = factor(set, levels = c('hw', 'ds'), labels = c('Headwater', 'Downstream')))
ggplot(plot_dat, aes(x = set, y = abs(sen))) +
  # geom_hline(yintercept = 0, lty = 'dashed') +
  geom_boxplot(outliers = T, fill = '#c69b00') +
  scale_fill_discrete(guide = 'none') +
  # scale_color_manual(values = c('black', 'grey1'), guide = 'none') +
  ylab('|Sen\'s Slope|') +
  xlab('Central Plains Predictors') +
  # scale_x_discrete(labels = c('App Mtns', 'Ctl Plains', 'E Forests', 'Grt Plains',
  #                             'Mtn West', 'N Forests', 'Desert SW')) +
  facet_wrap(vars(var), scales = 'free_y', nrow = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('figures/trends/box_plots/preds_diff_Q95cor_centralplains.png', width = 7, height = 5, units = 'in')

diff_p <- plot_dat %>%
  group_by(var) %>%
  summarise(wc_p = wilcox.test(abs(sen[set == 'Headwater']), abs(sen[set == 'Downstream']))$p.value)
