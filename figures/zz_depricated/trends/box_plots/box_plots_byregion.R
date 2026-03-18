# load data ---------------------------------------------------------------
library(tidyverse)
library(reshape2)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
# metrics_sel <- c('Q_mean', 'Q5', 'Q95',
#                  'Q_totalduration_low_4', 'Q_frequency_high_2',
#                  'HFD_mean', 'HFI_mean',
#                  'FlashinessIndex', 'BaseflowRecessionK')

trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  left_join(., select(all_gage_info, site_no, type, order, region2, drainage_area)) %>%
  filter(order %in% 1:6,
         var %in% metrics_sel) %>%
  mutate(var = factor(var, levels = metrics_sel),
         region = region_recoder(region2),
         type2 = case_when(
           order <= 3 ~ 'headwater',
           order > 3 ~ 'downstream'
         ),
         type3 = case_when(
           order %in% c(1,2) ~ 'headwater',
           order == 3 ~ 'order3',
           order >= 4 ~ 'downstream'
         )) %>%
  mutate(type2 = factor(type2, levels = c('headwater', 'downstream')),
         type3 = factor(type3, levels = c('headwater', 'order3', 'downstream')),
         order = factor(order, levels = 1:6))

# lumped hw (1-3) vs ds (4-6) boxes ---------------------------------------
#main plot
ggplot(trends, aes(x = region, y = abs(sen), fill = region, color = type2)) +
  # geom_hline(yintercept = 0, lty = 'dashed') +
  geom_boxplot(outliers = F) +
  scale_fill_discrete(guide = 'none') +
  scale_color_manual(values = c('black', 'grey1'), guide = 'none') +
  ylab('|Sen\'s Slope|') +
  xlab('Region') +
  scale_x_discrete(labels = c('App Mtns', 'Ctl Plains', 'E Forests', 'Grt Plains',
                              'Mtn West', 'N Forests', 'Desert SW')) +
  facet_wrap(vars(var), scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('figures/trends/box_plots/abs_sen_byregion.png', width = 9.6, height = 6, units = 'in')

#box sigs
trend_p <- trends %>%
  group_by(var, region) %>%
  summarise(wc_p = wilcox.test(abs(sen[type2 == 'headwater']), abs(sen[type2 == 'downstream']))$p.value)

#box n counts
gage_counts <- select(trends, site_no, order, region, region2, type2, type3)
ggplot(gage_counts, aes(x = region, fill = region, color = type2)) +
  geom_bar(position = 'dodge') +
  scale_fill_discrete(guide = 'none') +
  scale_color_manual(values = c('black', 'grey1'), guide= 'none') +
  xlab(NULL) +
  ylab('# Gages')
ggsave('figures/trends/box_plots/gage_count_byregion.png', width = 4, height = 2, units = 'in')



# separate order boxes ----------------------------------------------------
#main plot
ggplot(trends, aes(x = region, y = abs(sen), fill = region, color = type3)) +
  # geom_hline(yintercept = 0, lty = 'dashed') +
  geom_boxplot(outliers = F) +
  scale_fill_discrete(guide = 'none') +
  scale_color_manual(values = rep('black',6), guide = 'none') +
  ylab('|Sen\'s Slope|') +
  xlab('Region') +
  scale_x_discrete(labels = c('App Mtns', 'Ctl Plains', 'E Forests', 'Grt Plains',
                              'Mtn West', 'N Forests', 'Desert SW')) +
  facet_wrap(vars(var), scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('figures/trends/box_plots/abs_sen_byregion.png', width = 9.6, height = 6, units = 'in')

#box sigs
wc_p <- tibble()
for(v in seq_along(unique(trends$var))){
  for(r in seq_along(unique(trends$region))){
    var_sel = unique(trends$var)[v]
    reg_sel = unique(trends$region)[r]
    dat_fil <- filter(trends, var == var_sel, region == reg_sel)
    
    wc_p_i <- melt(pairwise.wilcox.test(abs(dat_fil$sen), dat_fil$type3, p.adjust.method = 'BH')$p.value) %>%
      mutate(var = var_sel,
             region = reg_sel) %>%
      select(var, region, order1 = Var1, order2 = Var2, p = value) %>%
      filter(!is.na(p))
    wc_p <- rbind(wc_p, wc_p_i)
  }
}

#box n counts
gage_counts <- select(trends, site_no, order, region, region2, type2, type3) %>%
  filter(!duplicated(.))
ggplot(gage_counts, aes(x = region, fill = region, color = type3)) +
  geom_bar(position = 'dodge') +
  scale_fill_discrete(guide = 'none') +
  scale_color_manual(values = rep('black',6), guide= 'none') +
  xlab(NULL) +
  ylab('# Gages')
ggsave('figures/trends/box_plots/gage_count_byregion.png', width = 4, height = 2, units = 'in')

# preds -------------------------------------------------------------------
preds <- read_csv('data/gages/predictors/pred_trends.csv') %>%
  left_join(., select(all_gage_info, site_no, type, order, region2, drainage_area)) %>%
  filter(order <= 6) %>%
  mutate(region = region_recoder(region2),
         type2 = ifelse(str_detect(type, 'downstream'), 'downstream', 'headwater')) %>%
  mutate(type2 = factor(type2, levels = c('headwater', 'downstream'))) %>%
  group_by(var) %>%
  mutate(sen_z = (sen-mean(sen, na.rm = T))/sqrt(var(sen, na.rm = T)),
         var = factor(var, levels = unique(var), labels = pred_labeller(unique(var))))

ggplot(preds, aes(x = region, y = abs(sen), fill = region, color = type2)) +
  # geom_hline(yintercept = 0, lty = 'dashed') +
  geom_boxplot(outliers = F) +
  scale_fill_discrete(guide = 'none') +
  scale_color_manual(values = c('black', 'grey1'), guide = 'none') +
  ylab('|Sen\'s Slope|') +
  xlab('Region') +
  scale_x_discrete(labels = c('App Mtns', 'Ctl Plains', 'E Forests', 'Grt Plains',
                              'Mtn West', 'N Forests', 'Desert SW')) +
  facet_wrap(vars(var), scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pred_p <- preds %>%
  group_by(var, region) %>%
  summarise(wc_p = wilcox.test(abs(sen[type2 == 'headwater']), abs(sen[type2 == 'downstream']))$p.value)

pred_sel <- filter(pred_p, region == 'cp') %>%
  arrange(wc_p)
ggplot(subset(preds, var %in% pred_sel$var[1:8] & region == 'cp' & abs(sen_z) <= 3), aes(x = type2, y = abs(sen))) +
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
ggsave('figures/trends/box_plots/preds_centralplains.png', width = 7, height = 5, units = 'in')
