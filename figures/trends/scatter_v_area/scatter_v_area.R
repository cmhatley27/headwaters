library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

# metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 'Q_frequency_high_2', 'Q_frequency_noflow',
#                  'Q_totalduration_high_2', 'Q_totalduration_noflow',
#                  'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
#                  'BaseflowRecessionK', 'Recession_a_Seasonality')
metrics_sel <- c('Q_mean', 'Q5', 'Q95',
                 'Q_totalduration_low_4', 'Q_frequency_high_2',
                 'HFD_mean', 'HFI_mean',
                 'FlashinessIndex', 'BaseflowRecessionK')

trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  left_join(., select(all_gage_info, site_no, type, order, region2, drainage_area)) %>%
  filter(order <= 6,
         var %in% metrics_sel) %>%
  mutate(var = factor(var, levels = metrics_sel),
         region = region_recoder(region2),
         type2 = ifelse(str_detect(type, 'downstream'), 'downstream', 'headwater')) %>%
  mutate(type2 = factor(type2, levels = c('headwater', 'downstream')))

trends_trim <- trends %>%
  group_by(var) %>%
  filter(abs(sen) <= mean(sen, na.rm = T)+4*sqrt(var(sen, na.rm = T)))
trends_lm <- trends_trim %>%
  group_by(var) %>%
  summarise(lm_r2 = summary(lm(abs(sen)~log10(drainage_area)))$r.squared,
            lm_p = summary(lm(abs(sen)~log10(drainage_area)))$coefficients[2,4],
            lm_int = summary(lm(abs(sen)~log10(drainage_area)))$coefficients[1,1],
            max_sen = max(abs(sen)),
            cor = cor(abs(sen), log10(drainage_area), use = 'pairwise.complete', method = 'spearman'))

sct_plt <- ggplot(trends_trim, aes(x = log10(drainage_area), y = abs(sen))) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  geom_text(data = trends_lm, aes(x = 3.5, y =max_sen*0.95, label = paste0('R2 = ',round(lm_r2,4))), color = 'blue') +
  facet_wrap(vars(var), scales = 'free') +
  ylab('|Sen\'s Slope|') +
  xlab('Log10 Drainage Area')
sct_plt
ggsave('figures/trends/scatter_v_area/abs_trend_v_area.png', sct_plt, width = 8, height = 5, units = 'in')

trends_rgn_lm <- trends_trim %>%
  group_by(var, region) %>%
  summarise(lm_r2 = summary(lm(abs(sen)~log10(drainage_area)))$r.squared,
            lm_p = summary(lm(abs(sen)~log10(drainage_area)))$coefficients[2,4],
            lm_int = summary(lm(abs(sen)~log10(drainage_area)))$coefficients[1,1],
            max_sen = max(abs(sen)))
sct_plt_rgn <- ggplot(trends_trim, aes(x = log10(drainage_area), y = abs(sen), color = region)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  # geom_text(data = trends_lm, aes(x = 3.7, y =max_sen*0.95, label = paste0('R2 = ',round(lm_r2,4))), color = 'blue') +
  facet_wrap(vars(var), scales = 'free') +
  ylab('|Sen\'s Slope|') +
  xlab('Log10 Drainage Area') +
  scale_color_discrete(guide = 'none')
sct_plt_rgn
ggsave('figures/trends/scatter_v_area/abs_trend_v_area_byregion.png', sct_plt_rgn, width = 8, height = 5, units = 'in')
