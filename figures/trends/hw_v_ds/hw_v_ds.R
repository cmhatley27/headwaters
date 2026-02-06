# load data ---------------------------------------------------------------
library(tidyverse)
library(reshape)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  mutate(type = 'metric')
pred_trends <- read_csv('data/gages/predictors/pred_trends.csv') %>%
  mutate(type = 'pred')
pred_statics <- read_csv('data/gages/predictors/pred_statics.csv')

trends <- rbind(metric_trends, pred_trends) %>%
  select(site_no, var, sen) %>%
  mutate(set = ifelse(site_no %in% hw_gage_info$site_no, 'headwater', 'downstream'))

trends_wide <- trends %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = sen)

c_diffs <- tibble()
for(c in 1:nrow(connections)){
  c_sel <- filter(connections, connection_id == c)
  
  c_trends <- rbind(filter(trends_wide, site_no == c_sel$headwater_id),
                    filter(trends_wide, site_no == c_sel$downstream_id)) %>%
    select(!site_no) %>%
    mutate(across(everything(), abs))
  c_diffs_i <- (c_trends[1,] - c_trends[2,]) %>%
    mutate(connection_id = c,
           match_id = NA)
  c_diffs <- rbind(c_diffs, c_diffs_i)
}
c_diffs <- left_join(c_diffs, select(connections, connection_id, site_no = headwater_id))

m_diffs <- tibble()
for(m in 1:nrow(matches)){
  m_sel <- filter(matches, match_id == m)
  
  m_trends <- rbind(filter(trends_wide, site_no == m_sel$headwater_id),
                    filter(trends_wide, site_no == m_sel$downstream_id)) %>%
    select(!site_no) %>%
    mutate(across(everything(), abs))
  m_diffs_i <- (m_trends[1,] - m_trends[2,]) %>%
    mutate(connection_id = NA,
           match_id = m)
  m_diffs <- rbind(m_diffs, m_diffs_i)
}
m_diffs <- left_join(m_diffs, select(matches, match_id, site_no = headwater_id))

all_diffs <- rbind(c_diffs, m_diffs) %>%
  pivot_longer(!c(connection_id, match_id, site_no), names_to = 'metric', values_to = 'diff') %>%
  left_join(select(hw_gage_info, site_no, aggregion, region2)) %>%
  mutate(region = region_recoder(region2)) %>%
  group_by(metric) %>%
  mutate(diff_z = (diff - mean(diff, na.rm = T))/sqrt(var(diff, na.rm = T)),
         diff_z2 = scale(diff, center = F)[,1]) %>%
  ungroup(.)
       

# select metrics and plot -------------------------------------------------
metrics_sel <- c('Q_mean', 'Q5', 'Q95',
                 'Q_totalduration_low_4', 'Q_frequency_high_2',
                 'HFD_mean', 'HFI_mean', 'FlashinessIndex', 'BaseflowRecessionK')
metrics_sel <- unique(pred_trends$var)

plot_dat <- all_diffs %>%
  filter(metric %in% metrics_sel) %>%
  filter(!is.na(connection_id)) %>%
  mutate(metric = factor(metric, levels = metrics_sel))

#histogram of hw-ds differences
ggplot(subset(plot_dat, abs(diff_z) <= 3), aes(y = diff_z)) +
  geom_histogram(binwidth = 0.25) +
  geom_hline(yintercept = 0, color = 'red') +
  facet_wrap(vars(metric), scales = 'free_x') +
  xlab(NULL) +
  ylab('|HW trend| - |DS trend|')
ggsave('figures/trends/hw_v_ds/metric_trend_diffs_hist_conly.png', height = 6, width = 8, units = 'in')

#box plot of hw-ds differences
ggplot(subset(plot_dat, abs(diff_z) <= 3), aes(x = metric, y = diff_z)) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  geom_boxplot(outliers = T)
diff_p <- plot_dat %>%
  group_by(metric) %>%
  filter(abs(diff_z) <= 3) %>%
  summarise(wc_p = wilcox.test(diff_z)$p.value)

#box plot of hw-ds differences, by region
ggplot(subset(plot_dat, abs(diff_z2) <= 3), aes(x = region, y = diff_z2, fill = region)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outliers = T) +
  ylab('|HW trend| - |DS trend|') +
  # ylim(c(-2,2)) +
  xlab('Region') +
  scale_fill_discrete(guide = 'none') +
  scale_x_discrete(labels = c('App Mtns', 'Ctl Plains', 'E Forests', 'Grt Plains',
                              'Mtn West', 'N Forests', 'Desert SW')) +
  facet_wrap(vars(metric), scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
diff_r_p <- plot_dat %>%
  group_by(metric, region) %>%
  filter(abs(diff_z2) <= 30,
         !is.na(connection_id)) %>%
  summarise(wc_p = wilcox.test(diff)$p.value)
ggsave('figures/trends/hw_v_ds/metric_trend_diffs_boxes_byregion_conly.png', height = 6, width = 8, units = 'in')

#pick a specific region
metric_subsubset <- filter(diff_r_p, region == 'cp') %>%
  arrange(wc_p)
ggplot(subset(plot_dat, abs(diff_z) <= 3 & region == 'cp' & metric %in% metric_subsubset$metric[1:8]), aes(x = metric, y = diff_z)) +
  geom_hline(yintercept = 0) +
  geom_boxplot(fill = '#c69b00') +
  xlab('Predictor') +
  scale_x_discrete(labels = pred_labeller) +
  ylab('|HW trend| - |DS trend|') +
  # facet_wrap(vars(metric), scales = 'free_y', nrow = 2) +
  ylim(c(-2,2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('figures/trends/hw_v_ds/pred_diffs_centralplains.png', height = 3, width = 6, units = 'in')

asdf <- plot_dat %>%
  filter(region == 'cp' & metric == 'Q95' & diff_z >= 0.5)
cn_ids <- asdf$connection_id

preds_sel <- unique(pred_trends$var)

asdf <- all_diffs %>%
  filter(region == 'cp' & metric %in% c('Q95', preds_sel) & !is.na(connection_id)) %>%
  pivot_wider(id_cols = connection_id, names_from = metric, values_from = diff)
asdf_cor <- asdf %>%
  select(!connection_id) %>%
  cor(., use = 'pairwise.complete', method = 'spearman') %>%
  melt(.) %>%
  filter(X1 == 'Q95')

ggplot(asdf, aes(x = pet_annual, y = Q95)) +
  geom_point()

asdf_fil <- filter(asdf, Q95 > 0.5)

