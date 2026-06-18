library(tidyverse)

metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')

model_names <- paste0(tolower(metrics_sel),'_annual_w3_emp')
model_dirs <- paste0('data/models/',model_names,'/')


shaps <- read_csv(paste0(model_dirs, 'shaps.csv')) %>%
  pivot_wider(id_cols = c(metric, site_no, wateryear), names_from = 'var', values_from = c('val', 'shap')) %>%
  group_by(metric, site_no) %>%
  summarise(area = mean(val_drainage_area),
            shap = mean(shap_drainage_area))


trends <- read_csv(paste0(model_dirs, 'trends.csv')) %>%
  filter(type == 'shap',
         var == 'drainage_area') %>%
  group_by(metric) %>%
  filter(abs(sen) <= quantile(abs(sen), 0.99, na.rm = T)) %>%
  ungroup(.) %>%
  select(metric, site_no, sen)

cn_trends <- left_join(connections, select(trends, metric, headwater_id = site_no, up = sen)) %>%
  left_join(select(trends, metric, downstream_id = site_no, down = sen)) %>%
  select(metric, up, down) %>%
  pivot_longer(!metric, names_to = 'site', values_to = 'sen') %>%
  mutate(metric_name = factor(labelinator(metric), levels = labelinator(metrics_sel)))

ggplot(cn_trends, aes(x = site, y = sen, fill = site)) +
  geom_hline(yintercept = 0, color = 'grey70') +
  geom_boxplot(outlier.size = 0.8) +
  scale_x_discrete(limits = c('up', 'down'), labels = c('Upstream', 'Downstream')) +
  scale_fill_manual(limits = c('up', 'down'), labels = c('Upstream', 'Downstream'),
                    values = c('#d73027','#4575b4'), guide = NULL) +
  xlab(NULL) +
  ylab('Drainage Area SHAP Trend') +
  facet_wrap(vars(metric_name), scales = 'free_y')
ggsave('figures/drainage_area_stuff/up_down_boxes.png',
       height = 70, width = 100, units = 'mm')


wilcox.test(cn_trends$sen[cn_trends$site == 'up' & cn_trends$metric == 'FlashinessIndex'],
            cn_trends$sen[cn_trends$site == 'down'& cn_trends$metric == 'FlashinessIndex'])



trend_area_comp <- left_join(trends, shaps) %>%
  filter(site_no %in% c(connections$headwater_id, connections$downstream_id))

ggplot(trend_area_comp, aes(x = log(area), y = sen)) +
  geom_point() +
  geom_smooth(method= 'lm') +
  facet_wrap(vars(metric), scales = 'free_y')

summary(lm(sen~log(area), data = filter(trend_area_comp, metric == 'Q95')))
