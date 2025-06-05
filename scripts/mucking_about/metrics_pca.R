library(tidyverse)
library(FactoMineR)
source('./scripts/functions/read_gages.R')
source('./scripts/functions/read_spatial.R')

hw_gage_info <- read_gage_info()
all_metrics <- read_csv('./data/metrics/all_metrics.csv')
all_trends <- read_csv('./data/metrics/all_trends.csv')

dat <- select(all_metrics,where(is.numeric)) %>%
  select(!wateryear)

metrics_pca = PCA(dat)
metrics_pca$eig
metrics_loads = as.data.frame(metrics_pca$var$contrib)
metrics_cors = as.data.frame(metrics_pca$var$cor)

a<-0.05
metrics_pc <- select(all_metrics, site_no, wateryear) %>%
  cbind(., metrics_pca$ind$coord) %>%
  rename(pc1 = 3, pc2 = 4, pc3 = 5, pc4 = 6, pc5 = 7) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'pc', values_to = 'val') %>%
  select(!wateryear) %>%
  group_by(site_no, pc) %>%
  summarise(tau = mk.test(val)$estimates['tau'],
            tau_p = mk.test(val)$p.value) %>%
  mutate(tau_trend = ifelse(tau > 0 & tau_p <= a, 'pos',
                            ifelse(tau < 0 & tau_p <= a, 'neg',
                                   'none'))) %>%
  ungroup()
hw_metrics_pc <- filter(metrics_pc, site_no %in% hw_gage_info$site_no)
table(select(hw_metrics_pc, pc, tau_trend))

plot_dat <- hw_metrics_pc %>%
  left_join(hw_gage_info) %>%
  st_as_sf(coords = c('lon','lat'), crs = 4269) %>%
  mutate(tau_trend = factor(tau_trend, levels = c('none','neg','pos'))) %>%
  arrange(tau_trend)

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = plot_dat, aes(color = tau_trend)) +
  scale_color_manual(limits = c('none','neg','pos'),
                     values = c('black','red','blue'), 
                     labels = c('Non-significant','Negative','Positive')) +
  # scale_size_manual(values = c(0.3,0.5,0.5), 
  #                   labels = c('Non-significant','Negative','Positive'),
  #                   name = NULL, guide = NULL) +
  facet_wrap(vars(pc)) +
  theme(legend.position = 'bottom',
        axis.text = element_blank(),
        axis.ticks = element_blank())
  







# pca on trends -----------------------------------------------------------
trends_wide <- select(all_trends, site_no, var, tau) %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = tau) %>%
  select(!site_no)

trends_pca = PCA(trends_wide)
trends_pca$eig
loads = as.data.frame(trends_pca$var$contrib)
cors = as.data.frame(trends_pca$var$cor)
