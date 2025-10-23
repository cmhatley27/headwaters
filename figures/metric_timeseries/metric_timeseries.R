source('scripts/functions/utilities.R')
library(tidyverse)

metrics <- read_csv('data/gages/metrics/merged/metrics_window3.csv')
trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv')

site_sel <- '01054200'
metric_sel <- 'Q_frequency_high_3'

q <- read_csv(paste0('data/gages/q/',site_sel,'.csv'))

plot_dat <- filter(metrics, site_no == site_sel) %>%
  select(wateryear, obs = all_of(metric_sel))

ggplot(plot_dat, aes(x = wateryear, y = obs)) +
  geom_point() +
  ggtitle(paste0(metric_sel,' @ ',site_sel))

ggplot(subset(q, wateryear %in% 1982), aes(x = date, y = q_norm)) +
  geom_line() +
  geom_hline(yintercept = c(0.75, 0.67)*median(q$q_norm[q$wateryear %in% 1982]))

