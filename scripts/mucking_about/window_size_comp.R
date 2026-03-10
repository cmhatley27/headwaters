library(tidyverse)

metric_sel <- 'Q95'

# metric values -----------------------------------------------------------
metrics1 <- read_csv('./data/gages/metrics/merged/metrics_window1.csv') %>%
  mutate(window = 'w1')
metrics3 <- read_csv('./data/gages/metrics/merged/metrics_window3.csv') %>%
  mutate(window = 'w3')
metrics <- rbind(metrics1, metrics3) %>%
  pivot_wider(id_cols = c(site_no, wateryear), names_from = window, values_from = all_of(metric_sel))

ggplot(data = metrics, aes(x = w1, y = w3)) +
  geom_point()


# trends ------------------------------------------------------------------
trends1 <- read_csv('./data/gages/metrics/trends/metrics_trends_window1.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = sen) %>%
  mutate(window = 'w1') 
trends3 <- read_csv('./data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = sen) %>%
  mutate(window = 'w3')
trends <- rbind(trends1, trends3) %>%
  pivot_wider(id_cols = c(site_no), names_from = window, values_from = all_of(metric_sel))

ggplot(trends, aes(x = w1, y = w3)) +
  geom_point()




