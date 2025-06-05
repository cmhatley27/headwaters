metrics1 <- read_csv('./data/gages/metrics/metrics_window1.csv') %>%
  mutate(window = 1) %>%
  select(!qp_elasticity)
metrics3 <- read_csv('./data/gages/metrics/metrics_window3.csv') %>%
  mutate(wateryear = wateryear+1,
         window = 3)
metrics5 <- read_csv('./data/gages/metrics/metrics_window5.csv') %>%
  mutate(wateryear = wateryear+3,
         window = 5)
metrics10 <- read_csv('./data/gages/metrics/metrics_window10.csv') %>%
  mutate(wateryear = wateryear+5,
         window = 10)
metrics20 <- read_csv('./data/gages/metrics/metrics_window20.csv') %>%
  mutate(wateryear = wateryear+10,
         window = 20)
all_metrics <- rbind(metrics1,metrics3,metrics5,metrics10,metrics20)

ggplot(data = subset(all_metrics, site_no == '01094400'), aes(x = wateryear, y = BFI, color = factor(window))) +
  geom_point() +
  facet_wrap(vars(window))


trends1 <- read_csv('./data/gages/metrics/trends_window1.csv') %>%
  mutate(window = 1)
trends3 <- read_csv('./data/gages/metrics/trends_window3.csv') %>%
  mutate(window = 3)
trends5 <- read_csv('./data/gages/metrics/trends_window5.csv')%>%
  mutate(window = 5)
trends10 <- read_csv('./data/gages/metrics/trends_window10.csv')%>%
  mutate(window = 10)
trends20 <- read_csv('./data/gages/metrics/trends_window20.csv')%>%
  mutate(window = 20)
all_trends <- rbind(trends1, trends3, trends5, trends10, trends20)

window_sigs <- all_trends %>%
  group_by(window) %>%
  summarise(sig = length(tau_trend[tau_trend != 'none'])/length(tau_trend))

ggplot(window_sigs, aes(x = window, y = sig)) +
  geom_point()



