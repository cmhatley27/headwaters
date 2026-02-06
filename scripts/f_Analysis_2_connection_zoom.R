# libraries and data ------------------------------------------------------
library(tidyverse)
library(plotly)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

metric_trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  mutate(type = 'metric')
pred_trends <- read_csv('data/gages/predictors/pred_trends.csv') %>%
  mutate(type = 'pred')
pred_statics <- read_csv('data/gages/predictors/pred_statics.csv')

metrics <- read_csv('data/gages/metrics/merged/metrics_window3.csv', col_select = !contains('error_str')) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val')
preds <- read_csv('data/gages/predictors/pred_timeseries.csv') %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val')
ts_dat <- rbind(metrics, preds)

cn_diffs <- read_csv('data/gages/cn_diffs.csv')

# identify differences --------------------------------------------------------------------
cn_sel <- 167
gages_sel <- unique(unlist(filter(connections, connection_id %in% cn_sel) %>% select(headwater_id, downstream_id)))


metric_diffs <- filter(cn_diffs, var %in% c(metric_trends$var), connection_id %in% cn_sel)
pred_diffs <- filter(cn_diffs, var %in% c(pred_trends$var, pred_statics$var), connection_id %in% cn_sel)

# ggplot(filter(pred_statics, var == 'drainage_area'), aes(x = val)) +
#   geom_histogram()
# asdf <- ecdf(pred_statics$val[pred_statics$var == 'drainage_area'])
# asdf(pred_diffs$hw_val[pred_diffs$var == 'drainage_area'])
# 
# qwer <- ecdf(connections$drainage_ratio)
# qwer(0.2539135)

# time series plots of relevant series ------------------------------------
ts_dat_sel <- filter(ts_dat,
                     site_no %in% gages_sel,
                     var %in% c('Q_mean', 'FlashinessIndex', 'developed')) %>%
  group_by(site_no, var) %>%
  mutate(val = scale(val, scale = F, center = F))

ggplot(ts_dat_sel, aes(x = wateryear, y = val, color = site_no)) +
  geom_line() +
  facet_wrap(vars(var), scales = 'free_y')
  

# get hydrographs of selected gages ---------------------------------------
q_files <- paste0('data/gages/q/',gages_sel,'.csv')
q_dat <- read_csv(q_files) %>%
  left_join(select(all_gage_info, site_no, type)) %>%
  mutate(month = month(date), day = day(date)) %>%
  mutate(wateryear = ifelse(month(date) >= 10, year(date)+1, year(date)))

q95_timing <- q_dat %>%
  group_by(site_no, wateryear) %>%
  arrange(q_norm) %>%
  mutate(q_rnk = 1:n(),
         q_pct = q_rnk/max(q_rnk)) %>%
  filter(q_pct >= 0.95)
ggplot(q95_timing, aes(x = site_no, y = month(date))) +
  geom_violin() 
  

plot_ly(data = filter(q_dat),
        x = ~date,
        y = ~q_norm,
        color = ~site_no,
        mode = 'lines') %>%
  layout(dragmode = 'pan',
         yaxis = list(fixedrange = F)) %>%
  config(displayModeBar = T,
         scrollZoom = T,
         doubleClick = T)

q_year <- q_dat %>%
  group_by(site_no, month = month(date), day = day(date)) %>%
  summarise(q_norm = mean(q_norm)) %>%
  mutate(date = ymd(paste('2020',month, day)))
ggplot(q_year, aes(x = date, y = q_norm, color = site_no)) +
  geom_line()

ggplot(q_dat, aes(x = ymd(paste('2020',month,day)), y = q_norm, color = site_no)) +
  geom_line() +
  facet_wrap(vars(factor(wateryear)))
