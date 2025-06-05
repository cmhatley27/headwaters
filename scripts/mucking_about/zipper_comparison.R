files <- list.files('./data/gages/q/', full.names = T)
gage_q <- map(files, ~read_csv(.x, col_types = cols(site_no = col_character()))) %>%
  list_rbind()

hw_gage_info_fil <- gage_q %>%
  group_by(site_no, wateryear) %>%
  summarise(n_noflow = length(q[q == 0])) %>%
  group_by(site_no) %>%
  summarise(mean_noflow = mean(n_noflow, na.rm = T)) %>%
  filter(mean_noflow >= 5 & mean_noflow <= 360)

trends1 <- read_csv('./data/gages/metrics/trends_window1.csv') %>%
  filter(site_no %in% hw_gage_info_fil$site_no, var == 'Q_totalduration_noflow')
trends3 <- read_csv('./data/gages/metrics/trends_window3.csv') %>%
  filter(site_no %in% hw_gage_info_fil$site_no, var == 'Q_totalduration_noflow')

table(trends1$tau_trend)
table(trends3$tau_trend)
