all_gage_info <- read_csv('data/gages/all_gage_info_exp.csv')
connections <- read_csv('data/gages/hw_ds_connections_exp.csv')

q <- read_csv(list.files('data/gages/q_exp/', full.names = T))

q_summary <- q %>%
  group_by(site_no) %>%
  summarise(start = date[1],
            end = date[length(date)],
            n = length(!is.na(q_norm)))

gage_info_windows <- left_join(all_gage_info, q_summary)
cn_windows <- left_join(connections, q_summary, by = join_by(headwater_id == site_no)) %>%
  left_join(., q_summary, by = join_by(downstream_id == site_no), suffix = c('_hw', '_ds')) %>%
  mutate(n_rat = min(n_hw/n_ds, n_ds/n_hw), .by = connection_id)

cn_sel <- 321
hw_gage <- connections$headwater_id[connections$connection_id == cn_sel]
ds_gage <- connections$downstream_id[connections$connection_id == cn_sel]
q_fil <- filter(q, site_no %in% c(hw_gage, ds_gage)) %>%
  mutate(gage = ifelse(site_no == hw_gage, 'hw', 'ds'))

ggplot(q_fil, aes(x = date, y = gage)) +
  geom_point() +
  ggtitle(paste('Connection',cn_sel)) +
  scale_x_date(limits = ymd(c('1981-10-01', '2023-09-30')))

summary(cn_windows$n_rat)

write_csv(cn_windows, 'data/gages/hw_ds_connections_exp.csv')

write_csv(gage_info_windows, 'data/gages/all_gage_info_exp.csv')


filter(gage_info_windows, n >= 37*365)
