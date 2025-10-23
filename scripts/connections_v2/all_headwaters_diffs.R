# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/connections_v2/functions/get_gages_diff.R')
source('scripts/connections_v2/functions/get_huc8_matches.R')
source('scripts/Theme+Settings.R')

hw_gage_info <- read_gage_info()
hw12_gage_info <- filter(hw_gage_info, order %in% 1:2)
connections <- read_gage_info('connections')
connections12 <- filter(connections, hw_order %in% 1:2)

# load differences output -------------------------------------------------
hw_diffs <- read_csv('scripts/connections_v2/order12_hw_diffs_set1.csv')
hw_diffs_info <- read_csv('scripts/connections_v2/order12_hw_diffs_set1_info.csv')


# plot a point ------------------------------------------------------------
plot_gages_diff(hw_diffs, hw12_gage_info$site_no[126])

# get gages in the same huc8 ----------------------------------------------
huc8_matches <- get_huc8_matches(hw12_gage_info$site_no)

# filter differences to downstream gages in the same HUC8 -----------------
hw_diffs_fil <- hw_diffs %>%
  filter(target_order > 2 & drainage_ratio < 1) %>%
  group_by(goi_id) %>%
  mutate(ds_ranking = rank(diffmetric)) %>%
  left_join(., huc8_matches) %>%
  filter(!is.na(huc8)) %>%
  group_by(goi_id) %>%
  mutate(huc8_ranking = rank(diffmetric)) %>%
  left_join(., select(connections, goi_id = headwater_id, target_id = downstream_id, connection_id)) %>%
  mutate(nhd_connection = ifelse(is.na(connection_id), F, T))

cn_mean <- mean(hw_diffs_fil$diffmetric[hw_diffs_fil$nhd_connection == T])

ggplot(hw_diffs_fil, aes(x = nhd_connection, y = diffmetric)) +
  geom_jitter(width = 0.2) +
  geom_hline(yintercept = cn_mean, linetype = 'dashed')

ggplot(subset(hw_diffs_fil, nhd_connection == T), aes(x = huc8_ranking)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(1,15)) +
  xlab('Diff metric ranking') +
  ggtitle('Diff metric ranking for downstream gages WITH flowline connection')

ggplot(hw_diffs_fil, aes(x = drainage_ratio, y = diffmetric)) +
  geom_point()


# get new connections and plot ------------------------------------------------
library(sf)
source('scripts/functions/load_states.R')
gages2_loc <- read_csv('data/gagesii/spreadsheets-in-csv-format/conterm_basinid.txt') %>%
  select(site_no = STAID, lon = LNG_GAGE, lat = LAT_GAGE) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>% st_transform(5070)

new_connections <- hw_diffs_fil %>%
  filter(diffmetric <= cn_median | nhd_connection == T)

old_connections_hw <- filter(connections12, ds_order > 2 & drainage_ratio < 1) %>%
  select(site_no = headwater_id) %>%
  left_join(gages2_loc)
new_connections_hw <- data.frame(site_no = unique(new_connections$goi_id)) %>%
  left_join(gages2_loc)

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = old_connections_hw, aes(geometry = geometry))
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = new_connections_hw, aes(geometry = geometry))
