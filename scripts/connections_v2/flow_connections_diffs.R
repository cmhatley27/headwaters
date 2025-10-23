# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/connections_v2/functions/get_gages_diff.R')
source('scripts/connections_v2/functions/get_huc8_matches.R')
source('scripts/Theme+Settings.R')

hw_gage_info <- read_gage_info()
connections <- read_gage_info('connections')
gagesii_dir <- 'data/gagesii/spreadsheets-in-csv-format'

# load differences output -------------------------------------------------
connection_hw_diffs <- read_csv('scripts/connections_v2/nhd_cns_diffs_set1.csv')
connection_hw_diffs_info <- read_csv('scripts/connections_v2/nhd_cns_diffs_set1_info.csv')

# or run a new one --------------------------------------------------------
goi <- unique(connections$headwater_id)
gagesii_dir <- 'data/gagesii/spreadsheets-in-csv-format'
gagesii_cols <- c('ELEV_MEAN_M_BASIN', 'PPTAVG_BASIN', 'T_AVG_BASIN', 'WD_BASIN', 'PRECIP_SEAS_IND')
weights = c(1,1,1,1,1,1)
connection_hw_diffs <- get_gages_diff(goi = goi, 
                                      gagesii_dir = gagesii_dir,
                                      gagesii_cols = gagesii_cols,
                                      weights = weights)

# plot a point ------------------------------------------------------------
plot_gages_diff(connection_hw_diffs, connections$headwater_id[1], gagesii_dir = gagesii_dir)


# get difference ranks of connections -------------------------------------
diffs_rerank <- connection_hw_diffs %>%
  filter(drainage_ratio < 1) %>%
  group_by(goi_id) %>%
  mutate(rerank = rank(diffmetric))

connection_diffs <- left_join(connections,
                              select(diffs_rerank, 
                                     headwater_id = goi_id, downstream_id = target_id, 
                                     diffmetric, rerank)) %>%
                    filter(!duplicated(.))

ggplot(connection_diffs, aes(x = rerank)) + 
  geom_histogram(binwidth = 5) +
  xlim(0,100)

ggplot(connection_diffs, aes(x = drainage_ratio, y = rerank)) +
  geom_point() +
  ylim(0,100)


# which connections are in same huc8? -------------------------------------
huc8_matches <- get_huc8_matches(unique(connections$headwater_id))
connection_huc8s <- left_join(connections,
                              select(huc8_matches, 
                                     headwater_id = goi_id, downstream_id = target_id, 
                                     huc8))
sum(!is.na(connection_huc8s$huc8))
sum(!is.na(connection_huc8s$huc8))/nrow(connection_huc8s)
