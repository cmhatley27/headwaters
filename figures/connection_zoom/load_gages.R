library(tidyverse)
hw_gage_info <- read_csv('data/hw_gage_info.csv') %>%
  mutate(type = 'headwater')
ds_c_gage_info <- read_csv('data/ds_gage_info.csv') %>%
  mutate(type = 'downstream_connected')
ds_m_gage_info <- read_csv('data/ds_matched_gage_info.csv') %>%
  mutate(type = 'downstream_matched')
ds_gage_info <- rbind(ds_c_gage_info, ds_m_gage_info)
all_gage_info <- rbind(hw_gage_info, ds_gage_info)

connections <- read_csv('data/hw_ds_connections.csv')
matches <- read_csv('data/hw_ds_matches.csv')