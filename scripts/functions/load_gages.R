library(tidyverse)
source('scripts/functions/utilities.R')

# Read in all the different gage info files, created in the a_GageSelection scripts
hw_gage_info <- read_gage_info() %>%
  mutate(type = 'headwater')
ds_c_gage_info <- read_gage_info('downstream') %>%
  mutate(type = 'downstream_connected')
ds_m_gage_info <- read_gage_info('downstream_matched') %>%
  mutate(type = 'downstream_matched')
ds_gage_info <- rbind(ds_c_gage_info, ds_m_gage_info)
all_gage_info <- read_gage_info('all')

connections <- read_gage_info('connections')
matches <- read_gage_info('matches')