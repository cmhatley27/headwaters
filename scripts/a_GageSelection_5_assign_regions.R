library(tidyverse)
source('scripts/functions/utilities.R')

hw_gage_info <- read_csv('data/gages/hw_gage_info.csv') %>%
  mutate(type = 'headwater')
ds_c_gage_info <- read_csv('data/gages/ds_gage_info.csv') %>%
  mutate(type = 'downstream_connected')
ds_m_gage_info <- read_csv('data/gages/ds_matched_gage_info.csv') %>%
  mutate(type = 'downstream_matched')
ds_gage_info <- rbind(ds_c_gage_info, ds_m_gage_info)

all_gage_info <- rbind(hw_gage_info, ds_gage_info)

region_codes <- c('nf' = 5.2,
                  'nf' = 5.3,
                  'mw' = 6.2,
                  'mw' = 7.1,
                  'ef' = 8.1,
                  'cp' = 8.2,
                  'ef' = 8.3,
                  'ap' = 8.4,
                  'ef' = 8.5,
                  'gp' = 9.2,
                  'gp' = 9.3,
                  'gp' = 9.4,
                  'gp' = 9.5,
                  'sw' = 10.1,
                  'sw' = 10.2,
                  'sw' = 11.1,
                  'sw' = 12.1,
                  'sw' = 13.1)

all_gage_info <- mutate(all_gage_info,
                        region = names(region_codes)[match(region2, region_codes)])
write_csv(all_gage_info, 'data/gages/all_gage_info.csv')

# ggplot(all_gage_info, aes(x = lon, y = lat, color = region)) +
#   geom_point()
