# Load libraries and data --------------------------------------------
library(tidyverse)
source(file.path('scripts','functions','read_gages.R'))
source(file.path('scripts','functions','read_spatial.R'))
source(file.path('scripts','functions','Theme+Settings.R'))

gage_info_1970 <- read_gage_info(folder = '1970_2022')
gage_info_1981 <- read_gage_info(folder = '1981_2022')

gages_added <- gage_info_1981 %>%
  mutate(in_1970 = site_no %in% gage_info_1970$site_no) %>%
  filter(!in_1970)

ggplot(data = gage_info_1970, aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point() +
  ggtitle('1970 Set')

ggplot(data = gage_info_1981, aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point() +
  xlab('') +
  ylab('')

ggplot(data = gage_info_1981, aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point() +
  geom_point(data = gages_added, color = 'blue') +
  ggtitle('Gages added by moving to 1981')

