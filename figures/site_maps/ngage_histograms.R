library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

#headwaters
plot_dat <- filter(all_gage_info, type %in% c('headwater'))
ggplot(plot_dat, aes(x = factor(order))) +
  geom_bar() +
  scale_x_discrete(name = 'Order') +
  ggtitle(paste0('Headwaters, n = ',nrow(plot_dat)))

#downstream connections
plot_dat <- filter(all_gage_info, type %in% c('downstream_connected'))
ggplot(plot_dat, aes(x = factor(order))) +
  geom_bar() +
  scale_x_discrete(name = 'Order') +
  ggtitle(paste0('Downstream connections, n = ',nrow(plot_dat)))

#downstream matches
plot_dat <- filter(all_gage_info, type %in% c('downstream_matched'))
ggplot(plot_dat, aes(x = factor(order))) +
  geom_bar() +
  scale_x_discrete(name = 'Order') +
  ggtitle(paste0('Downstream matches, n = ',nrow(plot_dat)))

#all downstreams
plot_dat <- filter(all_gage_info, type %in% c('downstream_connected', 'downstream_matched'))
ggplot(plot_dat, aes(x = factor(order))) +
  geom_bar() +
  scale_x_discrete(name = 'Order') +
  ggtitle(paste0('All downstreams, n = ',nrow(plot_dat)))

#headwater + connections
plot_dat <- filter(all_gage_info, type %in% c('headwater', 'downstream_connected'))
ggplot(plot_dat, aes(x = factor(order))) +
  geom_bar() +
  scale_x_discrete(name = 'Order') +
  ggtitle(paste0('No matches, n = ',nrow(plot_dat)))

#all gages
plot_dat <- all_gage_info
ggplot(plot_dat, aes(x = factor(order), fill = type)) +
  geom_bar() +
  scale_x_discrete(name = 'Order') +
  ggtitle(paste0('All gages, n = ',nrow(plot_dat)))

ggplot(plot_dat, aes(x = factor(order), fill = type)) +
  geom_bar() +
  scale_x_discrete(name = 'Order') +
  ggtitle(paste0('All gages, n = ',nrow(plot_dat))) +
  facet_wrap(vars(region_recoder(region2)))
