library(tidyverse)
source('./scripts/functions/read_gages.R')

tossh_files <- list.files('./data/metrics/tossh/', full.names = T)
r_files <- list.files('./data/metrics/r/', full.names = T)

tossh_dat <- map(tossh_files, ~read_csv(.x, col_types = cols(site_no = col_character()))) %>%
  list_rbind()
r_dat <- map(r_files, ~read_csv(.x, col_types = cols(site_no = col_character()))) %>%
  list_rbind()

merge <- left_join(tossh_dat, r_dat)

write_csv(merge, './data/metrics/all_metrics.csv')