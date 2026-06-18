library(tidyverse)
source('scripts/functions/load_gages.R')
#all headwaters
hw <- filter(all_gage_info, 
             order <= 3)
#all downstreams
ds <- filter(all_gage_info, 
             order > 3)
#pnw, washington only
wa <- filter(all_gage_info, 
             order <= 3,
             str_detect(station_nm, 'WA$'),
             !str_detect(site_no, '^14'))
#central plains
cp <- filter(all_gage_info,
             order <= 3,
             region2 == 8.2)
#new england
ne <- filter(all_gage_info,
             order <= 3,
             word(station_nm, -1) %in% c('Maine', 'MA', 'VT', 'NH', 'CT', 'RI'))
#southern rockies
sr <- filter(all_gage_info,
             order <= 3,
             region3 == 21)
#mid-atlantic
ma <- filter(all_gage_info,
             order <= 3,
             word(station_nm, -1) %in% c('VA', 'WV', 'MD', 'DC', 'DE', 'PA', 'NJ', 'NY', 'CT', 'RI'))

site_sets <- list(
  'hw' = hw$site_no,
  'ds' = ds$site_no,
  'wa' = wa$site_no,
  'cp' = cp$site_no,
  'ne' = ne$site_no,
  'sr' = sr$site_no,
  'ma' = ma$site_no
)
