library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

asdf <- all_gage_info %>%
  filter(site_no %in% connections$headwater_id & site_no %in% matches$headwater_id)

site_sel <- '12115500'
ds_connections <- connections$downstream_id[connections$headwater_id == site_sel]
ds_matches <- matches$downstream_id[matches$headwater_id == site_sel]
sel_gage_info <- filter(all_gage_info, site_no %in% c(site_sel, ds_connections, ds_matches))
huc8s <- 
