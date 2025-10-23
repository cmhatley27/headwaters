library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/connections_v2/functions/get_gages_diff.R')

#name for output files
set_name <- 'order12_hw_diffs_set1'

#select which gages to calculate difference metrics for
hw_gage_info <- read_gage_info()
connections <- read_gage_info('connections')
goi <- hw_gage_info$site_no[hw_gage_info$order %in% 1:2]

#load gagesii
gagesii_dir <- 'data/gagesii/spreadsheets-in-csv-format'
gages2_meta <- read_csv(paste0(gagesii_dir, "/conterm_basinid.txt"))      
gages2_clim <- read_csv(paste0(gagesii_dir, "/conterm_climate.txt"))
gages2_hydro <- read_csv(paste0(gagesii_dir, "/conterm_hydro.txt"))
gages2_topo <- read_csv(paste0(gagesii_dir, "/conterm_topo.txt"))
gages2_complete <- Reduce(function(x, y) left_join(x, y, by="STAID"), list(gages2_meta, 
                                                                           gages2_clim, 
                                                                           gages2_hydro, 
                                                                           gages2_topo))

#select columns and their weights for calculating difference metric
#geographic distance is automatically included in output and doesnt need to be 
#specified in the gagesii_cols vector. The first weight value is for distance.
gagesii_cols <- c('ELEV_MEAN_M_BASIN', 'PPTAVG_BASIN', 'T_AVG_BASIN', 'WD_BASIN', 'PRECIP_SEAS_IND')
weights = c(1,1,1,1,1,1)

#run function to calculate difference and save outputs
diffs <- get_gages_diff(goi = goi, 
                        gagesii_dir = gagesii_dir,
                        gagesii_cols = gagesii_cols,
                        weights = weights)
write_csv(diffs, paste0('scripts/connections_v2/',set_name,'.csv'))

diff_info <- data.frame(
  col = c('geo_dist', gagesii_cols),
  weight = weights
)
write_csv(diff_info, paste0('scripts/connections_v2/',set_name,'_info.csv'))