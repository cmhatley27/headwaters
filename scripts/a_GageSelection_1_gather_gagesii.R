# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(dataRetrieval)

# Load GAGESII data set ---------------------------------------------------
#Downloaded from: https://www.sciencebase.gov/catalog/item/631405bbd34e36012efa304a
#Scroll down to 'Attached Files' and grab 'basinchar_and_report_sept_2011.zip'

#Point to excel sheet with all relevant data within the downloaded zip
dir_gagesii <- file.path('data', 'gagesii', 'gagesII_sept30_2011_conterm.xlsx')

#Gather relevant info:
#Gage IDs and locations
basin_id <- read_excel(dir_gagesii, sheet = 'BasinID')
#Stream order
hydro <- read_excel(dir_gagesii, sheet = 'Hydro')
#Data availability
flow_rec <- read_excel(dir_gagesii, 'FlowRec')
#Reference/Non-reference classification and hydrological modification index
basin_class <- read_excel(dir_gagesii, 'Bas_Classif')
#Dam info
hydromod <- read_excel(dir_gagesii, sheet = 'HydroMod_Dams')
#climate
climate <- read_excel(dir_gagesii, sheet = 'Climate')

#Combine all and rename columns
gagesii <- select(basin_id, id = STAID, name = STANAME, state = STATE, drainage_area = DRAIN_SQKM, lat = LAT_GAGE, lon = LNG_GAGE) %>%
  left_join(select(basin_class, id = STAID, ref = CLASS, region = AGGECOREGION, dist_index = HYDRO_DISTURB_INDX, wr_comments = WR_REPORT_REMARKS, screen_comments = SCREENING_COMMENTS)) %>%
  left_join(select(hydro, id = STAID, order = STRAHLER_MAX)) %>%
  left_join(select(flow_rec, id = STAID, active_09 = ACTIVE09, 
                   flow_years_1900_2009 = FLOWYRS_1900_2009, flow_years_1950_2009 = FLOWYRS_1950_2009, flow_years_1990_2009 = FLOWYRS_1990_2009,
                   7:116)) %>%
  left_join(select(hydromod, id = STAID, dam_count = NDAMS_2009, dam_storage = STOR_NID_2009)) %>%
  left_join(select(climate, id = STAID, precip = PPTAVG_BASIN, temp = T_AVG_BASIN, pet = PET)) %>%
  #convert precip to mm/year
  mutate(precip = precip*10,
         storage_precip_ratio = dam_storage/precip)


# download NWIS info for gagesii gages ------------------------------------
# dataRetrieval only allows up to 1800 requests in one go so have to get the
# info in batches
batch_length <- 1800
batches <- ceiling(nrow(gagesii)/batch_length)
nwis_gagesii <- data.frame()
for(batch in 1:batches){
  i_start = batch_length*(batch-1) + 1
  i_end = min(batch_length*batch, nrow(gagesii))
  
  nwis_batch <- whatNWISdata(
    siteNumber = as.character(gagesii$id[i_start:i_end]),
    parameterCd = '00060',
    service = 'dv',
    statCd = '00003')
  
  nwis_gagesii <- rbind(nwis_gagesii, nwis_batch)
  print(paste0('batch ', batch, '/', batches, ' done!!!!'))
}
#trim out unneeded columns from nwis
extraneous_cols <- c('agency_cd','site_tp_cd','coord_acy_cd','dec_coord_datum_cd',
                     'alt_acy_va','alt_datum_cd','data_type_cd','parm_cd','stat_cd',
                     'ts_id','loc_web_ds','medium_grp_cd','parm_grp_cd','srs_id',
                     'access_cd')
nwis_gagesii <- select(nwis_gagesii, !all_of(extraneous_cols)) %>%
  rename(lat = dec_lat_va, lon = dec_long_va, alt = alt_va, obs_count = count_nu)

# join gagesii info to nwis info and save -------------------------------------------
all_gages <- left_join(nwis_gagesii, 
                       select(gagesii, c(site_no = id, ref, order, drainage_area, 
                                         precip, dam_count, dam_storage, storage_precip_ratio,
                                         dist_index, wr_comments, screen_comments)))
write_csv(all_gages, './data/gagesii/all_gages_summary.csv')
