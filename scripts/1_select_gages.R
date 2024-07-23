# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(dataRetrieval)
library(sf)
source('scripts/Theme+Settings.R')

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
#Reference/Non-reference classification
basin_class <- read_excel(dir_gagesii, 'Bas_Classif')

#Combine all and rename columns
dat <- select(basin_id, id = STAID, name = STANAME, state = STATE, drain_area_sqkm = DRAIN_SQKM, lat = LAT_GAGE, lon = LNG_GAGE) %>%
  left_join(select(basin_class, id = STAID, ref = CLASS, region = AGGECOREGION)) %>%
  left_join(select(hydro, id = STAID, max_order = STRAHLER_MAX)) %>%
  left_join(select(flow_rec, id = STAID, active_09 = ACTIVE09, 
                   flow_years_1900_2009 = FLOWYRS_1900_2009, flow_years_1950_2009 = FLOWYRS_1950_2009, flow_years_1990_2009 = FLOWYRS_1990_2009,
                   7:116))


# Filter by desired characteristics and check availability ---------------------------------------
#Only get headwaters (~stream order <= 3)
stream_order <- 3
#Get gages with at least 30 yrs of data
period_floor <- 0

dat_fil <- filter(dat, active_09 == 'yes', max_order <= stream_order, flow_years_1900_2009 >= period_floor)

#Add up number of available gages during each year
gage_availability <- tibble(
  year = as.numeric(str_sub(colnames(select(dat_fil, starts_with('wy'))), 3, 6)),
  n = colSums(select(dat_fil, starts_with('wy')))
) %>% mutate(prod = (2022-year)*n)

ggplot(data = gage_availability, aes(x = year, y = n)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1900,2010,by = 10)) +
  ggtitle('# of gages active during each year')


#Download filtered gage data from NWIS
nwis_dat_fil <- whatNWISdata(
  siteNumber = as.character(dat_fil$id),
  parameterCd = '00060',
  service = 'dv') 

# Search gages and count NAs --------------------------------------
start_year <- 1981
end_year <- 2022
start_date_sel <- paste0(start_year,'-10-01')
end_date_sel <- paste0(end_year,'-09-30')

dur <- as.numeric(ymd(end_date_sel)-ymd(start_date_sel)) + 1

dat_availability <- nwis_dat_fil %>%
  filter(ymd(end_date_sel) <= ymd(end_date),
         ymd(start_date_sel) >= ymd(begin_date)) %>%
  mutate(days_out_of_period = (as.numeric(ymd(end_date) - ymd(end_date_sel))) + (as.numeric(ymd(start_date_sel) - ymd(begin_date))),
         days_in_period = count_nu - days_out_of_period,
         days_missing = dur - days_in_period)

buffer_range <- 0:(365*(end_year-start_year))

summary <- tibble(
  missing_days = buffer_range,
  n = NA
)

for(buffer in seq_along(buffer_range)){
  summary$n[buffer] <- nrow(filter(dat_availability, days_missing <= buffer_range[buffer]))
}

ggplot(data = summary) +
  geom_line(aes(x = missing_days, y = n)) +
  scale_x_continuous(breaks = seq(0,max(buffer_range), by = 365), labels = seq(0,max(buffer_range)/365), minor_breaks = NULL) +
  xlab('Max Missing Values (yrs)') +
  ggtitle(paste0('# of gages with at most X missing values between ',start_year,'-',end_year,' (',floor(dur/365),' years)'))



# Choose max NAs and filter final dataset ---------------------------------
buffer_sel <- 365*3

print(summary$n[summary$missing_days == buffer_sel])

gages_final <- filter(dat_availability, days_missing <= buffer_sel)

# Geospatial extents ------------------------------------------------------
dir_states <- file.path('data','shapefiles','states','States_shapefile.shp')
states <- st_read(dir_states) %>%
  filter(State_Code != 'AK', State_Code != 'HI')

ggplot(data = nwis_dat_fil) +
  geom_sf(data = states) +
  geom_point(aes(x = dec_long_va, y = dec_lat_va)) +
  ggtitle(paste0('All Gages with Stream Order <= 3 (n=',nrow(nwis_dat_fil),')'))

ggplot(data = dat_availability) +
  geom_sf(data = states) +
  geom_point(aes(x = dec_long_va, y = dec_lat_va)) +
  ggtitle(paste0('All Gages with Data Since ',start_year,', (n=',nrow(dat_availability),')'))

ggplot(data = gages_final) +
  geom_sf(data = states) +
  geom_point(aes(x = dec_long_va, y = dec_lat_va)) +
  ggtitle(paste0('Gages with <= ',buffer_sel,' NAs within period ',start_year,'-',end_year, ' (n=',nrow(gages_final),')'))


# Download gages with missing data --------------------------------------------
min_na <- 5
gages_to_check <- gages_final$site_no[gages_final$days_missing >= min_na]

gages_missing <- tibble()
count <- 0
for(gage in gages_to_check){
  gage_q <- readNWISdv(
    siteNumbers = gage,
    parameterCd = '00060',
    startDate = start_date_sel, endDate = end_date_sel
  ) %>%
    select(c(site_no, Date, X_00060_00003))
  gages_missing <- rbind(gages_missing, gage_q)
  rm(gage_q)
  
  count <- count + 1
  print(paste0(count,'/',length(gages_to_check),' done!!'))
}

write_csv(gages_missing, file.path('data','out', 'gage_q', 'na_checks', 
                                   paste0('gages_',min_na,'naMin_',buffer_sel,'naMax_',start_year,'start.csv')))
# Check missing data periods ----------------------------------------------

gages_missing <- read_csv(file.path('data','out','gage_q','na_checks',
                                    paste0('gages_',min_na,'naMin_',buffer_sel,'naMax_',start_year,'start.csv'))) %>%
  select(site_no, date = Date, q = X_00060_00003) %>%
  pivot_wider(id_cols = date, names_from = site_no, values_from = q) %>%
  pivot_longer(!date, names_to = 'site_no', values_to = 'q') %>%
  left_join(.,select(gages_final, site_no, days_missing)) %>%
  arrange(days_missing)

gages_missing_fil <- filter(gages_missing, is.na(q) == TRUE) %>%
  mutate(site_no = factor(site_no, levels = unique(site_no)),
         site_no_num = match(site_no, levels(site_no)),
         is_na = is.na(q))

ggplot(data = gages_missing_fil) +
  geom_hline(aes(yintercept = site_no_num), linetype = 'solid', color = 'grey80') +
  geom_point(aes(x = date, y = site_no_num, color = site_no), size = 1.7) +
  scale_color_discrete(guide = 'none') +
  ggtitle(paste0('Timing of NAs for each gage with >= 5 NA days, max ',buffer_sel/365,' years of NA'))


# Save list of gage numbers -----------------------------------------------
write_csv(select(gages_final,site_no), file.path('data', 'out', paste0('gage_list_',start_year,'start.csv')))

