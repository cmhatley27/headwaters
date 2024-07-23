# Load libraries -------------------------------------------------
library(tidyverse)
library(dataRetrieval)


# Download selected gages, calculate baseflow series, and save -----------------
source(file.path('scripts', 'functions', 'baseflow_wmo.R'))
start_year <- 1981
end_year <- 2022
start_date_sel <- paste0(start_year,'-10-01')
end_date_sel <- paste0(end_year,'-09-30')

gage_list <- read_csv(file.path('data', 'out', paste0('gage_list_',start_year,'start.csv')))$site_no
save_path <- file.path('data', 'out', 'gage_q', paste0(start_year,'_',end_year))
dir.create(save_path)

count <- 0
for(gage in gage_list){
  if(file.exists(file.path(save_path, paste0(gage,'.csv')))){
    count <- count + 1
    print(paste0(count,'/',length(gage_list),' done!!'))
    next
  }
  gage_q <- readNWISdv(
    siteNumbers = gage,
    parameterCd = '00060',
    startDate = start_date_sel, endDate = end_date_sel
  ) %>%
    select(site_no, date = Date, q = X_00060_00003) %>%
    mutate(bf = calc_bf(q),
           wateryear = ifelse(month(date) > 9, year(date) + 1, year(date))) %>%
    select(site_no, date, wateryear, q, bf)
  
  write_csv(gage_q, file.path(save_path, paste0(gage,'.csv')))
  
  rm(gage_q)
  
  count <- count + 1
  print(paste0(count,'/',length(gage_list),' done!!'))
}


gage_info <- whatNWISsites(sites = c(gage_list)) %>%
  select(c(site_no, station_nm, dec_lat_va, dec_long_va))
write_csv(gage_info, file.path(save_path, 'gage_info.csv'))

