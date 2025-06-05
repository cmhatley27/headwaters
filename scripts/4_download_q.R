# Load libraries -------------------------------------------------
library(tidyverse)
library(dataRetrieval)
source('scripts/functions/read_gages.R')

# Download selected gages and save -----------------
start_year <- 1981
end_year <- 2022
start_date_sel <- paste0(start_year,'-10-01')
end_date_sel <- paste0(end_year,'-09-30')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
gage_list <- unique(c(hw_gage_info$site_no, ds_gage_info$site_no))

save_path <- file.path('data', 'gages', 'q')
dir.create(save_path)

count <- 0
for(gage in gage_list){
  if(file.exists(file.path(save_path, paste0(gage,'.csv')))){
    count <- count + 1
    print(paste0(count,'/',length(gage_list),' done!!'))
    next
  }
  
  #get drainage area for Q normalization
  if(gage %in% hw_gage_info$site_no) da <- hw_gage_info$drainage_area[hw_gage_info$site_no == gage]
  if(gage %in% ds_gage_info$site_no) da <- ds_gage_info$drainage_area[ds_gage_info$site_no == gage]
  da <- da*1e6
  
  gage_q <- readNWISdv(
    siteNumbers = gage,
    parameterCd = '00060',
    startDate = start_date_sel, endDate = end_date_sel
  ) %>%
    select(site_no, date = Date, q = X_00060_00003) %>%
    mutate(wateryear = ifelse(month(date) > 9, year(date) + 1, year(date)),
           q = round(q*0.028316847, 4),
           q_norm = round(q*60*60*24*1000/da, 4)) %>%
    select(site_no, date, wateryear, q, q_norm)
  
  write_csv(gage_q, file.path(save_path, paste0(gage,'.csv')))
  
  rm(gage_q)
  
  count <- count + 1
  print(paste0(count,'/',length(gage_list),' done!!'))
}
