# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
gage_list <- unique(c(hw_gage_info$site_no, ds_gage_info$site_no))


# calculate climate predictors --------------------------------------------
#annual
for(i in 1:length(gage_list)){
  gage_sel <- gage_list[i]
  gage_dat <- read_csv(paste0('./data/gages/climate/',gage_sel,'.csv'))
  
  annual_climate_preds <- gage_dat %>%
    mutate(wateryear = ifelse(month(date) >= 10, year(date) + 1, year(date))) %>%
    group_by(wateryear) %>%
    summarise(precip_annual = sum(precip, na.rm = T),
              pet_annual = sum(pet, na.rm = T),
              temp_annual = mean(temp, na.rm = T),
              ppet_annual = precip_annual/pet_annual) %>%
    mutate(across(ends_with('annual'), ~lag(.x), .names = '{.col}_prev'),
           site_no = gage_sel)

  seasonal_climate_preds <- gage_dat %>%
    mutate(wateryear = ifelse(month(date) >= 10, year(date) + 1, year(date)),
           month = month(date),
           season = case_when(
             month %in% 10:12 ~ 'ond',
             month %in% 1:3 ~ 'jfm',
             month %in% 4:6 ~ 'amj',
             month %in% 7:9 ~ 'jas'
           )) %>%
    group_by(wateryear, season) %>%
    summarise(precip = sum(precip, na.rm = T),
              pet = sum(pet, na.rm = T),
              temp = mean(temp, na.rm = T),
              ppet = precip/pet) %>%
    pivot_wider(names_from = season, values_from = !c(wateryear,season)) %>%
    ungroup() %>%
    mutate(across(!wateryear, ~lag(.x), .names = '{.col}_prev'),
           site_no = gage_sel)
  
  climate_preds <- left_join(annual_climate_preds, seasonal_climate_preds) %>%
    select(site_no, wateryear, everything())
  
  write_csv(climate_preds, paste0('data/gages/predictors/climate/',gage_sel,'.csv'))
  print(paste0('gage ',i,'/',length(gage_list),' done!'))
}


# SWE ---------------------------------------------------------------------
for(i in 1:length(gage_list)){
  gage_sel <- gage_list[i]
  gage_dat <- read_csv(paste0('./data/gages/swe/',gage_sel,'.csv'))
  
  swe_preds <- gage_dat %>%
    mutate(wateryear = ifelse(month(date) >= 10, year(date)+1, year(date))) %>%
    mutate(swe_diff = swe - lag(swe)) %>%
    group_by(wateryear) %>%
    summarise(max_swe = max(swe, na.rm = T),
              max_swe_date = date[swe == max_swe][1],
              max_swe_day = yday(max_swe_date),
              zero_swe_date = date[swe == 0 & date > max_swe_date][1],
              zero_swe_day = yday(zero_swe_date),
              swe_annual = sum(swe_diff[swe_diff > 0], na.rm = T),
              swe_persistence = sum(swe > 0)/length(swe)) %>%
    mutate(max_swe_day = case_when(year(max_swe_date) == wateryear ~ max_swe_day + 92,
                                   leap_year(max_swe_date) ~ max_swe_day - 274,
                                   !leap_year(max_swe_date) ~ max_swe_day - 273),
           zero_swe_day  = case_when(year(zero_swe_date) == wateryear ~ zero_swe_day + 92,
                                     leap_year(zero_swe_date) ~ zero_swe_day - 274,
                                     !leap_year(zero_swe_date) ~ zero_swe_day - 273),
           melt_duration = as.numeric(zero_swe_date - max_swe_date),
           site_no = gage_sel) %>%
      select(site_no, wateryear, swe_annual, max_swe, max_swe_day, zero_swe_day, melt_duration, swe_persistence)
  
  write_csv(swe_preds, paste0('data/gages/predictors/swe/',gage_sel,'.csv'))
  print(paste0('gage ',i,'/',length(gage_list),' done!'))
}


# Other anthro variables --------------------------------------------------
wuse <- read_delim('data/gagesii/Dataset10_WaterUse/WaterUse_1985-2010.txt') %>%
  rename(site_no = STAID) %>%
  filter(site_no %in% gage_list) %>%
  pivot_longer(!site_no, names_to = 'year', values_to = 'water_use') %>%
  mutate(year = str_sub(year, 3))
write_csv(wuse, 'data/gages/predictors/other_temporal/water_use.csv')


# merge all temporals -----------------------------------------------------
climate <- gather_loose('data/gages/predictors/climate')
swe <- gather_loose('data/gages/predictors/swe')
lc <- gather_loose('data/gages/predictors/land_cover')
wuse <- read_csv('data/gages/predictors/other_temporal/water_use.csv')

merge <- left_join(climate, swe) %>%
  left_join(., lc, join_by(site_no, wateryear == year)) %>%
  left_join(., wuse, join_by(site_no, wateryear == year))

write_csv(merge, 'data/gages/predictors/pred_timeseries.csv')
