# load libraries and data -------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
gage_list <- unique(c(hw_gage_info$site_no, ds_gage_info$site_no))

# get gagesii static predictors --------------------------------------------
library(readxl)
dir_gagesii <- file.path('data', 'gagesii', 'gagesII_sept30_2011_conterm.xlsx')
basin_id <- read_excel(dir_gagesii, sheet = 'BasinID')
topo <- read_excel(dir_gagesii, sheet = 'Topo')
soils <- read_excel(dir_gagesii, sheet = 'Soils')
hydro <- read_excel(dir_gagesii, sheet = 'Hydro')
classif <- read_excel(dir_gagesii, sheet = 'Bas_Classif')

statics <- basin_id %>%
  select(site_no = STAID, drainage_area = DRAIN_SQKM) %>%
  left_join(., select(topo, site_no = STAID, elev = ELEV_MEAN_M_BASIN, slope = SLOPE_PCT)) %>%
  left_join(., select(soils, site_no = STAID, soil_perm = PERMAVE, soil_awc = AWCAVE)) %>%
  left_join(., select(hydro, site_no = STAID, twi = TOPWET)) %>%
  left_join(., select(classif, site_no = STAID, dist_index = HYDRO_DISTURB_INDX)) %>%
  filter(site_no %in% gage_list) %>%
  pivot_longer(!site_no, names_to = 'var', values_to = 'val')
write_csv(statics, 'data/gages/predictors/statics/gagesii_statics.csv')


# calculate geologic age --------------------------------------------------
geol <- gather_loose('data/gages/geology/')
ages <- read_csv('data/shapefiles/sgmc/SGMC_Age.csv') %>%
  select(unit = UNIT_LINK, min = MIN_MA, max = MAX_MA) %>%
  mutate(age = (min + max)/2) %>%
  select(unit, age)

geol_age <- left_join(geol, ages) %>%
  mutate(age = age*area) %>%
  group_by(site_no) %>%
  summarise(age = round(sum(age, na.rm = T)), 2) %>%
  mutate(var = 'age') %>%
  select(site_no, var, val = age)

write_csv(geol_age, 'data/gages/predictors/statics/geol_age.csv')


# hydro means ------------------------------------------------------------
climate_dat <- gather_loose('data/gages/climate/')
climate_means <- climate_dat %>%
  mutate(wateryear = ifelse(month(date) >= 10, year(date)+1, year(date))) %>%
  group_by(site_no, wateryear) %>%
  summarise(precip = sum(precip),
            temp = mean(temp),
            pet = sum(pet)) %>%
  group_by(site_no) %>%
  summarise(across(c(precip, temp, pet), mean, .names = '{.col}_mean'))

q_dat <- gather_loose('data/gages/q/')
q_means <- q_dat %>%
  mutate(wateryear = ifelse(month(date) >= 10, year(date)+1, year(date))) %>%
  group_by(site_no, wateryear) %>%
  summarise(q_norm = sum(q_norm)) %>%
  group_by(site_no) %>%
  summarise(q_norm_mean = mean(q_norm))

hydro_means <- left_join(q_means, climate_means) %>%
  pivot_longer(!site_no, names_to = 'var', values_to = 'val')

write_csv(hydro_means, 'data/gages/predictors/statics/hydro_means.csv')


# water use mean ----------------------------------------------------------
wuse_mean <- read_delim('data/gagesii/Dataset10_WaterUse/WaterUse_1985-2010.txt') %>%
  rename(site_no = STAID) %>%
  filter(site_no %in% gage_list) %>%
  pivot_longer(!site_no, names_to = 'year', values_to = 'water_use') %>%
  mutate(year = str_sub(year, 3)) %>%
  group_by(site_no) %>%
  summarise(val = mean(water_use)) %>%
  mutate(var = 'water_use_mean') %>%
  select(site_no, var, val)

write_csv(wuse_mean, 'data/gages/predictors/statics/wuse_mean.csv')

# merge statics -----------------------------------------------------------
gagesii <- read_csv('data/gages/predictors/statics/gagesii_statics.csv')
geol_age <- read_csv('data/gages/predictors/statics/geol_age.csv')
hydro_means <- read_csv('data/gages/predictors/statics/hydro_means.csv')
wuse_mean <- read_csv('data/gages/predictors/statics/wuse_mean.csv')

merged <- rbind(gagesii, geol_age, hydro_means, wuse_mean)
write_csv(merged, 'data/gages/predictors/pred_statics.csv')
