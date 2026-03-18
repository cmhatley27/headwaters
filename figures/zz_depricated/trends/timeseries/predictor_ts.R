library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')
library(modifiedmk)

gages_sel <- all_gage_info %>%
  mutate(region = region_recoder(region2)) %>%
  filter(region == 'cp') %>%
  select(site_no, type) %>%
  mutate(type2 = ifelse(str_detect(type, 'downstream'), 'downstream', 'headwater'))

climate_files <- read_csv(paste0('data/gages/predictors/climate/',gages_sel$site_no,'.csv'))

dat <- left_join(climate_files, gages_sel)

ggplot(dat, aes(x = wateryear, y = temp_annual, color = type2, group = site_no)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(vars(type2))
dat_lms <- dat %>%
  group_by(site_no) %>%
  summarise(slp = summary(lm(precip_amj~wateryear))$coefficients[2,1],
            sen = mmkh3lag(precip_amj)[7]) %>%
  left_join(gages_sel)

ggplot(dat_lms, aes(x = type2, y = sen)) +
  geom_boxplot()


snow_files <- read_csv(paste0('data/gages/predictors/swe/',gages_sel$site_no,'.csv'))

swe_dat <- left_join(snow_files, gages_sel)

ggplot(swe_dat, aes(x = wateryear, y = max_swe_day, color = type2, group = site_no)) +
  geom_line() +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(vars(type2))

swe_dat_lms <- swe_dat %>%
  group_by(site_no) %>%
  summarise(slp = summary(lm(max_swe_day~wateryear))$coefficients[2,1],
            sen = mmkh3lag(max_swe_day)[7]) %>%
  left_join(gages_sel)

ggplot(swe_dat_lms, aes(x = type2, y = sen)) +
  geom_boxplot()
