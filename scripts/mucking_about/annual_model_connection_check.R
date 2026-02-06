# libraries and data ------------------------------------------------------
library(tidyverse)
library(plotly)
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

#function for removing outliers
trimmer <- function(x){
  u = mean(x, na.rm = T)
  sd = sqrt(var(x, na.rm = T))
  x_lower = u-3*sd
  x_upper = u+3*sd
  x_trim = x
  x_trim[x_trim <= x_lower | x_trim >= x_upper] = NA
  return(x_trim)
}

annual_predictions <- read_csv(paste0('data/models/all_annual/predictions.csv')) %>%
  left_join(., select(all_gage_info, site_no, order, type, region2)) %>%
  mutate(region = region_recoder(region2)) #%>%
  # filter(order %in% 1:6) %>%
  # group_by(var) %>%
  # mutate(x_trim = trimmer(obs)) %>%
  # filter(!is.na(x_trim))

# look at modeled connections ---------------------------------------------
var_sel <- 'BaseflowRecessionK'

#indiana cluster c(125, 126, 128)
#chicago cluster c(162, 164, 166, 167)
#washington cluster c(223, 225)
cn_sel <- c(165)

gages_sel <- unique(unlist(filter(connections, connection_id %in% cn_sel) %>% select(headwater_id, downstream_id)))

dat <- filter(annual_predictions, var == var_sel, site_no %in% gages_sel)
#scatter of obs vs pred
ggplot(dat, aes(x = obs, y = pred, color = site_no)) +
  geom_point() +
  facet_wrap(vars(site_no)) +
  geom_abline(slope = 1)
#time series of obs vs pred
ggplot(dat) +
  geom_line(aes(x = wateryear, y = obs)) +
  geom_line(aes(x = wateryear, y = pred, color = site_no)) +
  facet_wrap(vars(site_no))

#calculate trends across time series and compare obs vs pred
trends <- dat %>%
  group_by(site_no) %>%
  summarise(across(c(pred, obs), ~trendinator(.x), .unpack = T)) %>%
  pivot_longer(c(pred_sen, obs_sen), names_to = 'type', values_to = 'sen')

ggplot(trends, aes(x = site_no, y = sen, fill = type)) +
  geom_col(position = 'dodge', color = 'black')
