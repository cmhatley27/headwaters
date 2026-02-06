library(tidyverse)
source('scripts/Theme+Settings.R')
source('scripts/functions/utilities.R')
source('scripts/functions/load_gages.R')

annual_predictions <- read_csv(paste0('data/models/all_annual/predictions.csv'))

annual_mean_predictions <- annual_predictions %>%
  group_by(site_no, var) %>%
  summarise(annual_mean = mean(pred, na.rm = T))

annual_performance <- annual_predictions %>%
  group_by(var) %>%
  summarise(r2 = r2(pred, obs),
            rmse = rmse(pred, obs)) %>%
  filter(!is.infinite(r2))

mean_predictions <- read_csv(paste0('data/models/all_mean/predictions.csv')) %>%
  left_join(., annual_mean_predictions)

mean_performance <- mean_predictions %>%
  group_by(var) %>%
  summarise(r2 = r2(pred, obs),
            rmse = rmse(pred, obs),
            r2_annual = r2(annual_mean, obs),
            rmse_annual = rmse(annual_mean, obs)) %>%
  filter(!is.infinite(r2))

ggplot(mean_performance, aes(x = ))

comp <- left_join(mean_performance, annual_performance, by = join_by(var),
                  suffix = c('_mean', '_annual'))




