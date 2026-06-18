# libraries ---------------------------------------------------------------
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(modifiedmk)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('scripts/functions/load_states.R')


# calculate aridity index -------------------------------------------------
start_year <- 1981
end_year <- 2023

for(y in start_year:end_year){
  if(file.exists(paste0('data/rasters/gridmet/annual_ppet_',y,'.tif'))) next
  pr <- rast(paste0('data/rasters/gridmet/pr_',y,'.nc'))
  pr_sum <- sum(pr, na.rm = T)

  pet <- rast(paste0('data/rasters/gridmet/pet_',y,'.nc'))
  pet_sum <- sum(pet, na.rm = T)

  ppet <- pr_sum/pet_sum
  writeRaster(ppet, paste0('data/rasters/gridmet/annual_ppet_',y,'.tif'),
              overwrite = T)
}


# calculate trends --------------------------------------------------------
ppet_ind <- list.files('data/rasters/gridmet/',
                       pattern = 'annual_ppet',
                       full.names = T)

ppet_comb <- rast(ppet_ind)
mk.test()
get_sen <- function(x){
  n <- length(x)
  d <- rep(NA, n * (n - 1)/2)
  k <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      k <- k + 1
      d[k] <- (x[j] - x[i])/(j - i)
    }
  }
  sen <- median(d, na.rm = TRUE)
  return(round(sen,5))
}

ppet_trends <- app(ppet_comb, get_sen)
writeRaster(ppet_trends, 'data/rasters/gridmet/ppet_trends.tif',
            overwrite = T)


# make map ----------------------------------------------------------------
ppet_trends <- rast('data/rasters/gridmet/ppet_trends.tif') %>%
  terra::project('epsg:5070') %>%
  terra::trim(.)
lims <- as.numeric(global(ppet_trends, quantile,  probs = c(0.025,0.975), na.rm = T))

hw_gages <- filter(all_gage_info, order <= 3) %>%
  mutate(cn = site_no %in% connections$headwater_id) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(hw_gages) <- 4326
hw_gages <- st_transform(hw_gages, 5070)

ggplot() +
  geom_spatraster(data = ppet_trends) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = hw_gages, aes(shape = cn), size = 1.4, fill = NA, stroke = 0.35) +
  scale_shape_manual(limits = c(T, F), labels = c('Yes', 'No'), name = 'Downstream\nConnection?',
                     values = c(16,21)) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_gradient2(limits = lims,
                       oob = scales::squish,
                        # high = 'blue',
                        high = "#4575b4",
                        mid = "grey90",
                        low = "#d73027",
                        # low = 'red',
                        name = 'Annual P/PET\nSen\'s Slope',
                       na.value = NA) +
  theme(panel.border = element_blank(),
        legend.position = 'right',
        legend.box.spacing = unit(-2,'mm'),
        legend.key.height = unit(7, 'mm'),
        legend.key.width = unit(3, 'mm'))
ggsave('figures/site_map/conus_ppet.png',
       height = 90, width = 160, units = 'mm')


# hist by order -----------------------------------------------------------
hw_gages <- filter(all_gage_info, order <= 3) %>%
  mutate(cn = site_no %in% connections$headwater_id) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(hw_gages) <- 4326
hw_gages <- st_transform(hw_gages, 5070)

ggplot(hw_gages, aes(x = factor(order))) +
  geom_bar(width = 0.5, color= 'black', linewidth = 0.25) +
  xlab('Stream Order') +
  ylab('Count') +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.25))
ggsave('figures/site_map/order_bars.png',
       height = 22.5, width = 50, units = 'mm')


# land cover classes ------------------------------------------------------
lc <- read_csv('data/gages/predictors/pred_timeseries_window3.csv') %>%
  filter(site_no %in% all_gage_info$site_no[all_gage_info$order <= 3]) %>%
  group_by(site_no) %>%
  summarize(across(c(developed, ag, forest, grass), ~mean(.x, na.rm = T))) %>%
  pivot_longer(!site_no, names_to = 'class', values_to = 'pct') %>%
  group_by(class) %>%
  mutate(p = cume_dist(pct),
         class = factor(class, levels = c('developed', 'ag', 'forest', 'grass'),
                        labels = c('Developed', 'Agriculture', 'Forest', 'Grassland')))

ggplot(lc, aes(x = pct)) +
  geom_histogram() +
  ylab('Count') +
  xlab('% Coverage') +
  facet_wrap(vars(class))

lc_class <- group_by(lc, site_no) %>%
  summarise(class = class[pct == max(pct)]) %>%
  left_join(select(all_gage_info, site_no, ref)) %>%
  mutate(ref = factor(ref,
                      levels = c('Ref', 'Non-ref'),
                      labels = c('Yes', 'No')))

ggplot(lc_class, aes(x = class, alpha = ref)) +
  geom_bar(color = 'black') +
  xlab(NULL) +
  ylab('Count') +
  scale_alpha_discrete(name = 'Reference\nGage?') +
  theme(panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        # legend.text = element_text(size = rel(0.8)),
        legend.key.size = unit(3,'mm'),
        legend.position = 'right')
ggsave('figures/site_map/lc_bars.png',
       height = 50, width = 100, units = 'mm')

table(lc_class$ref)
table(lc_class$class)/nrow(lc_class)
