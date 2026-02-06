# data --------------------------------------------------------------------
library(tidyverse)
library(sf)
source('scripts/functions/utilities.R')
source('scripts/Theme+Settings.R')

source('scripts/functions/load_states.R')

hw_gage_info <- read_gage_info(type = 'headwaters')
ds_gage_info <- read_gage_info(type = 'downstream')
gage_coords <- rbind(hw_gage_info, ds_gage_info) %>%
  select(site_no, lat, lon) %>% filter(!duplicated(.))

trend_var <- 'sen'
trend_type <- 'val'
set <- 'hw'

if(set == 'hw'){gage_info <- hw_gage_info
                set_label <- 'Headwaters'}
  
if(set == 'ds'){gage_info <- ds_gage_info
                set_label <- 'Downstream'}

pred_trends <- read_csv('data/gages/predictors/pred_trends.csv') %>%
  select(site_no, var, val = all_of(trend_var))
pred_statics <- read_csv('data/gages/predictors/pred_statics.csv')

dat <- rbind(pred_trends, pred_statics) %>%
  filter(site_no %in% gage_info$site_no) %>%
  left_join(gage_coords) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(., 5070) #equal area projection


# plot trends -------------------------------------------------------------
preds_sel = unique(pred_trends$var)

for(i in 1:length(preds_sel)){
  pred_sel <- preds_sel[i]
  pred_label = pred_labeller(pred_sel)
  
  plot_dat <- filter(dat, var == pred_sel) %>%
    mutate(val = replace_na(val, 0)) %>%
    arrange(abs(val))
    # mutate(val = replace_na(val, 'none')) %>%
    # mutate(val = factor(val, levels = c('none', 'neg', 'pos'))) %>%
    # arrange(val)
  
  ggplot() +
    geom_sf(data = states, linewidth = 0.5) +
    geom_sf(data = plot_dat, aes(color = val), size = 1.2) +
    scale_color_gradient2(low = 'red', mid = 'grey70', high = 'blue',
                          name = NULL) +
    # scale_color_manual(values = c('black','red','blue'),
    #                    limits = c('none','neg','pos'),
    #                    labels = c('Non-significant','Negative','Positive'),
    #                    name = NULL, guide = NULL) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          legend.position = 'right',
          text = element_text(size = 12)) +
    ggtitle(paste0(pred_label,', ',set_label))
  ggsave(paste0('figures/data_maps/pred_trends/preds_',trend_type,'/',set,'/',pred_sel,'.png'), height = 3.6, width = 6, units = 'in')
}


# plot statics ------------------------------------------------------------
preds_sel = unique(pred_statics$var)
trim_outliers = T

for(i in 1:length(preds_sel)){
  pred_sel <- preds_sel[i]
  pred_label = pred_labeller(pred_sel)
  
  plot_dat <- filter(dat, var == pred_sel) %>%
    mutate(val = as.numeric(val)) %>%
    {if(trim_outliers){ 
      filter(., val <= mean(val, na.rm = T) + 3*sqrt(var(val, na.rm = T)),
             val >= mean(val, na.rm = T) - 3*sqrt(var(val, na.rm = T)))
    } else .} %>%
    arrange(abs(val))
  
  ggplot() +
    geom_sf(data = states, linewidth = 0.5) +
    geom_sf(data = plot_dat, aes(color = val), size = 1.2) +
    scale_color_gradient(low = 'grey70', high = 'blue')
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          text = element_text(size = 12)) +
    ggtitle(paste0(pred_label,', ',set_label))
  ggsave(paste0('figures/data_maps/pred_trends/preds_',trend_type,'/',set,'/',pred_sel,'.png'), height = 3.6, width = 6, units = 'in')
}



