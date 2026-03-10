library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('./scripts/Theme+Settings.R')

trends <- read_csv(paste0('./data/gages/predictors/pred_trends_window3.csv'))

sites_sel <- all_gage_info$site_no[all_gage_info$order <= 3]
site_label <- 'hw'

preds_sel <- unique(trends$var)

plot_dat <- filter(trends, site_no %in% sites_sel, var %in% preds_sel) %>%
  left_join(select(all_gage_info, site_no, lat, lon)) %>%
  filter(!is.na(sen)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat) <- 4326
plot_dat <- st_transform(plot_dat, 5070)

# make plots --------------------------------------------------------------
for(m in preds_sel){
  plot_dat_m <- filter(plot_dat, var == m) %>%
    arrange(abs(tau))
  
  p <- ggplot() +
    geom_sf(data = states) +
    geom_sf(data = plot_dat_m,
            aes(color = tau, shape = p <= 0.1),
            fill = NA, size = 1.5) +
    scale_color_gradient2(limits = quantile(plot_dat_m$tau, c(0.01,0.99)),
                          oob = scales::squish,
                          # high = 'blue',
                          high = "#4575b4",
                          mid = "#ffffcf",
                          low = "#d73027",
                          # low = 'red',
                          name = 'Kendall \u03c4') +
    scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    ggtitle(paste(labelinator(m, pred_labels),'Trends'))
  ggsave(paste0('figures/predictor_trends/predictor_trends_maps/',site_label,'/',m,'.png'),
         plot = p,
         width = 5, height = 3, units = 'in')
}
