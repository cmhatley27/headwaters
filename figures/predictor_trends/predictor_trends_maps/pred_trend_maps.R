library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')

trends <- read_csv(paste0('./data/gages/predictors/pred_trends_window3.csv'))

sites_sel <- all_gage_info$site_no[all_gage_info$order <= 3]
site_label <- 'hw'

preds_sel <- unique(trends$var)
preds_sel <- c('ag', 'developed', 'grass', 'forest')

plot_dat <- filter(trends, site_no %in% sites_sel, var %in% preds_sel) %>%
  left_join(select(all_gage_info, site_no, lat, lon)) %>%
  filter(!is.na(sen)) %>%
  mutate(sig = factor(sig_ar2a, levels = c('none', 'pos', 'neg'))) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat) <- 4326
plot_dat <- st_transform(plot_dat, 5070)

# make plots --------------------------------------------------------------
for(m in preds_sel){
  plot_dat_m <- filter(plot_dat, var == m) %>%
    arrange(abs(tau))
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = plot_dat_m,
            aes(color = sen, shape = p <= 0.1),
            fill = NA, size = 1.5) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    scale_color_gradient2(limits = quantile(plot_dat_m$sen, c(0.05,0.95)),
                          oob = scales::squish,
                          # high = 'blue',
                          high = "#4575b4",
                          mid = "#ffffcf",
                          low = "#d73027",
                          # low = 'red',
                          name = 'Sen\'s Slope') +
    scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    ggtitle(paste(labelinator(m, pred_labels),'Trends')) +
    theme(panel.border = element_blank(),
          legend.position = 'right',
          legend.box.spacing = unit(-2,'mm'),
          legend.key.height = unit(7, 'mm'),
          legend.key.width = unit(3, 'mm'))
  ggsave(paste0('figures/predictor_trends/predictor_trends_maps/',site_label,'/',m,'.png'),
         width = 5, height = 3, units = 'in')
}


# mini inset hists --------------------------------------------------------
for(m in preds_sel){
  plot_dat_m <- filter(plot_dat, var == m)
  
  ggplot(plot_dat_m, aes(x = sen, fill = sig)) +
    geom_histogram(color = 'black',
                   bins = 20,
                   boundary = 0,
                   linewidth = 0.15) +
    scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
                      name = 'Trend Sig.', guide = NULL) +
    scale_y_continuous(n.breaks = 3) +
    scale_x_continuous(n.breaks = ifelse(m %in% c('Q5', 'FlashinessIndex'),4,5), limits = quantile(plot_dat_m$sen, c(0.01,0.99))) +
    geom_vline(xintercept = 0, linewidth = 0.5) +
    xlab('Sen\'s Slope') +
    ylab(NULL) +
    theme(panel.border = element_blank(),
          axis.line = element_line(color = 'black', linewidth = 0.25),
          axis.text = element_text(size = rel(0.8)),
          axis.title = element_text(size = rel(0.9)))
  
  ggsave(paste0('figures/predictor_trends/predictor_trends_maps/',site_label,'_minihists/',m,'.png'),
         width = 30, height = 18, units = 'mm')
}
