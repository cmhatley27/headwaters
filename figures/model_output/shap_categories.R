# data --------------------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('scripts/functions/load_states.R')

metric_sel <- 'Q95'
metric_name <- labelinator(metric_sel)

model_name <- paste0(tolower(metric_sel),'_annual_w3_emp')
model_dir <- paste0('data/models/',model_name,'/')

site_label <- 'hw'
sites_sel <- all_gage_info$site_no[all_gage_info$order <= 3]
# sites_sel <- sites_sel[sites_sel %nin% c('12035400', '12143700', '12143900', '14314500')]

shaps <- read_csv(paste0(model_dir,'shaps.csv')) %>%
  filter(site_no %in% sites_sel)

predictions <- read_csv(paste0(model_dir,'predictions.csv')) %>%
  filter(site_no %in% sites_sel)

trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(site_no %in% sites_sel)

# top SHAP VALUE category -------------------------------------------------
plot_dat <- shaps %>%
  group_by(site_no,var) %>%
  summarise(shap = mean(abs(shap))) %>%
  arrange(desc(shap), .by_group = T) %>%
  group_by(site_no) %>%
  summarise(var1 = var[1],
            var1_pct = shap[var == var[1]]/sum(shap),
            var2 = var[2],
            var2_pct = shap[var == var[2]]/sum(shap),
            shap_sum = sum(shap)) %>%
  mutate(var1_name = labelinator(var1, pred_labels),
         var1_cat = factor(labelinator(var1, pred_cats)),
         var2_name = labelinator(var2, pred_labels),
         var2_cat = factor(labelinator(var2, pred_cats))) %>%
  left_join(select(all_gage_info, site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat) <- 4326
plot_dat <- st_transform(plot_dat, 5070)
  
#VALUE MAP
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = plot_dat, aes(color = var1_cat),
          size = 1) +
  scale_color_discrete(limits = names(pred_cat_colors), name = 'Category') +
  ggtitle(paste(metric_name,'Category of Top Predictor'))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_top_SHAP_value_map.png'), height = 3, width = 6, units = 'in')

#VALUE BARS
ggplot(plot_dat, aes(x = var1_cat, fill = var1_cat)) +
  geom_bar(color = 'black') +
  scale_x_discrete(limits = names(pred_cat_colors)) +
  scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
  xlab('') +
  ylab('# Gages') +
  ggtitle(paste(metric_name, 'Category of Top Value Predictor')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_top_SHAP_value_bars.png'), height = 4, width = 5, units = 'in')



# top SHAP trend category -------------------------------------------------

plot_dat <- trends %>%
  filter(type == 'shap') %>%
  group_by(site_no) %>%
  arrange(desc(abs(sen)), .by_group = T) %>%
  summarise(var1 = var[1],
            var1_pct = abs(sen[var == var[1]])/sum(abs(sen)),
            var2 = var[2],
            var2_pct = abs(sen[var == var[2]])/sum(abs(sen)),
            sen_sum = sum(sen)) %>%
  mutate(var1_name = labelinator(var1, pred_labels),
         var1_cat = factor(labelinator(var1, pred_cats), levels = names(pred_cat_colors)),
         var2_name = labelinator(var2, pred_labels),
         var2_cat = factor(labelinator(var2, pred_cats), levels = names(pred_cat_colors))) %>%
  left_join(select(filter(trends, var == metric_sel, type == 'obs'), site_no, sen, p)) %>%
  left_join(select(filter(trends, var == metric_sel, type == 'prediction'), site_no, pred_sen = sen)) %>%
  left_join(select(all_gage_info, site_no, lat, lon, region2, region3)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat) <- 4326
plot_dat <- st_transform(plot_dat, 5070)

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = subset(plot_dat, region3 %in% 1:4), aes(color = factor(region3)))

#Cateogry MAP
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = plot_dat, aes(color = var1_cat),
          size = 1, fill = NA) +
  scale_color_discrete(limits = names(pred_cat_colors), name = 'Category') +
  # scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
  ggtitle(paste(metric_name,'Category of Top Trend Predictor'))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_top_SHAP_trend_map.png'), height = 3, width = 6, units = 'in')

#Category BARS
ggplot(filter(plot_dat, !is.na(p)), aes(x = var1_cat, fill = var1_cat)) +
  geom_bar(color = 'black') +
  scale_x_discrete(limits = names(pred_cat_colors)) +
  scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
  # scale_alpha_manual(limits = c(F,T), labels = c('Non-sig.', 'Sig.'), values = c(0.3, 1), na.value = 0.3, name = 'Metric Trend\nSignificance') +
  xlab('') +
  ylab('# Gages') +
  ggtitle(paste(metric_name, 'Category of Top Trend Predictor')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_top_SHAP_trend_bars.png'), height = 3, width = 5, units = 'in')


#Category HISTOGRAM
ggplot(plot_dat, aes(x = sen, fill = var1_cat)) +
  geom_histogram(color = 'black',
                 bins = 20,
                 boundary = 0) +
  scale_fill_discrete(limits = names(pred_cat_colors), name = 'Category') +
  xlim(quantile(plot_dat$sen, c(0.01,0.99), na.rm = T)) +
  geom_vline(xintercept = 0, linewidth = 0.75) +
  xlab('Sen\'s Slope') +
  ylab('') +
  ggtitle(paste(labelinator(metric_sel, metric_labels),'Trends'))
ggsave(paste0('figures/model_output/',model_name,'/',site_label,'_top_SHAP_trend_hist.png'),
       width = 6, height = 3, units = 'in')


#performance by Category
ggplot(plot_dat, aes(x = sen, y = sen_sum, color = var1_cat)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_abline(slope = 1) +
  scale_color_discrete(limits = names(pred_cat_colors), name = 'Category') +
  facet_wrap(vars(var1_cat))


# top SHAP TREND DIFF category --------------------------------------------
metric_trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(var == metric_sel, type == 'obs') %>%
  select(site_no, sen)
shap_trends <- read_csv(paste0(model_dir,'trends.csv')) %>%
  filter(type == 'shap') %>%
  select(site_no, var, sen) 

cn_trend_diffs <- left_join(connections, rename(shap_trends, headwater_id = site_no, up_sen = sen)) %>%
  left_join(rename(shap_trends, downstream_id = site_no, down_sen = sen)) %>%
  left_join(rename(metric_trends, headwater_id = site_no, up_sen_metric = sen)) %>%
  left_join(rename(metric_trends, downstream_id = site_no, down_sen_metric = sen)) %>%
  mutate(sen_diff = up_sen - down_sen,
         sen_metric_diff = up_sen_metric - down_sen_metric,
         abs_sen_metric_diff = abs(up_sen_metric) - abs(down_sen_metric)) %>%
  filter(!is.na(sen_diff)) #%>%
  filter(abs(sen_diff) <= quantile(abs(sen_diff), 0.999))

plot_dat <- cn_trend_diffs %>%
  group_by(connection_id, headwater_id) %>%
  arrange(desc(sen_diff), .by_group = T) %>%
  summarise(var1 = var[1],
            var1_pct = abs(sen_diff[var == var[1]])/sum(abs(sen_diff)),
            var2 = var[2],
            var2_pct = abs(sen_diff[var == var[2]])/sum(abs(sen_diff)),
            shap_sum = sum(sen_diff),
            across(contains('sen_metric'), ~mean(.x))) %>%
  mutate(var1_name = labelinator(var1, pred_labels),
         var1_cat = factor(labelinator(var1, pred_cats), levels = names(pred_cat_colors)),
         var2_name = labelinator(var2, pred_labels),
         var2_cat = factor(labelinator(var2, pred_cats), levels = names(pred_cat_colors))) %>%
  left_join(select(all_gage_info, headwater_id = site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat) <- 4326
plot_dat <- st_transform(plot_dat,5070)

#Category BARS
ggplot(plot_dat, aes(x = var1_cat, fill = var1_cat)) +
  geom_bar(color = 'black') +
  scale_x_discrete(limits = names(pred_cat_colors)) +
  scale_fill_discrete(limits = names(pred_cat_colors), guide = NULL) +
  xlab('') +
  ylab('# Gages') +
  ggtitle(paste(metric_sel, 'Category of Top \u0394Trend Predictor')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0('figures/model_output/',model_name,'/connection_top_SHAP_trend_diffs_bars.png'), height = 3, width = 5, units = 'in')

#Cateogry MAP
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = plot_dat, aes(color = var1_cat),
          size = 1) +
  scale_color_discrete(limits = names(pred_cat_colors), name = 'Category') +
  ggtitle(paste(metric_sel,'Category of Top \u0394Trend Predictor'))
ggsave(paste0('figures/model_output/',model_name,'/connection_top_SHAP_trend_diffs_map.png'), height = 3, width = 6, units = 'in')


#Category HISTOGRAM
ggplot(plot_dat, aes(x = abs_sen_metric_diff, fill = var1_cat)) +
  geom_histogram(color = 'black',
                 bins = 20,
                 boundary = 0) +
  scale_fill_discrete(limits = names(pred_cat_colors), name = 'Category') +
  xlim(quantile(plot_dat$sen_metric_diff, c(0.01,0.99), na.rm = T)) +
  geom_vline(xintercept = 0, linewidth = 0.75) +
  xlab('Sen\'s Slope Difference') +
  ylab('') +
  ggtitle(paste(labelinator(metric_sel, metric_labels),'Connection Trend Differences'))
ggsave(paste0('figures/model_output/',model_name,'/connection_top_SHAP_trend_diffs_hist.png'),
       width = 6, height = 3, units = 'in')

#Category SCATTER
ggplot(plot_dat, aes(x = up_sen_metric, y = down_sen_metric, color = var1_cat)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope = 1) +
  geom_point() +
  xlim(quantile(plot_dat$up_sen_metric, c(0.01,0.99), na.rm = T)) +
  ylim(quantile(plot_dat$down_sen_metric, c(0.01,0.99), na.rm = T)) +
  scale_color_discrete(limits = names(pred_cat_colors), name = 'Top \u0394SHAP Category') +
  xlab('Upstream Sen\'s Slope') +
  ylab('Downstream Sen\'s Slope') +
  ggtitle(paste(labelinator(metric_sel, metric_labels),'Connection Trends'))
ggsave(paste0('figures/model_output/',model_name,'/connection_top_SHAP_trend_diffs_scatter.png'),
       width = 6, height = 4, units = 'in')


#performance scatter
ggplot(plot_dat, aes(x = sen_metric_diff, y = shap_sum, color = var1_cat)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope = 1) +
  geom_point() +
  xlim(quantile(plot_dat$sen_metric_diff, c(0.01,0.99), na.rm = T)) +
  ylim(quantile(plot_dat$shap_sum, c(0.01,0.99), na.rm = T)) +
  scale_color_discrete(limits = names(pred_cat_colors), name = 'Top \u0394SHAP Category') +
  facet_wrap(vars(var1_cat))
  
# SHAP trend value for a predictor ----------------------------------------

predictor_sel <- 'drainage_area'
plot_dat_m <- filter(trends, var == predictor_sel, type == 'shap') %>%
  left_join(select(all_gage_info, site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(plot_dat_m) <- 4326
plot_dat_m <- st_transform(plot_dat_m, 5070)

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = plot_dat_m,
          aes(color = sen, shape = p <= 0.1),
          fill = NA, size = 1.5) +
  scale_color_gradient2(limits = quantile(plot_dat_m$sen, c(0.01,0.99)),
                        oob = scales::squish,
                        # high = 'blue',
                        high = "#4575b4",
                        mid = "#ffffcf",
                        low = "#d73027",
                        # low = 'red',
                        name = 'Sen\'s Slope') +
  scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
  ggtitle(paste(labelinator(predictor_sel, pred_labels),'SHAP Trends for',metric_sel))
