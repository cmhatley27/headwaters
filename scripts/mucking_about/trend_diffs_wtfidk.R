# load data ---------------------------------------------------------------
library(tidyverse)
library(reshape2)
library(plotly)
source('scripts/functions/utilities.R')
source('scripts/functions/load_states.R')
source('scripts/functions/load_gages.R')
source('scripts/Theme+Settings.R')

metric_trends <- read_csv('data/gages/metrics/trends/metrics_trends_window3.csv') %>%
  mutate(type = 'metric')
pred_trends <- read_csv('data/gages/predictors/pred_trends.csv') %>%
  mutate(type = 'pred')
pred_statics <- read_csv('data/gages/predictors/pred_statics.csv')

trends <- rbind(metric_trends, pred_trends) %>%
  select(site_no, var, val = sen) %>%
  rbind(., pred_statics) %>%
  mutate(set = ifelse(site_no %in% hw_gage_info$site_no, 'headwater', 'downstream'))

dat <- connections %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, lat, lon, region1, region2, region3)) %>%
  left_join(., select(trends, headwater_id = site_no, var, hw_val = val)) %>%
  left_join(., select(trends, downstream_id = site_no, var, ds_val = val)) %>%
  mutate(diff = abs(hw_val) - abs(ds_val),
         region = region_recoder(region2)) %>%
  mutate(diff_z = scale(diff, center = F)[,1], .by = var)


# plotly map --------------------------------------------------------------
var_sel <- 'Q95'
plot_dat <- dat %>%
  filter(var == var_sel) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(5070)

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = plot_dat, aes(color = diff_z))

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)
plot_geo() %>%
  add_sf(data = subset(plot_dat, abs(diff_z) <= 3), color = ~abs(diff_z)) %>%
  layout(geo = g)


fig <- plot_geo()
fig <- fig %>% add_markers(
  text = ~paste(airport, city, state, paste("Arrivals:", cnt), sep = "<br />"),
  color = ~cnt, symbol = I("square"), size = I(8), hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "Incoming flights<br />February 2011")
fig <- fig %>% layout(geo = g
)

fig
# other misc plots --------------------------------------------------------

#box plots of connection diffs, by region
metrics_sel <- c('Q_mean', 'Q5', 'Q95',
                 'Q_totalduration_low_4', 'Q_frequency_high_2',
                 'HFD_mean', 'HFI_mean', 'FlashinessIndex', 'BaseflowRecessionK')
metrics_sel <- unique(c(pred_trends$var, pred_statics$var))

ggplot(subset(dat, var %in% metrics_sel & abs(diff_z) <= 5), aes(x = region, y = diff_z, fill = region)) +
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  facet_wrap(vars(var), scales = 'free_y')
#p values for these box plots
diff_p <- dat %>%
  filter(var %in% metrics_sel) %>%
  group_by(var, region) %>%
  summarise(wc_p = wilcox.test(diff)$p.value)

#correlations between pred diffs and metric diffs
metric_sel <- 'BFI'
vars_sel <- c(metric_sel, unique(c(pred_trends$var, pred_statics$var)))
dat_wide <- filter(dat, var %in% vars_sel, region == 'cp') %>%
  select(connection_id, var, diff_z) %>%
  pivot_wider(id_cols = connection_id, names_from = var, values_from = diff_z) %>%
  rename(metric_sel = 2)

cors <- dat_wide %>%
  select(!connection_id) %>%
  cor(., use = 'pairwise.complete', method = 'spearman') %>%
  melt(.) %>%
  filter(Var1 == 'metric_sel' & Var2 != 'metric_sel')

ggplot(dat_wide, aes(x = temp_jfm, y = metric_sel)) +
  geom_point() +
  ggtitle(metric_sel)
