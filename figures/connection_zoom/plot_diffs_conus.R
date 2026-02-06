library(plotly)
library(tidyverse)
library(sf)
source('scripts/functions/load_states.R')

dat <- read_csv('figures/connection_zoom/cn_diffs.csv')

var_sel <- 'Q_frequency_high_2'
plot_dat <- dat %>%
  filter(var == var_sel) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4269) %>%
  st_transform(5070)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5,
  lataxis = list(range = c(0,90))
)

plot_ly() %>%
  add_sf(data = states, type = 'scatter',
         color = I('grey90')
         ) %>%
  add_sf(data = subset(plot_dat, abs(diff_z) <= 3),
         marker = list(color = ~diff_z,
                       colorscale = 'RdBu',
                       cmid = 0,
                       reversescale = T,
                       colorbar = list(title = '|HW| - |DS|')),
         hoverinfo = 'text',
         text = ~paste('ID:',connection_id,'\nZ: ',round(diff_z, 2))) %>%
  layout(
         title = list(text = paste(var_sel, 'Trend Differences'),
                      yanchor = 'middle')
         )
