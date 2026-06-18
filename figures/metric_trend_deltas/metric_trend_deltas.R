# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('scripts/functions/site_sets.R')

trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_window1.csv'))

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 
                 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')
metrics_labels <- labelinator(metrics_sel, metric_labels)

site_set <- 'hw'
site_set_name <- 'Mid-Atlantic'

metrics <- read_csv('data/gages/metrics/merged/metrics_window1.csv') %>%
  select(site_no, wateryear, all_of(metrics_sel))
cn_diff_trends <- tibble()
for(m in metrics_sel){
  cn_diff_trends_m <- left_join(connections, select(metrics, headwater_id = site_no, wateryear, up_metric = m)) %>%
    left_join(select(metrics, downstream_id = site_no, wateryear, down_metric = m)) %>%
    mutate(metric_diff = up_metric-down_metric) %>%
    group_by(connection_id, headwater_id, downstream_id) %>%
    summarize(across(metric_diff, ~trendinator(.x), .unpack = '{inner}'),
              int = median(metric_diff, na.rm = T) - (sen*median(wateryear))) %>%
    mutate(metric = m, var = metric) %>%
    select(metric, connection_id, headwater_id, downstream_id, var, tau, sen, p, int) %>%
    mutate(type = 'obs',
           diff_sig = p <= 0.05)
  cn_diff_trends <- rbind(cn_diff_trends, cn_diff_trends_m)
}

# cn_diff_trends <- read_csv(paste0('./data/models/',metrics_sel,'_annual_w3_emp/connection_difference_trends.csv')) %>%
#   filter(type == 'obs') %>%
#   mutate(diff_sig = p <= 0.05)

cn_dat <- left_join(connections, select(trends, headwater_id = site_no, var, up_sen = sen, up_tau = tau, up_p = p, up_sig = sig_ar2a)) %>%
  left_join(select(trends, downstream_id = site_no, var, down_sen = sen, down_tau = tau, down_p = p, down_sig = sig_ar2a)) %>%
  filter(var %in% metrics_sel) %>%
  mutate(sen_diff = abs(up_sen) - abs(down_sen),
         tau_diff = abs(up_tau) - abs(down_tau)) %>% 
  filter(!is.na(up_sen), !is.na(down_sen)) %>%
  group_by(var) %>%
  filter(abs(up_sen) <= quantile(c(abs(up_sen)), 0.99, na.rm = T),
         abs(down_sen) <= quantile(c(abs(down_sen)), 0.99, na.rm = T)) %>%
  ungroup(.) %>%
  left_join(select(cn_diff_trends, metric, var, connection_id, diff_sen = sen, diff_p = p, diff_sig)) %>%
  mutate(sig_cat = case_when(
    up_sen >= 0 & down_sen >= 0 & up_sen >= down_sen ~ 'up_pos',
    up_sen >= 0 & down_sen >= 0 & up_sen < down_sen ~ 'down_pos',
    up_sen <= 0 & down_sen <= 0 & up_sen <= down_sen ~ 'up_neg',
    up_sen <= 0 & down_sen <= 0 & up_sen > down_sen ~ 'down_neg',
    up_sen >= 0 & down_sen <= 0 ~ 'up_pos_opp',
    up_sen <= 0 & down_sen >= 0 ~ 'up_neg_opp'
  ),
  sig_cat = factor(sig_cat, levels = c('up_neg_opp', 'down_pos', 'up_pos', 'up_neg', 'down_neg', 'up_pos_opp')),
  sig_cat_simple = case_when(
    !diff_sig ~ 'none',
    up_sen >= 0 & down_sen >= 0 & abs(up_sen) > abs(down_sen) & diff_sig ~ 'up',
    up_sen <= 0 & down_sen <= 0 & abs(up_sen) > abs(down_sen) & diff_sig ~ 'up',
    up_sen >= 0 & down_sen >= 0 & abs(up_sen) < abs(down_sen) & diff_sig ~ 'down',
    up_sen <= 0 & down_sen <= 0 & abs(up_sen) < abs(down_sen) & diff_sig ~ 'down',
    sign(up_sen) != sign(down_sen) & diff_sig ~ 'opposite',
    .default = 'none'
  ),
  sig_cat_simple = factor(sig_cat_simple, levels = c('up', 'down', 'opposite', 'none')),
  sig_cat_dir = case_when(
    up_sen >= 0 & down_sen >= 0 & diff_sig ~ 'both_pos',
    up_sen <= 0 & down_sen <= 0 & diff_sig ~ 'both_neg',
    .default = 'none'
  ),
  sig_cat_dir = factor(sig_cat_dir, levels = c('both_pos', 'both_neg', 'none'))
  ) %>%
  left_join(select(all_gage_info, headwater_id = site_no, lat, lon)) %>%
  filter(headwater_id %in% site_sets[[site_set]]) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(cn_dat) <- 4326
cn_dat <- st_transform(cn_dat, 5070)

n_sigs <- cn_dat %>%
  group_by(connection_id) %>%
  summarise(n_sig = sum(diff_sig)) %>%
  group_by(n_sig) %>%
  summarise(n_cn = n())
ggplot(n_sigs, aes(x = n_sig, y = n_cn)) +
  geom_col()

percents <- st_drop_geometry(cn_dat) %>%
  group_by(var) %>%
  summarise(n = n(),
            up = sum(sig_cat_simple == 'up')/n,
            down = sum(sig_cat_simple == 'down')/n,
            opposite = sum(sig_cat_simple == 'opposite')/n,
            total = up+down+opposite) %>%
  mutate(across(c(up, down, opposite, total), ~round(.x, 2)))

# maps ---------------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(desc(sig_cat_simple))
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = cn_dat_m,
            # aes(color = sen_diff, shape = diff_sig),
            aes(color = sig_cat_simple, fill = sig_cat_simple, shape = sig_cat_dir),
            size = 1) +
    # scale_color_gradient2(limits = quantile(cn_dat_m$sen_diff, c(0.01,0.99)),
    #                       oob = scales::squish,
    #                       low = '#4575b4',
    #                       mid = "#ffffcf",
    #                       high = '#d73027',
    #                       name = '|Sen| Diff') +
    # scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    scale_color_manual(values = c('#d73027','#4575b4','purple','grey60'),
                       aesthetics = c('color', 'fill'), guide = NULL) +
    scale_shape_manual(values = c(24,25,19), guide = NULL) +
    ggtitle(paste(labelinator(m, metric_labels),'Up/Downstream Trend Differences'))
  ggsave(paste0('figures/metric_trend_deltas/maps/',m,'_sig_cat.png'),
         width = 80, height = 52.5, units = 'mm',
         dpi = 600)
}

# histograms ---------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff))
  
  percents <- round(table(cn_dat_m$sig_cat_simple)/nrow(cn_dat_m)*100, 1)
  
  
  p <- ggplot(filter(cn_dat_m), aes(x = sen_diff, fill = sig_cat_simple)) +
    geom_histogram(color = 'black', #fill = 'grey90',
                   bins = 20,
                   boundary = 0) +
    geom_vline(xintercept = 0, linewidth = 0.75) +
    # annotate('text', label = paste0(percents[1],'%'),
    #           x= -Inf, y = Inf, hjust = -1, vjust = 8) +
    # annotate('text', label = paste0(percents[2],'%'),
    #          x= Inf, y = Inf, hjust = 2, vjust = 8) +
    scale_fill_manual(values = c('grey90', 'purple', '#4575b4', '#d73027'),
                      guide = NULL) +
    # scale_fill_manual(values = c('grey90', '#4575b4', '#4575b4', '#d73027', '#d73027',
    #                               '#4575b4', '#4575b4', '#d73027', '#d73027'),
    #                    guide = NULL) +
    # scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
    #                   guide = NULL) +
    xlab('|Sen\'s Slope| Difference') +
    ylab('') +
    ggtitle(paste(labelinator(m, metric_labels),'Connection Trend Differences'))
  p
  ggsave(paste0('figures/metric_trend_deltas/hists/',m,'_sig.png'),
         plot = p,
         width = 4, height = 3, units = 'in')
}


# mini hists --------------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff)) %>%
    mutate(sig_cat_simple  = factor(sig_cat_simple, levels = c('none', 'opposite', 'down', 'up')))
  levels(cn_dat_m$sig_cat_simple) <- c('none', 'opposite', 'down', 'up')
  
  p <- ggplot(cn_dat_m, aes(x = sen_diff, fill = sig_cat_simple)) +
    geom_histogram(color = 'black',
                   bins = 20,
                   boundary = 0,
                   linewidth = 0.15) +
    geom_vline(xintercept = 0, linewidth = 0.5) +
    scale_x_continuous(n.breaks = 4) +
    # scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
    #                   guide = NULL) +
    scale_fill_manual(values = c( 'grey90', 'purple', '#4575b4','#d73027'),
                      guide = NULL) +
    xlab(NULL) +
    ylab(NULL) +
    theme(axis.text = element_text(size = rel(0.8)),
          plot.margin = margin(l = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black', linewidth = 0.25),
          plot.background = element_rect(fill = fill_alpha('white',0), color = alpha('white',0)),
          panel.background = element_rect(fill = fill_alpha('white',0)))
  p
  ggsave(paste0('figures/metric_trend_deltas/hists_mini/',m,'.png'),
         plot = p,
         width = 1, height = 0.5, units = 'in',
         dpi = 600)
}


# minibars ----------------------------------------------------------------
for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff)) %>%
    mutate(sig_cat_simple  = factor(sig_cat_simple, levels = c('none', 'opposite', 'down', 'up')))
  
  percents <- cn_dat_m %>%
    group_by(sig_cat_simple) %>%
    summarise(percent = round(n()/nrow(.)*100))
  
  ggplot(cn_dat_m, aes(x = sig_cat_simple, fill = sig_cat_simple)) +
    geom_bar(color = 'black', linewidth = 0.15) +
    geom_text(data = percents, aes(x = sig_cat_simple, y = ))
    # scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
    #                   guide = NULL) +
    scale_x_discrete(limits = c('up', 'opposite', 'down'),
                     labels = NULL) +
    scale_fill_manual(limits = c('up', 'opposite', 'down'),
                      values = c('#d73027', 'purple', '#4575b4'),
                      guide = NULL) +
    xlab(NULL) +
    ylab(NULL) +
    theme(axis.text = element_text(size = rel(0.8)),
          plot.margin = margin(l = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black', linewidth = 0.25),
          plot.background = element_rect(fill = fill_alpha('white',0), color = alpha('white',0)),
          panel.background = element_rect(fill = fill_alpha('white',0)))
  
  ggsave(paste0('figures/metric_trend_deltas/bars_mini/',m,'.png'),
         width = 1, height = 0.5, units = 'in',
         dpi = 600)
}

# scatters ---------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(desc(sig_cat_simple))
  
  percents <- round(table(cn_dat_m$sig_cat[cn_dat_m$diff_sig])/nrow(cn_dat_m)*100)
  percents
  
  sen_cor <- round(cor(cn_dat_m$up_sen, cn_dat_m$down_sen, use = 'pairwise.complete'), 3)
  sen_r2 <- round(r2(cn_dat_m$up_sen, cn_dat_m$down_sen), 3)
  
  ggplot(cn_dat_m, aes(x = up_sen, y = down_sen, text = connection_id, 
                            color = sig_cat_simple, fill = sig_cat_simple)) +
    geom_point(size = 1) +
    geom_abline(slope = 1) +
    # geom_abline(slope = -1, alpha = 0.33, lty = 'dashed') +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(n.breaks = 4) +
    # scale_color_manual(values = c('black', '#4575b4', '#4575b4', '#d73027', '#d73027',
    #                               '#4575b4', '#4575b4', '#d73027', '#d73027'),
    #                    guide = NULL) +
    scale_color_manual(values = c('#d73027','#4575b4','purple','grey60'),
                       labels = c('Up > Down', 'Down > Up',
                                  'Opposite Trends', 'Nonsig. Difference'),
                       name = 'Difference Category', aesthetics = c('color', 'fill')) +
    # scale_shape_manual(values = c(24,25,19), 
    #                    labels = c('Increasing', 'Decreasing', 'Opposite/Nonsig.'),
    #                    name = 'Direction Category') +
    annotate('text', label = paste('\u03C1:',sen_cor),
             x = -Inf, y = Inf, hjust = -.1, vjust = 2.5) +
    xlab('Upstream Sen\'s Slope') +
    ylab('Downstream Sen\'s Slope') +
    ggtitle(paste(labelinator(m, metric_labels),'Up/Down Trend Differences')) +
    theme(legend.position = 'none',
          legend.box.spacing = unit(5,'mm'),
          legend.spacing.y = unit(0,'mm'),
          legend.key.spacing.y = unit(-2,'mm'),
          plot.margin=unit(c(1,21,1,1), "mm"))
  ggsave(paste0('figures/metric_trend_deltas/scatters/',m,'_sig_cat.png'),
         width = 75, height = 52.5, units = 'mm')
  #with legend: w:100, h:52.5
  #without legend: w:52.5
}

# mini maps ---------------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(desc(sig_cat_simple))
  
  ggplot() +
    geom_sf(data = states) +
    geom_sf(data = cn_dat_m,
            # aes(color = sen_diff, shape = diff_sig),
            aes(color = sig_cat_simple, fill = sig_cat_simple),
            size = 0.5) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    # scale_color_gradient2(limits = quantile(cn_dat_m$sen_diff, c(0.01,0.99)),
    #                       oob = scales::squish,
    #                       low = '#4575b4',
    #                       mid = "#ffffcf",
    #                       high = '#d73027',
    #                       name = '|Sen| Diff') +
    # scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    scale_color_manual(values = c('#d73027','#4575b4','purple','grey60'),
                       aesthetics = c('color', 'fill'), guide = NULL) +
    # scale_shape_manual(values = c(24,25,19), guide = NULL) +
    theme(panel.border = element_blank())
  ggsave(paste0('figures/metric_trend_deltas/maps_mini/',m,'_sig_cat.png'),
         width = 35, height = 25, units = 'mm',
         dpi = 600)
}

# medium hists --------------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff)) %>%
    mutate(sig_cat_simple  = factor(sig_cat_simple, levels = c('none', 'opposite', 'down', 'up')))
  levels(cn_dat_m$sig_cat_simple) <- c('none', 'opposite', 'down', 'up')
  
  ggplot(cn_dat_m, aes(x = sen_diff, fill = sig_cat_simple)) +
    geom_histogram(color = 'black',
                   bins = 20,
                   boundary = 0,
                   linewidth = 0.15) +
    geom_vline(xintercept = 0, linewidth = 0.5) +
    scale_x_continuous(n.breaks = 4) +
    # scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
    #                   guide = NULL) +
    scale_fill_manual(values = c( 'grey90', 'purple', '#4575b4','#d73027'),
                      guide = NULL) +
    xlab('|Sen\'s Slope| Diff.\n(Upstream-Downstream)') +
    ylab(NULL) +
    theme(axis.text = element_text(size = rel(0.9)),
          plot.margin = margin(l = 1),
          panel.border = element_blank(),
          axis.line = element_line(color = 'black', linewidth = 0.25),
          plot.background = element_rect(fill = fill_alpha('white',0), color = alpha('white',0)),
          panel.background = element_rect(fill = fill_alpha('white',0)))

  ggsave(paste0('figures/metric_trend_deltas/hists_medium/',m,'.png'),
         width = 35, height = 25, units = 'mm',
         dpi = 600)
}
