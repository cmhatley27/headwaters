# libraries and data ------------------------------------------------------
library(tidyverse)
source('./scripts/functions/utilities.R')
source('./scripts/functions/load_states.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')
source('./scripts/Theme+Settings.R')

trends <- read_csv(paste0('./data/gages/metrics/trends/metrics_trends_window3.csv'))

metrics_sel <- c('Q_mean', 'Q5', 'Q95', 'TotalRR', 
                 'Q_frequency_high_2', 'Q_frequency_noflow',
                 'Q_totalduration_high_2', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 
                 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
metrics_labels <- labelinator(metrics_sel, metric_labels)


cn_dat <- left_join(connections, select(trends, headwater_id = site_no, var, up_sen = sen, up_tau = tau, up_p = p, up_sig = sig_ar2a)) %>%
  left_join(select(trends, downstream_id = site_no, var, down_sen = sen, down_tau = tau, down_p = p, down_sig = sig_ar2a)) %>%
  filter(var %in% metrics_sel) %>%
  mutate(sen_diff = abs(up_sen) - abs(down_sen),
         tau_diff = abs(up_tau) - abs(down_tau),
         sig_cat = case_when(
           up_sig != 'none' & down_sig == 'none' ~ 'up_only',
           up_sig == 'none' & down_sig != 'none' ~ 'down_only',
           up_sig == 'none' & down_sig == 'none' ~ 'both_none',
           up_sig == down_sig & up_sig != 'none' ~ 'both_sig',
           up_sig != down_sig & up_sig != 'none' & down_sig != 'none' ~ 'opposite'
         )) %>%
  filter(!is.na(up_sen), !is.na(down_sen)) %>%
  group_by(var) %>%
  filter(abs(up_sen) <= quantile(c(abs(up_sen)), 0.99, na.rm = T),
         abs(down_sen) <= quantile(c(abs(down_sen)), 0.99, na.rm = T)) %>%
  ungroup(.) %>%
  left_join(select(all_gage_info, headwater_id = site_no, lat, lon)) %>%
  st_as_sf(., coords = c('lon', 'lat'))
st_crs(cn_dat) <- 4326
cn_dat <- st_transform(cn_dat, 5070)

# maps ---------------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff))
  
  p <- ggplot() +
    geom_sf(data = states) +
    geom_sf(data = cn_dat_m,
            aes(color = sen_diff),
            size = 1.5) +
    scale_color_gradient2(limits = quantile(cn_dat_m$sen_diff, c(0.05,0.95)),
                          oob = scales::squish,
                          # high = 'blue',
                          low = '#619CCF',
                          # high = "#4575b4",
                          mid = "#ffffcf",
                          high = '#F8766D',
                          # low = "#d73027",
                          # low = 'red',
                          name = '|Sen| Diff') +
    # scale_shape_manual(limits = c(T,F), values = c(19,21), guide = NULL) +
    ggtitle(paste(labelinator(m, metric_labels),'Connection Trend Differences'))
  ggsave(paste0('figures/metric_trend_deltas/maps/',m,'.png'),
         plot = p,
         width = 5, height = 3, units = 'in')
}

# histograms ---------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff))
  
  percents <- round(table(cn_dat_m$sen_diff >= 0)/nrow(cn_dat_m)*100)
  
  
  p <- ggplot(cn_dat_m, aes(x = sen_diff)) +
    geom_histogram(color = 'black', fill = 'grey80',
                   bins = 20,
                   boundary = 0) +
    geom_vline(xintercept = 0, linewidth = 0.75) +
    annotate('text', label = paste0(percents[1],'%'),
              x= -Inf, y = Inf, hjust = -1, vjust = 8) +
    annotate('text', label = paste0(percents[2],'%'),
             x= Inf, y = Inf, hjust = 2, vjust = 8) +
    # geom_vline(xintercept = quantile(cn_dat_m$sen_diff, c(0.25,0.5,0.75)), color = 'red') +
    xlab('|Sen\'s Slope| Difference') +
    ylab('') +
    ggtitle(paste(labelinator(m, metric_labels),'Connection Trend Differences'))
  p
  ggsave(paste0('figures/metric_trend_deltas/hists/',m,'.png'),
         plot = p,
         width = 4, height = 3, units = 'in')
}


# mini hists --------------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff))
  
  p <- ggplot(cn_dat_m, aes(x = sen_diff)) +
    geom_histogram(color = 'black', fill = 'grey80',
                   bins = 20,
                   boundary = 0) +
    # scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
    #                   name = 'Trend Sig.') +
    # xlim(quantile(cn_dat_m$sen_diff, c(0.01,0.99))) +
    geom_vline(xintercept = 0, linewidth = 0.75) +
    xlab('|Sen\'s Slope| Difference') +
    ylab('') +
    theme_classic(base_line_size = 0.25, base_size = 8) +
    theme(plot.margin = margin(l = 1),
          plot.background = element_rect(fill = fill_alpha('white',0), color = alpha('white',0)),
          panel.background = element_rect(fill = fill_alpha('white',0)))
  p
  ggsave(paste0('figures/metric_trend_deltas/hists_mini/',m,'.png'),
         plot = p,
         width = 1.15, height = 0.67, units = 'in')
}

# scatters ---------------------------------------------------------

for(m in metrics_sel){
  cn_dat_m <- filter(cn_dat, var == m) %>%
    arrange(abs(tau_diff))
  
  sen_cor <- round(cor(cn_dat_m$up_sen, cn_dat_m$down_sen, use = 'pairwise.complete', method = 'spearman'), 3)
  paired_wilcox_p <- round(wilcox.test((cn_dat_m$up_sen), (cn_dat_m$down_sen), paired = T)$p.value, 3)
  wilcox_p <- round(wilcox.test((cn_dat_m$sen_diff))$p.value, 3)
  wilcox_s <- round(wilcox.test((cn_dat_m$sen_diff))$statistic, 3)
  
  p <- ggplot(cn_dat_m, aes(x = up_sen, y = down_sen)) +
    geom_point() +
    geom_abline(slope = 1) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    annotate('text', label = paste('Cor \u03C1:',sen_cor),
             x = -Inf, y = Inf, hjust = -.1, vjust = 2) +
    annotate('text', label = paste('Wil. S:',wilcox_s),
             x = -Inf, y = Inf, hjust = -.1, vjust = 4) +
    annotate('text', label = paste('Wil. p:',wilcox_p),
             x = -Inf, y = Inf, hjust = -.1, vjust = 6) +
    # scale_fill_manual(values = c('grey90', '#4575b4', '#d73027'),
    #                   name = 'Trend Sig.') +
    # xlim(quantile(cn_dat_m$up_sen, c(0.01,0.99))) +
    # ylim(quantile(cn_dat_m$down_sen, c(0.01,0.99))) +
    # geom_vline(xintercept = 0, linewidth = 0.75) +
    xlab('Upstream Sen\'s Slope') +
    ylab('Downstream Sen\'s Slope') +
    ggtitle(paste(labelinator(m, metric_labels),'Connection Trends'))
  p
  ggsave(paste0('figures/metric_trend_deltas/scatters/',m,'.png'),
         plot = p,
         width = 3, height = 3, units = 'in')
}
