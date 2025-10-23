library(tidyverse)
library(reshape2)
source('scripts/functions/utilities.R')
# load it all -------------------------------------------------------------
hw_gage_info <- read_gage_info()
ds_gage_info <- read_gage_info('downstream')
connections_info <- read_gage_info('connections')

target_var <- 'sen'

metric_trends <- read_csv('data/gages/metrics/trends_window3.csv') %>%
  select(site_no, var, all_of(target_var)) %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = 3)
metrics <- colnames(select(metric_trends, !site_no))
metrics_sel <- c('Q_mean', 'Q95', 'Q10', 'TotalRR', 'Q_frequency_high_3', 'Q_frequency_noflow',
                 'Q_totalduration_high_3', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')
metric_labels <- c('Mean Annual Q', 'Q95', 'Q10', 'Runoff Ratio',
                   '# High-Flow Peaks', '# No-Flow Periods',
                   'High-Flow Duration', 'No-Flow Duration',
                   'Half-Flow Date', 'Half-Flow Interval', 'Peak Q Date',
                   'Baseflow Index', 'Flashiness Index', 'FDC Mid-Section Slope',
                   'Recession Constant', 'Recession Seasonality')

pred_trends <- read_csv('data/gages/predictors/pred_trends_window3.csv') %>%
  select(site_no, var, all_of(target_var)) %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = 3)
preds <- colnames(select(pred_trends, !site_no))
preds_sel <- c('precip_annual','temp_annual','pet_annual',
               'precip_jfm','temp_jfm','pet_jfm',
               'precip_amj','temp_amj','pet_amj',
               'precip_jas','temp_jas','pet_jas',
               'precip_ond','temp_ond','pet_ond',
               'ag','developed','forest','grass',
               'water_use')
pred_labels <- c('Precip (annual)', 'Temp (annual)', 'PET (annual)',
                 'Precip (winter)', 'Temp (winter)', 'PET (winter)',
                 'Precip (spring)', 'Temp (spring)', 'PET (spring)',
                 'Precip (summer)', 'Temp (summer)', 'PET (summer)',
                 'Precip (fall)', 'Temp (fall)', 'PET (fall)',
                 'Agriculture', 'Developed', 'Forest', 'Grassland',
                 'Water Use')

all_dat <- left_join(metric_trends, pred_trends) %>% 
  select(all_of(metrics_sel), all_of(preds_sel))
hw_dat <- left_join(metric_trends, pred_trends) %>%
  filter(site_no %in% hw_gage_info$site_no) %>%
  select(all_of(metrics_sel), all_of(preds_sel))
hw_cn_dat <- left_join(metric_trends, pred_trends) %>%
  filter(site_no %in% connections_info$headwater_id) %>%
  select(all_of(metrics_sel), all_of(preds_sel))
ds_dat <- left_join(metric_trends, pred_trends) %>%
  filter(site_no %in% ds_gage_info$site_no) %>%
  select(all_of(metrics_sel), all_of(preds_sel))


# calc cors ---------------------------------------------------------------
#all gages
all_cors <- cor(all_dat, use = 'pairwise.complete', method = 'spearman')
all_cors_df <- all_cors %>%
  melt() %>%
  filter(Var1 %in% metrics & Var2 %in% preds) %>%
  arrange(desc(abs(value)))
plot_dat <- all_cors[c(metrics_sel),c(preds_sel)]
corrplot(plot_dat)

#only headwaters
hw_cors <- cor(hw_dat, use = 'pairwise.complete', method = 'spearman')
hw_cors_df <- hw_cors %>%
  melt() %>%
  filter(Var1 %in% metrics & Var2 %in% preds) %>%
  arrange(desc(abs(value)))
plot_dat <- hw_cors[c(metrics_sel),c(preds_sel)]
corrplot(plot_dat)

#only headwaters with a downstream connection
hw_cn_cors <- cor(hw_cn_dat, use = 'pairwise.complete', method = 'spearman')
hw_cn_cors_df <- hw_cn_cors %>%
  melt() %>%
  filter(Var1 %in% metrics & Var2 %in% preds) %>%
  arrange(desc(abs(value)))
plot_dat <- hw_cn_cors[c(metrics_sel),c(preds_sel)]
rownames(plot_dat) <- metric_labels
colnames(plot_dat) <- pred_labels
corrplot(plot_dat,
         tl.col = 'grey30', tl.srt = 45, tl.cex = 1.5,
         cl.cex = 1.5)

#only downstreams
ds_cors <- cor(ds_dat, use = 'pairwise.complete', method = 'spearman')
ds_cors_df <- ds_cors %>%
  melt() %>%
  filter(Var1 %in% metrics & Var2 %in% preds) %>%
  arrange(desc(abs(value)))
plot_dat <- ds_cors[c(metrics_sel),c(preds_sel)]
corrplot(plot_dat)

#all connections gages
cn_cors <- cor(rbind(hw_cn_dat, ds_dat), use = 'pairwise.complete', method = 'spearman')
plot_dat <- cn_cors[c(metrics_sel),c(preds_sel)]
corrplot(plot_dat)


# hw-ds cor strengths -----------------------------------------------------
cor_comp <- left_join(hw_cn_cors_df, ds_cors_df,
                      by = join_by(Var1, Var2)) %>%
  mutate(cor_diff = abs(value.x) - abs(value.y))

cor_comp_mat <- cor_comp %>%
  select(Var1, Var2, cor_diff) %>%
  pivot_wider(id_cols = Var1, names_from = Var2, values_from = cor_diff) %>%
  mutate(Var1 = factor(Var1, levels = metrics_sel)) %>%
  arrange(Var1) %>%
  select(all_of(preds_sel)) %>%
  as.matrix(.)
row.names(cor_comp_mat) <- metrics_sel

corrplot(cor_comp_mat, method = 'circle', is.corr = F)



# hw-ds sen differences ---------------------------------------------------

hw_sens <- left_join(select(connections_info, site_no = headwater_id), pred_trends) %>%
  select(!site_no) %>%
  pivot_longer(everything(), names_to = 'var', values_to = 'val') %>%
  mutate(type = 'hw')
ds_sens <- left_join(select(connections_info, site_no = downstream_id), pred_trends) %>%
  select(!site_no) %>%
  pivot_longer(everything(), names_to = 'var', values_to = 'val') %>%
  mutate(type = 'ds')

diff <- rbind(hw_sens, ds_sens) %>%
  group_by(var, type) %>%
  summarise(median = median(val, na.rm = T))



# asdf --------------------------------------------------------------------
hw_gage_info <- read_gage_info(type = 'headwaters')
hw_gage_info_fil <- filter(hw_gage_info, order <= 3, dist_index <= 56)

connections <- read_gage_info(type = 'connections') %>%
  left_join(., select(hw_gage_info, headwater_id = site_no, hw_order = order)) %>%
  left_join(., select(ds_gage_info, downstream_id = site_no, ds_order = order)) %>%
  mutate(across(c(hw_order, ds_order), ~ifelse(.x == -99, 0, .x))) %>%
  mutate(connection_id = 1:nrow(.))
connections_fil <- filter(connections, headwater_id %in% hw_gage_info_fil$site_no)

pred_trends <- read_csv('data/gages/predictors/pred_trends_window3.csv') %>%
  select(site_no, var, all_of(c('sen', 'tau_trend'))) %>%
    rename(tau = tau_trend)

connections_trends <- tibble()
for(i in 1:length(preds_sel)){
  metric = preds_sel[i]
  hw_dat <- filter(pred_trends, var == metric & site_no %in% hw_gage_info$site_no)
  ds_dat <- filter(pred_trends, var == metric & site_no %in% ds_gage_info$site_no)
  connection_i <- left_join(connections_fil, hw_dat, by = join_by(headwater_id == site_no)) %>%
    left_join(., ds_dat, by = join_by(downstream_id == site_no), suffix = c('_hw','_ds'))
  
  connections_trends <- rbind(connections_trends, connection_i)
}
trend_diffs <- mutate(connections_trends, across(contains('tau_trend'), ~replace_na(.x, 'none'))) %>%
  mutate(tau_comp = case_when(
    tau_hw == 'none' & tau_ds == 'none' ~ 'None',
    tau_hw == tau_ds ~ 'Same',
    tau_hw %in% c('neg', 'pos') & tau_ds == 'none' ~ 'HW Sensitive',
    tau_ds %in% c('neg', 'pos') & tau_hw == 'none' ~ 'DS Sensitive',
    tau_hw == 'pos' & tau_ds == 'neg' ~ 'Opposite',
    tau_hw == 'neg' & tau_ds == 'pos' ~ 'Opposite'))
    # sen_comp = case_when(
    #   sen_hw == 'none' & sen_ds == 'none' ~ 'None',
    #   sen_hw == sen_ds ~ 'Same',
    #   sen_hw %in% c('neg', 'pos') & sen_ds == 'none' ~ 'HW Sensitive',
    #   sen_ds %in% c('neg', 'pos') & sen_hw == 'none' ~ 'DS Sensitive',
    #   sen_hw == 'pos' & sen_ds == 'neg' ~ 'Opposite',
    #   sen_hw == 'neg' & sen_ds == 'pos' ~ 'Opposite'),
    # sen_diff = abs(sen_hw)-abs(sen_ds))

qwer <- select(trend_diffs, c(connection_id, var_hw, tau_comp, sen_hw, sen_ds)) %>%
  pivot_longer(!c(connection_id, var_hw, tau_comp), names_to = 'gage', values_to = 'trend')
ggplot(subset(qwer), aes(x = gage, y = abs(trend), fill = gage)) +
  geom_boxplot(outliers = F) +
  scale_x_discrete(limits = c('sen_hw','sen_ds'),
                   labels = c('Headwaters', 'Downstream'),
                   name = NULL) +
  scale_y_continuous(#breaks = c(0, 0.01, 0.02),
                     name = 'Sen\'s Slope Magnitude') +
  scale_fill_manual(limits = c('sen_hw','sen_ds'),
                    values = c('#F8766D','#619CCf')) +
  facet_wrap(vars(var_hw), scales = 'free_y') +
  theme(legend.position = 'none',
        text = element_text(size = 10))

qwer <- select(trend_diffs, c(connection_id, var_hw, tau_comp, sen_hw, sen_ds))
ggplot(subset(qwer, tau_comp == 'HW Sensitive'), aes(x = abs(sen_hw), y = abs(sen_ds))) +
  geom_point() +
  geom_abline(slope = 1) +
  facet_wrap(vars(var_hw), scales = 'free')
