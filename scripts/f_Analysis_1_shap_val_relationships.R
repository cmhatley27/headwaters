library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/functions/var_names.R')
source('scripts/functions/load_gages.R')

metrics_sel <- c('Q95', 'Q5', 'HFD_mean', 'FlashinessIndex')

model_names <- paste0(tolower(metrics_sel),'_annual_w3_emp')
model_dirs <- paste0('data/models/',model_names,'/')

metric_trends <- read_csv(paste0(model_dirs,'trends.csv')) %>%
  filter(var %in% metrics_sel, type == 'obs') %>%
  select(metric, site_no, sen, p)
connection_trends <- read_csv(paste0(model_dirs,'connection_difference_trends.csv')) %>%
  filter(type == 'obs') %>%
  select(metric, connection_id, metric_diff_p = p)

cn_trend_diffs <- connections %>%
  left_join(rename(metric_trends, headwater_id = site_no, up_metric = sen, up_metric_p = p)) %>%
  left_join(rename(metric_trends, downstream_id = site_no, down_metric = sen, down_metric_p = p)) %>%
  mutate(metric_diff = up_metric - down_metric,
         abs_metric_diff = abs(up_metric) - abs(down_metric)) %>%
  left_join(connection_trends) %>%
  filter(!is.na(up_metric), !is.na(down_metric)) %>%
  group_by(metric) %>%
  filter(abs(up_metric) <= quantile(abs(up_metric), 0.99, na.rm = T),
         abs(down_metric) <= quantile(abs(down_metric), 0.99, na.rm = T)) %>%
  mutate(
    diff_sig = metric_diff_p <= 0.1,
    sig_cat = case_when(
      up_metric >= 0 & down_metric >= 0 & up_metric >= down_metric ~ 'up_pos',
      up_metric >= 0 & down_metric >= 0 & up_metric < down_metric ~ 'down_pos',
      up_metric <= 0 & down_metric <= 0 & up_metric <= down_metric ~ 'up_neg',
      up_metric <= 0 & down_metric <= 0 & up_metric > down_metric ~ 'down_neg',
      up_metric >= 0 & down_metric <= 0 ~ 'up_pos_opp',
      up_metric <= 0 & down_metric >= 0 ~ 'up_neg_opp'
    ),
    sig_cat = factor(sig_cat, levels = c('up_neg_opp', 'down_pos', 'up_pos', 'up_neg', 'down_neg', 'up_pos_opp'))
  ) %>%
  ungroup(.)

shaps <- read_csv(paste0(model_dirs,'shaps.csv'))
shap_lms <- shaps %>%
  group_by(metric, site_no, var) %>%
  summarise(lm_b = tryCatch(summary(lm(shap~val))$coefficients[2,1],
                            error = function(e) NA))

cn_shap_lms <- left_join(cn_trend_diffs, rename(shap_lms, headwater_id = site_no, up_lm = lm_b)) %>%
  left_join(rename(shap_lms, downstream_id = site_no, down_lm = lm_b)) %>%
  filter(var == 'precip_annual')  %>%
  mutate(lm_rat = up_lm/down_lm)

cn_shap_lms_m <- filter(cn_shap_lms, metric == m)

ggplot(cn_shap_lms_m, aes(x = sig_cat, y = lm_rat)) +
  geom_boxplot() +
  scale_y_continuous(limits = quantile(cn_shap_lms_m$lm_rat, c(0.01,0.99), na.rm = T))




# correlations ------------------------------------------------------------
shaps <- read_csv(paste0(model_dirs,'shaps.csv'))
shaps_w <- pivot_wider(shaps, id_cols = c(metric, site_no, order, region, wateryear), names_from = var, values_from = shap)
vals_w <- pivot_wider(shaps, id_cols = c(metric, site_no, order, region, wateryear), names_from = var, values_from = val)
dat <- left_join(shaps_w, vals_w, by = c('metric', 'site_no', 'wateryear', 'order', 'region'), suffix = c('_shap', '_val')) %>%
  filter(metric == 'Q5')


dat_cors <- data.frame()
for(s in unique(dat$site_no)){
  dat_s <- filter(dat, site_no == s) %>%
    select(!c(metric, site_no, order, region, wateryear))
  dat_s_cor<- as.data.frame(cor(dat_s, use = 'pairwise.complete')) %>%
    mutate(site_no = s,
           var = row.names(.))
  
  dat_cors <- rbind(dat_cors, dat_s_cor)
}

site_cors <- filter(dat_cors, site_no %in% '12330000') %>%
  filter(var == 'elev_shap') %>%
  select(site_no, all_of(contains('val')))

var_cors <- filter(dat_cors, var == 'twi_shap') %>%
  select(site_no, all_of(contains('val'))) %>%
  pivot_longer(!site_no, names_to = 'var', values_to = 'cor') %>%
  mutate(ne = site_no %in% site_sets[['ne']])


var_cors_avg <- var_cors %>%
  group_by(ne, var) %>%
  summarise(mean_cor = mean(cor, na.rm = T))

ggplot(var_cors_avg, aes(x = var, y = mean_cor, fill = ne)) +
  geom_col(position = 'dodge2') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
