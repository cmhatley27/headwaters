source('scripts/functions/site_sets.R')
library(sf)
source('scripts/functions/load_states.R')
source('scripts/functions/load_gages.R')
library(trend)

sites_sel <- site_sets[['hw']]

statics <- read_csv('data/gages/predictors/pred_statics.csv')
mean_shaps <- read_csv('data/models/flashinessindex_annual_w3_emp/shaps.csv') %>%
  group_by(site_no, var) %>%
  summarise(shap = mean(shap, na.rm = T))
shap_trends <- read_csv('data/models/flashinessindex_annual_w3_emp/trends.csv') %>%
  filter(type == 'shap') %>%
  select(site_no, var, shap_sen = sen, p)
precip_trends <- read_csv('data/gages/predictors/pred_trends_window3.csv') %>%
  filter(var == 'precip_annual') %>%
  select(site_no, precip_sen = sen)
pet_trends <- read_csv('data/gages/predictors/pred_trends_window3.csv') %>%
  filter(var == 'pet_annual') %>%
  select(site_no, pet_sen = sen)
dev_trends <- read_csv('data/gages/predictors/pred_trends_window3.csv') %>%
  filter(var == 'developed') %>%
  select(site_no, dev_sen = sen)

lms <- read_csv('data/models/flashinessindex_annual_w3_emp/lms.csv') %>%
  filter(var == 'soil_perm', type != 'self') %>%
  pivot_wider(id_cols = c(metric, site_no, var), names_from = type, values_from = c(lm_b, lm_r2)) %>%
  left_join(., statics) %>%
  left_join(., mean_shaps) %>%
  left_join(., shap_trends) %>%
  left_join(., precip_trends) %>%
  left_join(., pet_trends) %>%
  left_join(., dev_trends) %>%
  left_join(select(all_gage_info, site_no, lat, lon)) %>%
  filter(site_no %in% sites_sel) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
  st_transform(5070)

ggplot(filter(lms, lm_r2_pet_annual >= 0.2), aes(x = lm_b_pet_annual, y = shap_sen)) +
  geom_point()
cor(lms$lm_b_pet_annual, lms$shap_sen)

r2(lms$lm_b_pet_annual*lms$pet_sen, lms$shap_sen)
r2(lms$lm_b_precip_annual*lms$precip_sen, lms$shap_sen)
r2(lms$lm_b_pet_annual*lms$pet_sen+lms$lm_b_precip_annual*lms$precip_sen, lms$shap_sen)

ggplot(lms, aes(y = lm_b_pet_annual*pet_sen)) +
  geom_histogram()
ggplot(lms, aes(y = lm_b_precip_annual*precip_sen)) +
  geom_histogram()

shaps <- read_csv('data/models/flashinessindex_annual_w3_emp/shaps.csv') %>%
  pivot_wider(id_cols = c(metric, site_no, wateryear), names_from = var, values_from = c(shap, val)) %>%
  filter(site_no %in% sites_sel)

asdf <- shaps %>%
  group_by(site_no) %>%
  summarise(across(starts_with('val_'),
                   ~cor(shap_soil_perm, .x, use = 'pairwise.complete'))) %>%
  pivot_longer(!site_no, names_to = 'var', values_to = 'cor') %>%
  filter(!is.na(cor))

ggplot(asdf, aes(x = var, y = abs(cor))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(shaps, aes(x = val_pet_annual, y = shap_soil_perm, group = site_no)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0.2) +
  geom_hline(yintercept = 0.2-0.001) +
  geom_smooth(method = 'lm', se = F)
