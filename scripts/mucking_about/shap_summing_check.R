library(trend)
source('scripts/functions/utilities.R')

shaps <- read_csv('data/models/q95_annual/shaps.csv')

shap_sen_sums <- shaps %>%
  group_by(site_no, var) %>%
  summarise(sen = sens.slope(shap)$estimates) %>%
  group_by(site_no) %>%
  summarise(sen_sum = sum(sen))

shap_sum_sens <- shaps %>%
  group_by(site_no, wateryear) %>%
  summarise(shap_sum = sum(shap)) %>%
  group_by(site_no) %>%
  summarise(sum_sen = sens.slope(shap_sum)$estimates)

sen_comb <- left_join(shap_sen_sums, shap_sum_sens)

ggplot(sen_comb, aes(x = sum_sen, y = sen_sum)) +
  geom_point()

r2(sen_comb$sum_sen, sen_comb$sen_sum)


shap_lm_sums <- shaps %>%
  group_by(site_no, var) %>%
  summarise(lm = lm(shap~wateryear)$coefficients[2]) %>%
  group_by(site_no) %>%
  summarise(lm_sum = sum(lm))

shap_sum_lms <- shaps %>%
  group_by(site_no, wateryear) %>%
  summarise(shap_sum = sum(shap)) %>%
  group_by(site_no) %>%
  summarise(sum_lm = lm(shap_sum~wateryear)$coefficients[2])

lm_comb <- left_join(shap_lm_sums, shap_sum_lms)

ggplot(lm_comb, aes(x = sum_lm, y = lm_sum)) +
  geom_point()

r2(lm_comb$sum_lm, lm_comb$lm_sum)
