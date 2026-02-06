# libraries and data ------------------------------------------------------
library(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/Theme+Settings.R')
shapr_emp <- read_csv('data/models/q_mean_shapr_cp_emp/shaps.csv') %>%
  select(!c(explain_id, none, type)) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'shapr_emp')
shapr_ind <- read_csv('data/models/q_mean_shapr_cp_ind/shaps.csv') %>%
  select(!c(explain_id, none, type)) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'shapr_ind')
shapr_gas <- read_csv('data/models/q_mean_shapr_cp_gas/shaps.csv') %>%
  select(!c(explain_id, none, type)) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'shapr_gas')
shapr_gas_t <- read_csv('data/models/q_mean_shapr_cp_gas_t/shaps.csv') %>%
  select(!c(explain_id, none, type)) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'shapr_gas_t') %>%
  mutate(shapr_gas_t = expm1(shapr_gas_t))
treeshap <- read_csv('data/models/q_mean_treeshap/shaps.csv') %>%
  filter(type == 'shap') %>%
  select(!type) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'treeshap')
obs <- read_csv('data/models/q_mean_treeshap/shaps.csv') %>%
  filter(type == 'obs') %>%
  select(!type) %>%
  pivot_longer(!c(metric, site_no), names_to = 'var', values_to = 'obs')

dat <- left_join(obs, treeshap) %>% left_join(shapr_emp) %>%
  left_join(shapr_gas) %>% left_join(shapr_ind) %>%
  left_join(shapr_gas_t) %>%
  filter(!is.na(shapr_gas))

#MSE
mse_gas <- read_csv('data/models/q_mean_shapr_cp_gas/shap_mse.csv') %>%
  mutate(method = 'gas')
mse_emp <- read_csv('data/models/q_mean_shapr_cp_emp/shap_mse.csv') %>%
  mutate(method = 'emp')
mse_ind <- read_csv('data/models/q_mean_shapr_cp_ind/shap_mse.csv') %>%
  mutate(method = 'ind')
mse_gas_t <- read_csv('data/models/q_mean_shapr_cp_gas_t/shap_mse.csv') %>%
  mutate(method = 'gas_t')
mse <- rbind(mse_gas, mse_emp, mse_ind, mse_gas_t)
mse %>% group_by(method) %>% summarise(mean = mean(value),
                                       sd = sqrt(var(value)))

mse_wide <- pivot_wider(mse, id_cols = 'site_no', names_from = 'method', values_from = 'value')
ggplot(mse_wide, aes(x = emp, y = gas)) +
  geom_abline(slope = 1) +
  geom_point()




#correlations
cor(select(dat, contains('shap')), use = 'pairwise.complete')
#general relationships

dat_long <- pivot_longer(dat, contains('shap'), names_to = 'method', values_to = 'shap')

ggplot(dat_long, aes(x = obs, shap, color = method)) +
  geom_point() +
  facet_wrap(vars(var), scales = 'free')


#mean importance
dat_means <- dat_long %>%
  group_by(var, method) %>%
  summarise(mean = mean(abs(shap)),
            sd = sqrt(var(abs(shap)))) %>%
  arrange(desc(mean)) #%>%
  mutate(var = factor(var, levels = var))

ggplot(dat_means, aes(y = fct_reorder(var, mean), x = mean, fill = method)) +
  geom_col(color = 'black', position = 'dodge2') +
  facet_wrap(vars(method), nrow = 1) +
  xlab('SHAP') +
  ylab('Var') +
  scale_fill_discrete(guide = 'none')
