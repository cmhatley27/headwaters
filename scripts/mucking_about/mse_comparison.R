library(tidyverse)
emp <- read_csv(list.files('data/models/q95_annual/mse/', full.names = T)) %>%
  mutate(method = 'emp')
gas <- read_csv(list.files('data/models/q95_annual_gas/mse/', full.names = T)) %>%
  mutate(method = 'gas')
ind <- read_csv(list.files('data/models/q95_annual_ind/mse/', full.names = T)) %>%
  mutate(method = 'ind')

mses <- rbind(emp, gas, ind) %>%
  pivot_wider(id_cols = c(site_no, wateryear), names_from = method, values_from = value) %>%
  filter(if_all(c(emp, gas, ind), ~!is.na(.x))) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'method', values_to = 'mse')

ggplot(mses, aes(x = method, y = mse)) +
  geom_boxplot()


w3_wrong <- read_csv(list.files('data/models/q95_annual/mse/', full.names = T)) %>%
  mutate(method = 'w3_wrong')
w3 <- read_csv(list.files('data/models/q95_annual_w3_emp/mse/', full.names = T)) %>%
  mutate(method = 'w3')
w1 <- read_csv(list.files('data/models/flashinessindex_annual_w1_emp/mse/', full.names = T)) %>%
  mutate(method = 'w1')

mses <- rbind(w3_wrong, w3) %>%
  pivot_wider(id_cols = c(site_no, wateryear), names_from = method, values_from = value) %>%
  filter(if_all(c(w3_wrong, w3), ~!is.na(.x))) %>%
  pivot_longer(!c(site_no, wateryear), names_to = 'method', values_to = 'mse')

ggplot(filter(mses, site_no == '06879650'), aes(x = method, y = mse)) +
  geom_boxplot()
