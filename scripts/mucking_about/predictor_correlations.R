library(tidyverse)
library(reshape)
pred_trends <- read_csv('data/gages/predictors/pred_trends.csv') %>%
  select(site_no, var, sen) %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = sen) %>%
  select(!site_no)

cors <- cor(pred_trends, use = 'pairwise.complete') %>%
  melt(.)

high_cors <- filter(cors, X1 != X2 & abs(value) >= 0.8)
