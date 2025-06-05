library(tidyverse)
library(corrplot)
source('./scripts/Theme+Settings.R')


# cor plot for raw metrics ------------------------------------------------
dat <- read_csv('./data/metrics/all_metrics.csv')
dat_subset <- select(dat, where(is.numeric)) %>%
  select(!c(contains('monthly'), contains('high_1'), contains('high_2'))) %>%
  select(!wateryear)

cor_mat <- cor(dat_subset, use = 'pairwise.complete', method = 'spearman')
corrplot(cor_mat, method = 'number')


# corplot for all metrics for subsetting ----------------------------------
dat_sub2 <- select(dat, where(is.numeric)) %>%
  select(!wateryear)
cor_mat2 <- cor(dat_sub2, use = 'pairwise.complete', method = 'spearman')
corrplot(cor_mat2, method = 'number')

#noncorrelated subset:
good_vars <- c(
  'Q_mean',
  'Q10',
  'BFI',
  'Q_frequency_high_2',
  'Q_meanduration_high_2',
  'Q_totalduration_noflow',
  'HFD_mean',
  'HFI_mean',
  'RLD',
  'FDC_slope',
  'Recession_a_Seasonality',
  'qp_elasticity'
)

dat_sub3 <- select(dat, all_of(good_vars))
cor_mat3 <- cor(dat_sub3, use = 'pairwise.complete', method = 'spearman')
corrplot(cor_mat3)

library(FactoMineR)
subset_pca <- PCA(dat_sub3)
subset_pca$eig
metrics_loads = as.data.frame(subset_pca$var$contrib)
metrics_cors = as.data.frame(subset_pca$var$cor)
# cor plot for trends -----------------------------------------------------
dat <- read_csv('./data/metrics/all_trends.csv')

dat_wide <- select(all_trends, site_no, var, tau) %>%
  pivot_wider(id_cols = site_no, names_from = var, values_from = tau) %>%
  select(!site_no) %>%
  select(all_of(colnames(dat_subset)))

cor_mat <- cor(dat_wide, use = 'pairwise.complete', method = 'spearman')
corrplot(cor_mat)

