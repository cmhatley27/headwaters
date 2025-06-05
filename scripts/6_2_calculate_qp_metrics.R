library(tidyverse)
source('./scripts/functions/qp_metrics.R')

files <- list.files('./data/gages/merged/', full.names = T)

for(i in 1:length(files)){
  dat <- read_csv(files[i], col_types = cols(site_no = col_character()))
  site <- dat$site_no[1]
  print(site)
  
  q = dat$q_norm
  p = dat$precip
  t = dat$date
  
  results <- sig_qp_elasticity(q,p,t, method = 'sanka') %>%
    mutate(site_no = site,
           qp_elasticity = round(qp_elasticity, 3))
  
  write_csv(results,paste0('./data/metrics/r/',site,'.csv'))
  
  print(paste0('gage ',i,'/',length(files),' done!!!!!'))
}
