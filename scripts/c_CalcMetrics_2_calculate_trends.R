# calc MK, Sen, and MW trends ---------------------------------------------
require(tidyverse)
source('scripts/functions/utilities.R')
source('scripts/Theme+Settings.R')

#select which window length metric sets to calculate trends for
windows <- c(3)

for(i in 1:length(windows)){
  window_size <- windows[i]
  
  #load metrics, drop error string columns
  metrics <- read_csv(paste0('./data/gages/metrics/merged/metrics_window',window_size,'.csv')) %>%
    select(site_no, wateryear, !contains('error_str')) %>%
    pivot_longer(!c(site_no, wateryear), names_to = 'var', values_to = 'val')
  
  #set trend parameters
  a = 0.05 #significance level for trend detection tests
  thresh = 5 #min number of years needed to calculate trend
  
  #calculate trends and then classify them based on direction & significance
  trends <- select(metrics, !c(wateryear)) %>%
    group_by(site_no, var) %>%
    summarise(across(val, ~trendinator(.x, length_thresh = thresh), .unpack = '{inner}')) %>%
    mutate(sig_0 = trend_classifier(sen, p0, alpha = a),
           sig_ar = trend_classifier(sen, p, alpha = a),
           sig_ar2a = trend_classifier(sen, p, alpha = 2*a),
           sig_arfdr = trend_classifier(sen, p, alpha = get_fdr_p(p, fdr_a = 2*a)), #FDR alpha recommended as 2*a
           sig_arfdr2a= trend_classifier(sen, p, alpha = get_fdr_p(p, fdr_a = 4*a)))
 
  #save
  write_csv(trends, paste0('./data/gages/metrics/trends/metrics_trends_window',window_size,'.csv'))
}
