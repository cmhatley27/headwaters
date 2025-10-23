require(tidyverse)

trend_comparinator <- function(x, var, type){
  df <- select(x, contains(var) & !contains('p'))
  if(type == 'sig'){
    df <- select(df, contains('trend')) %>%
      rename(hw = contains('hw'), ds = contains('ds')) %>%
      mutate(comp = case_when(
        is.na(hw) | is.na(ds) ~ NA,
        hw == 'none' & ds == 'none' ~ 'none',
        hw == ds ~ 'same',
        hw %in% c('pos', 'neg') & ds == 'none' ~ 'hw_only',
        hw == 'none' & ds %in% c('pos', 'neg') ~ 'ds_only',
        hw == 'pos' & ds == 'neg' ~ 'opposite',
        hw == 'neg' & ds == 'pos' ~ 'opposite'
      ))
  }
  return(df$comp)
}