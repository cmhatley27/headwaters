require(tidyverse)

sig_qp_elasticity <- function(q, p, t, method = 'sanka'){
  dat <- tibble(q = q, p = p, t = t) %>%
    mutate(wateryear = ifelse(month(t) >= 10, year(t) + 1, year(t)))
  
  dat_annual <- dat %>%
    group_by(wateryear) %>%
    summarise(n_q = length(!is.na(q)),
              n_p = length(!is.na(p)),
              q = sum(q, na.rm = T),
              p = sum(p, na.rm = T))
  
  switch(method,
         sawicz = {
           dat_annual <- mutate(dat_annual,
                                dq = q/n_q - lag(q/n_q),
                                dp = p/n_p - lag(p/n_p),
                                e = dq/dp*mean(p)/mean(q))
         },
         sanka = {
           dat_annual <- mutate(dat_annual,
                                dq = q/n_q - mean(q/n_q, na.rm = T),
                                dp = p/n_p - mean(p/n_p, na.rm = T),
                                e = dq/dp*mean(p)/mean(q))
         })
  out <- select(dat_annual, wateryear, qp_elasticity = e)
  return(out)
}