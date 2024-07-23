sig_total_Q <- function(q){
  return(sum(q, na.rm = TRUE))
}

sig_no_flow_frac <- function(q){
  no_flow <- sum(q == 0, na.rm = TRUE)
  return(no_flow/length(q))
}

sig_bfi <- function(q, bf){
  bfi <- sum(bf, na.rm = TRUE)/sum(q, na.rm = TRUE)
  return(bfi)
}

#Richard_Baker Flashiness Index (Baker et al. 2004)
sig_rb_flash <- function(q){
  pathlength <- sum(abs(q - lag(q))[-1], na.rm = TRUE)
  total_q <- sum(q, na.rm = TRUE)
  rb_flash <- pathlength/total_q
  
  if(total_q == 0){
    rb_flash <- 0
  }
  
  return(rb_flash)
}


