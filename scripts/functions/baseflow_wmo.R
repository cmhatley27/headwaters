# Calculate Baseflow Series -----------------------------------------------
#Simple filter method from World Meteorological Organization (2008) and
#recommended by Tarasova et al. (2018), who compared several baseflow
#estimation techniques for hundreds of catchments in Germany.
#Details at: http://neymo.imgw.pl/pub/WMO_1029_en_LOW_FLOW.pdf

#Input q must not have NAs in the first or last observations.
#Default window length (window_len) and 'turning point factor' (tp_factor) are 
#recommended by the original literature.
#Increasing window_len and/or tp_factor will result in a more aggressively
#smoothed-out baseflow series.

calc_bf <- function(q, window_len = 5, tp_factor = 0.9){
  q_len <- length(q)
  window_num <- ceiling(q_len/window_len)
  
  #break q series into [window_len]-length windows and find the local minima
  #in each.
  window_mins <- integer(length = window_num)
  for(window in seq(1,window_num)){
    #define window start and ends
    window_start <- (window*window_len) - (window_len - 1)
    window_end <- window*window_len
    if(window_end > q_len){
      window_end <- q_len
    }
    #indentify index of minima within window
    window_min <- which.min(q[window_start:window_end]) + window_start - 1
    #if entire window is NA, set the window minimum to be the first value to
    #avoid errors
    if(length(window_min) == 0){
      window_min <- 1 + window_start - 1
    }
    #save index of each minimum
    window_mins[window] <- window_min
  }
  
  #identify 'turning points': local minima from the previous step that are
  #lower than the preceding and succeeding local minima
  turning_points <- integer(length = window_num)
  #trim out first and last minima from search because they don't have two
  #adjacent values to compare to
  for(local_min in seq(2,length(window_mins)-1)){
    #define the 3 adjacent minima to compare. Central point is reduced by
    #a factor (default = 0.9)
    left <- q[window_mins[local_min-1]]
    center <- q[window_mins[local_min]]*tp_factor
    right <- q[window_mins[local_min+1]]
    
    #skip missing minima
    if(anyNA(c(center,left,right))) next
    
    #extract index of turning points
    if(center < left & center < right){
      turning_points[local_min] <- window_mins[local_min]
    }
  }
  #filter out minima that were not turning points, then add the first and last
  #data points back in for interpolation purposes
  turning_points <- c(1,turning_points[turning_points != 0],q_len)
  
  #interpolate between all turning points and constrain interpolated values
  #so that they are not higher than the original q series.
  #Constrain interpolated values to 0 where the original q series is NA
  #to avoid baseflow > q.
  linterp_q <- approx(x = turning_points, y = q[turning_points], 
                      xout = seq(1,q_len))$y
  q_0na <- replace(q, which(is.na(q)), 0)
  linterp_q[linterp_q > q_0na] <- q[linterp_q > q_0na]
  linterp_q <- round(linterp_q, 2)
  
  return(linterp_q)
}
