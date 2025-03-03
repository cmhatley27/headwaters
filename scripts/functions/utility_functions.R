#quickly save the most recent plot
qsave <- function(w = 7,h = 3.5){
  return(ggsave('figures/temp.png',width = w, height = h,dpi = 700))
}