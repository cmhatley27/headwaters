# Read in gages -------------------------------------------------------

read_gages <- function(folder = '1981_2022', subset = FALSE, nsub = 10, 
                       set.seed = TRUE, seed = 52798){
  
  #find file path for every downloaded gage
  q_paths <- list.files(path = file.path('data', 'out', 'gage_q', folder), 
                        full.names = TRUE)
  
  #indicate that we want every file except for the last (gage info .csv)
  gages_select <- seq(1,length(q_paths)-1)
  
  # #Optional - select just a subset of gages for faster testing
  if(subset == TRUE){
  num_gages <- nsub
    if(set.seed == TRUE){
    set.seed(seed)
    }
  gages_select <- floor(runif(num_gages, 1, length(q_paths)-1))
  }

  #pull in selected gages (will pull everything if the subsetting is not done)
  qs <- map_df(q_paths[gages_select], read_csv, col_types = 'cDddd')
  return(qs)
}

read_gage_info <- function(folder = '1981_2022', subset = FALSE, qs = NULL){
  
  #find file path for gage info
  info_path <- tail(list.files(path = file.path('data', 'out', 'gage_q', folder), 
                        full.names = TRUE), 1)
  
  gage_info <- read_csv(info_path)
  if(subset == TRUE){
    gage_info <- filter(gage_info, site_no %in% unique(qs$site_no))
  }
  return(gage_info)
}
