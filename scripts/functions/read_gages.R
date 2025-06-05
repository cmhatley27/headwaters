require(tidyverse)
#read info for selected gages. Headwaters selected in script 2_subset_headwaters,
#downstream gages and headwater-downstream connections selected in script 3_select_downstream_gages
read_gage_info <- function(type = 'headwaters'){
  #type = c('headwaters', 'downstream', 'connections)
  files <- c('headwaters' = 'hw_gage_info.csv',
             'downstream' = 'ds_gage_info.csv',
             'connections' = 'hw_ds_connections.csv')
  
  info_path <- file.path('data', 'gages', files[names(files) == type])

  gage_info <- read_csv(info_path)

  return(gage_info)
}


#read gage discharge data, downloaded in script 4_download_q
read_gages <- function(type = c('headwaters'), subset = FALSE, nsub = 10, 
                       set.seed = TRUE, seed = 52798){
  #type = c('headwaters', 'downstream')
  #find file path for every downloaded gage
  q_paths <- list.files(path = file.path('data', 'gages', 'q', type), 
                        full.names = TRUE)
  
  # #Optional - select just a subset of gages for faster testing
  if(subset == TRUE){
  num_gages <- nsub
    if(set.seed == TRUE){
    set.seed(seed)
    }
  gages_select <- floor(runif(num_gages, 1, length(q_paths)-1))
  }

  #pull in selected gages (will pull everything if the subsetting is not done)
  qs <- map_df(q_paths, read_csv, col_types = 'cDddd')
  return(qs)
}

