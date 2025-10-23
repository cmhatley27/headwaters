source('scripts/functions/utilities.R')

hw_gages <- read_gage_info()
ds_gages <- read_gage_info('downstream')
all_gages <- unique(c(hw_gages$site_no, ds_gages$site_no))

remove_paths <- c('data/gages/q/',
                  'data/gages/climate/',
                  'data/gages/land_cover/rasters/',
                  'data/gages/land_cover/summaries/',
                  'data/gages/geology/',
                  'data/gages/swe/',
                  'data/gages/metrics/loose/fixed_3/',
                  'data/gages/metrics/loose/window_1/',
                  'data/gages/metrics/loose/window_3/',
                  'data/gages/metrics/loose/window_5/',
                  'data/gages/predictors/climate/',
                  'data/gages/predictors/land_cover/',
                  'data/gages/predictors/swe/')

for(i in 1:length(remove_paths)){
  remove_path = remove_paths[i]
  if(!dir.exists(remove_path)) next
  
  downloaded_files <- list.files(remove_path)
  remove_files <- downloaded_files[str_sub(downloaded_files,1,-5) %nin% all_gages]
  file.remove(paste0(remove_path, remove_files))
}





