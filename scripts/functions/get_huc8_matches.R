require(sf)
require(tidyverse)

get_huc8_matches <- function(goi, 
                             gagesii_dir = 'data/gagesii/spreadsheets-in-csv-format',
                             huc8s_dir = 'data/shapefiles/huc8s/HUC8_US.shp'){
  huc8s <- st_read(huc8s_dir)
  sf_use_s2(FALSE)
  
  gages2_loc <- read_csv(paste0(gagesii_dir, "/conterm_basinid.txt")) %>%
    st_as_sf(coords = c('LNG_GAGE', 'LAT_GAGE'), crs = 4269) %>%
    st_transform(st_crs(huc8s))
  
  intersections <- data.frame(
    gage_id = gages2_loc$STAID,
    huc8_index = unlist(st_intersects(gages2_loc, huc8s)))
  intersections$huc8 = huc8s$HUC8[intersections$huc8_index]
  
  out <- data.frame()
  for(i in 1:length(goi)){
    goi_id <- goi[i]
    goi_huc8_index <- intersections$huc8_index[intersections$gage_id == goi_id]
    
    huc8_matches <- filter(intersections, huc8_index == goi_huc8_index & gage_id != goi_id) %>%
      mutate(goi_id = goi_id) %>%
      select(goi_id, target_id = gage_id, huc8)
    
    out <- rbind(out, huc8_matches)
  }
  return(out)
}

