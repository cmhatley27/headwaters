require(terra)
require(tidyverse)
require(sf)
source('scripts/functions/load_states.R')

get_gages_diff <- function(goi, gagesii_dir = 'data/gagesii/spreadsheets-in-csv-format', 
                           gagesii_cols, weights = NULL){
  gages2_meta <- read_csv(paste0(gagesii_dir, "/conterm_basinid.txt"))      
  gages2_clim <- read_csv(paste0(gagesii_dir, "/conterm_climate.txt"))
  gages2_hydro <- read_csv(paste0(gagesii_dir, "/conterm_hydro.txt"))
  gages2_topo <- read_csv(paste0(gagesii_dir, "/conterm_topo.txt"))
  gages2_complete <- Reduce(function(x, y) left_join(x, y, by="STAID"), list(gages2_meta, 
                                                                             gages2_clim, 
                                                                             gages2_hydro, 
                                                                             gages2_topo))
  gages2_selected <- select(gages2_complete, c('STAID', 'LNG_GAGE', 'LAT_GAGE',
                                               all_of(gagesii_cols)))
  
  if(is.null(weights)) weights = rep(1,length(gagesii_cols)+1)
  
  out <- data.frame()
  for(i in 1:length(goi)){
    gage_id <- goi[i]
    goi_index <- which(gages2_selected$STAID == gage_id) #This index is the same for each "gages2_XXX" above, except _descriptions
    
    goi_loc <- filter(gages2_selected, STAID == gage_id) %>%
      select(lon = LNG_GAGE, lat = LAT_GAGE)
    
    network_loc <-  filter(gages2_selected, STAID != gage_id) %>%
      select(lon = LNG_GAGE, lat = LAT_GAGE)
    
    #Calculate geographic distance
    geo_dist <- as.vector(terra::distance(goi_loc, network_loc, lonlat = T))
   
    #Calculate differences for other characteristics, combine with dist, and scale
    gages2_goi <- filter(gages2_selected, STAID == gage_id) %>%
      select(!c(STAID, LNG_GAGE, LAT_GAGE))
    gages2_network <- filter(gages2_selected, STAID != gage_id) %>%
      select(!c(STAID, LNG_GAGE, LAT_GAGE))
    gages2_diff <- gages2_network-slice(gages2_goi, rep(1, each = nrow(gages2_network)))
    
    #Combine with distance
    gages2_final <- cbind(geo_dist, gages2_diff) %>%
      #scale everything to mean 0 and s.d. 1
      mutate(across(everything(), ~abs(.x))) %>%
      mutate(across(everything(), ~(.x-min(.x))/(max(.x)-min(.x)))) %>%
      #mutate(across(everything(), ~scale(abs(.x))[,1])) #%>%
      #calculate difference metric as sum of all scaled differences (equal weighting)
      mutate(diffmetric = apply(., 1, function(x) weighted.mean(x, weights)),
             ranking = rank(diffmetric),
             #reattach gage number and location
             target_id = gages2_selected$STAID[-goi_index],
             LNG_GAGE = gages2_selected$LNG_GAGE[-goi_index],
             LAT_GAGE = gages2_selected$LAT_GAGE[-goi_index],
             goi_order = gages2_hydro$STRAHLER_MAX[goi_index],
             target_order = gages2_hydro$STRAHLER_MAX[-goi_index],
             goi_drainage = gages2_meta$DRAIN_SQKM[goi_index],
             target_drainage = gages2_meta$DRAIN_SQKM[-goi_index],
             drainage_ratio = goi_drainage/target_drainage,
             goi_id = gage_id) %>%
      select(goi_id, target_id, everything())

    #old stuff for plotting 'best' gages. Probably move to plotting function later
    # if(nearest >= 1){
    #   gages2_subset <- filter(gages2_final, ranking <= nearest)
    # } else {
    #   gages2_subset <- filter(gages2_final, ranking <= round(nearest*nrow(gages2_final)))
    # }
    out <- rbind(out, gages2_final)
  }
  return(out)
}

plot_gages_diff <- function(diff_frame, goi, type = 'metric_value', 
                            gagesii_dir = 'data/gagesii/spreadsheets-in-csv-format'){
  #diff_frame is the output of the get_gages_diff function
  all_sites <- read_csv(paste0(gagesii_dir,'/conterm_basinid.txt')) %>%
    st_as_sf(coords = c('LNG_GAGE', 'LAT_GAGE'), crs = 4269) %>% st_transform(5070)
  
  goi_info <- filter(all_sites, STAID == goi)
  diff_info <- filter(diff_frame, goi_id == goi) %>%
    select(STAID = target_id, diffmetric, ranking) %>%
    left_join(all_sites)
  
  plot <- ggplot() +
    geom_sf(data = states) +
    geom_sf(data = diff_info, aes(color = diffmetric, geometry = geometry)) +
    geom_sf(data = goi_info, color = 'red') +
    scale_color_viridis_b(, breaks = quantile(diff_info$diffmetric, probs = seq(0,1,by = 0.1)))
  return(plot)
}
