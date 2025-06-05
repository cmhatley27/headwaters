library(tidyverse)

# Get upstream/downstream connections from John Hammond's table  -----------

# Many gages had the leading 0s cut off from their IDs, this function
# pastes them back on.
id_fixinator <- function(x) {
  x_char <- as.character(x)
  wrong_ids <- str_length(x_char) == 7
  x_char[wrong_ids] <- paste0('0',x_char[wrong_ids])
  
  #a few IDs with abnormal lengths slipped through cracks:
  manual_fixes <- c('11055566', '11230695', '211139110', '214291555', '214676115')
  x_char[x_char %in% manual_fixes] <- paste0('0',x_char[x_char %in% manual_fixes])
  return(x_char)
}

# Load in John's table
nhd_connections <- read_csv('data/shapefiles/nhd/nhd_connections.csv') %>%
  filter(!is.na(provider_id)) %>%
  mutate(across(contains('provider_id'), id_fixinator)) %>%
  filter(!str_detect(provider_id, 'e'))

# column 'updown' tells the relationship between the gage listed in 'provider_id'
# to the gage listed in 'origin_gage_provider_id'. In row 1, for example, gage
# 01017000 is downstream ('downmain') of gage 01015800. In row 4, gage
# 01016500 is upstream of gage 01017000, upstream along a tributary
# rather than the mainstem ('uptrib'). 

# You can then get the distance between each pair of gages from 'distance_km' or
# the ratio of drainage areas using 'origin_to_compared_gage_DA_ratio_XXX' (I 
# use the one that ends with _NWIS)
