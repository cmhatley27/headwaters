# Load libraries and data --------------------------------------------
library(tidyverse)
library(trend)
source(file.path('scripts','functions','read_gages.R'))
source(file.path('scripts','functions','read_spatial.R'))
source(file.path('scripts','functions','signature_functions.R'))

qs <- read_gages(folder = '1981_2022', subset = FALSE, nsub = 9, set.seed = FALSE)
gage_info <- read_gage_info(folder = '1981_2022')

bfi_summary <- qs %>%
  group_by(site_no) %>%
  summarise(bfi = sig_bfi(q,bf)) %>%
  left_join(gage_info)

bfi_annual <- qs %>%
  group_by(site_no, wateryear) %>%
  summarise(bfi = sig_bfi(q,bf)) %>%
  left_join(gage_info)


# Plot baseflow series for 4 random gages ---------------------------------
r_int <- floor(runif(4,1,nrow(gage_info)))
gage_sel <- gage_info$site_no[r_int]
# gage_sel <- '14314500'

plot_dat <- qs %>%
  filter(site_no %in% gage_sel) %>%
  pivot_longer(cols = c(q, bf), names_to = 'series', values_to = 'q')
year_sel <- 2006:2007
ggplot(data = subset(plot_dat, wateryear %in% year_sel), aes(x = date, y = q, color = series, fill = series)) +
  geom_area(position = 'identity') +
  scale_color_manual(breaks = c('q', 'bf'), values = c('black', 'red')) +
  scale_fill_manual(breaks = c('q', 'bf'), values = c(alpha('white',0), alpha('red', 0.5))) +
  facet_wrap(vars(site_no), scales = 'free_y')

# Show gage locations -----------------------------------------------------
ggplot(data = subset(gage_info, site_no %in% gage_sel), aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point() +
  geom_text(aes(label = site_no), nudge_x = 4, nudge_y = 0.5)

# Look at full-record BFI --------------------------------------------------
ggplot(data = bfi_summary, aes(x = dec_long_va, y = dec_lat_va, color = bfi)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point() +
  scale_color_viridis_c()


# Plot annual BFI series for 4 random gages -------------------------------
r_int <- floor(runif(4,1,nrow(gage_info)))
gage_sel <- gage_info$site_no[r_int]

ggplot(data = subset(bfi_annual, site_no %in% gage_sel), aes(x = wateryear, y = bfi)) +
  geom_point() +
  facet_wrap(vars(site_no))



# Calculate annual BFI trends --------------------------------------------------
sig_level <- 0.05
bfi_trends <- bfi_annual %>%
  #filter out years where BFI is NaN, caused by 0 annual Q. Doesn't make sense
  #to assign these years a BFI of either 0 or 1 so I'm just removing them.
  filter(!is.na(bfi)) %>%
  group_by(site_no) %>%
  summarise(stat = mk.test(bfi)$estimates['S'],
            sig = mk.test(bfi)$p.value) %>%
  mutate(sig_dir = factor(ifelse(sig < sig_level,stat/abs(stat),NA))) %>%
  left_join(gage_info)

sum(!is.na(bfi_trends$sig_dir))
sum(!is.na(bfi_trends$sig_dir))/nrow(bfi_trends)

ggplot(data = bfi_trends, aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point(aes(color = sig_dir)) +
  scale_color_manual(values = c('red', 'blue'))



