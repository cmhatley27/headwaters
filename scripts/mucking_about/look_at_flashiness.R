# Load data and calculate flashiness signature ----------------------------
library(tidyverse)
library(trend)
source(file.path('scripts','functions','read_gages.R'))
source(file.path('scripts','functions','read_spatial.R'))
source(file.path('scripts','functions','signature_functions.R'))

qs <- read_gages(folder = '1970_2022')
gage_info <- read_gage_info(folder = '1970_2022')

flash_summary <- qs %>%
  group_by(site_no, wateryear) %>%
  summarise(rb_flash = sig_rb_flash(q))
  

# Plot annual flashiness values for 4 random gages ------------------------
r_int <- floor(runif(4,1,nrow(gage_info)))
gage_sel <- gage_info$site_no[r_int]

ggplot(data = subset(flash_summary, site_no %in% gage_sel), aes(x = wateryear, y = rb_flash)) +
  geom_point() +
  facet_wrap(vars(site_no))


# Show gage locations -----------------------------------------------------
ggplot(data = subset(gage_info, site_no %in% gage_sel), aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point() +
  geom_text(aes(label = site_no), nudge_x = 4, nudge_y = 0.5)

  
# Calculate trends --------------------------------------------------------
sig_level <- 0.05
flash_trends <- flash_summary %>%
  group_by(site_no) %>%
  summarise(stat = mk.test(rb_flash)$estimates['S'],
            sig = mk.test(rb_flash)$p.value) %>%
  mutate(sig_dir = factor(ifelse(sig < sig_level,stat/abs(stat),NA))) %>%
  left_join(gage_info)

sum(!is.na(flash_trends$sig_dir))
sum(!is.na(flash_trends$sig_dir))/nrow(flash_trends)

ggplot(data = flash_trends, aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point(aes(color = sig_dir)) +
  scale_color_manual(values = c('red', 'blue'))

