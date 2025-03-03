# Load data and calculate signatures ----------------------------
library(tidyverse)
library(trend)
source(file.path('scripts','functions','read_gages.R'))
source(file.path('scripts','functions','read_spatial.R'))
source(file.path('scripts','functions','signature_functions.R'))

qs <- read_gages(folder = '1981_2022')
gage_info <- read_gage_info(folder = '1981_2022')

sig_summary <- qs %>%
  group_by(site_no, wateryear) %>%
  summarise(totalQ = sig_total_Q(q),
            noflow = sig_no_flow_frac(q),
            bfi = sig_bfi(q, bf),
            flash = sig_rb_flash(q))

# Calculate trends --------------------------------------------------------
sig_level <- 0.05
sig_trends <- sig_summary %>%
  group_by(site_no) %>%
  summarise(stat_totalQ = mk.test(totalQ)$estimates['S'],
            sig_totalQ = mk.test(totalQ)$p.value,
            stat_noflow = mk.test(noflow)$estimates['S'],
            sig_noflow = mk.test(noflow)$p.value) %>%
  mutate(sig_dir_totalQ = factor(ifelse(sig_totalQ < sig_level,stat_totalQ/abs(stat_totalQ),NA)),
         sig_dir_noflow = factor(ifelse(sig_noflow < sig_level,stat_noflow/abs(stat_noflow),NA))) %>%
  left_join(gage_info)

sum(!is.na(sig_trends$sig_dir_totalQ))
sum(!is.na(sig_trends$sig_dir_totalQ))/nrow(sig_trends)

sum(!is.na(sig_trends$sig_dir_noflow))
sum(!is.na(sig_trends$sig_dir_noflow))/nrow(sig_trends)

ggplot(data = arrange(sig_trends, desc(is.na(sig_dir_totalQ))), aes(x = dec_long_va, y = dec_lat_va)) +
  geom_sf(data = states, inherit.aes = FALSE) +
  geom_point(aes(color = sig_dir_totalQ)) +
  scale_color_manual(values = c('red', 'blue'), name = 'Trend', labels = c('Decreasing','Increasing')) +
  ggtitle('Total Annual Discharge') +
  xlab('') +
  ylab('')
ggsave(file.path('figures','sig_maps','totalQ.png'), width = 6, height = 3, units = 'in')
