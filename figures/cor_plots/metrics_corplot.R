# libraries and data ------------------------------------------------------
library(tidyverse)
library(corrplot)
source('scripts/functions/utilities.R')
source('scripts/Theme+Settings.R')


hw_gage_info <- read_gage_info()
hw_dat <- read_csv('data/gages/metrics/metrics_window3.csv')

metrics_sel <- c('Q_mean', 'Q95', 'Q10', 'TotalRR', 'Q_frequency_high_3', 'Q_frequency_noflow',
                 'Q_totalduration_high_3', 'Q_totalduration_noflow',
                 'HFD_mean', 'HFI_mean', 'peakQ_timing', 'BFI', 'FlashinessIndex', 'FDC_slope',
                 'BaseflowRecessionK', 'Recession_a_Seasonality')

cors <- cor(select(hw_dat, all_of(metrics_sel)),
            use = 'pairwise.complete',
            method = 'spearman')


# plot --------------------------------------------------------------------
rownames(cors) <- c('Mean Annual Q', 'Q95', 'Q10', 'Runoff Ratio',
                    '# High-Flow Peaks', '# No-Flow Periods',
                    'High-Flow Duration', 'No-Flow Duration',
                    'Half-Flow Date', 'Half-Flow Interval', 'Peak Q Date',
                    'Baseflow Index', 'Flashiness Index', 'FDC Mid-Section Slope',
                    'Baseflow Recession Constant', 'Recession Seasonality')
colnames(cors) <- rownames(cors)

corrplot(cors,
         tl.col = 'black', tl.cex = 1.5,
         cl.cex = 1.5)
#export at 2400x1944



