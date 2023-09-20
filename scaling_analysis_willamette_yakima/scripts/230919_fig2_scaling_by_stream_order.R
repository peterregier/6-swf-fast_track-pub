## This script replicates figures from original Fco analysis using the new dataset
## as well as some exploration of how figures might look for new analyses
##
## Peter Regier
## 2023-09-19
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

require(pacman)

require(tidyverse)


# 2. Read in data --------------------------------------------------------------

df <- read_csv("data/new_data/nhd_stream_annual_resp.csv")

colnames(df)


# 3. Check plots in "script_scaling_plots.R" on repo 6 -------------------------

## L253: guerrero_etal_23_scaling_local_respiration_rates.svg
## This largely tracks what Fco found, although order 7 look a little different
ggplot(df, aes(as.factor(stream_order), totco2g_m2_day)) + 
  geom_boxplot() + 
  facet_wrap(~basin, nrow = 1) + 
  theme_minimal() + 
  scale_y_log10()

## Skipped head to see how good things look and they look GOOD!
ggplot(data = df,
       aes(x = accm_wshd_area_km2,
           y = accm_totco2g_day / accm_stream_area_m2))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Local respiration rates"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  facet_wrap(~basin, ncol = 2)



