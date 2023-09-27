################################################################################
# SCALING WATERSHED FUNCTION: DATA CLEANING - PART I WATERSHED & CATCHMENT AREAS
################################################################################

# IMPORTANT: This script is provisional! Final data cleaning script is contingent
# upon matching between NHDPlus 2.1 and NHDPlus HR

#Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 graphics,
                 nhdplusTools)

# Local Import-Export
raw_data <- "raw"
processed_data <- "processed"

phys_dat_ro <- read_csv(paste(raw_data,"230623_ord_basin_sni_nxss_hydrogeom_pnw.csv", sep = '/'),
                        show_col_types = FALSE)

son_etal_dat <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv", sep = '/'),
                         show_col_types = FALSE)

# Flagging discrepancies across datasets (sciencebase vs. gomz23_dat)

# Calculating differences to identify flagging thresholds. 

phys_dat_diff <- phys_dat_ro %>% 
  select(comid,
         basin,
         wshd_area_km2,
         wshd_area_km2_g,
         reach_length_km,
         reach_length_km_g,
         stream_order,
         stream_order_g,
         sinuosity,
         sinuosity_g,
         reach_slope,
         slope_g,
         stream_order,
         stream_order_g,
         bnkfll_width_m,
         bnkfll_width_m_g,
         bnkfll_depth_m,
         bnkfll_depth_m_g,
         mean_ann_flow_m3s,
         mean_ann_flow_m3s_g,
         mean_ann_vel_ms,
         mean_ann_vel_ms_g) %>% 
  mutate(wsa_diff = abs(wshd_area_km2 - wshd_area_km2_g),
         lgt_diff = abs(reach_length_km - reach_length_km),
         ord_diff = abs(stream_order - stream_order_g),
         sns_diff = abs(sinuosity - sinuosity_g),
         slp_diff = abs(reach_slope - slope_g),
         ord_diff = abs(stream_order - stream_order_g),
         bfw_diff = abs(bnkfll_width_m - bnkfll_width_m_g),
         bfd_diff = abs(bnkfll_depth_m - bnkfll_depth_m_g),
         flw_diff = abs(mean_ann_flow_m3s - mean_ann_flow_m3s_g),
         vel_diff = abs(mean_ann_vel_ms - mean_ann_vel_ms_g))

phys_dat_ro <- phys_dat_ro %>%
  mutate(wsa_flag = if_else(abs(wshd_area_km2 - wshd_area_km2_g) > 0.1, "flag", "ok"),
         rch_lgth_flag = if_else(abs(reach_length_km - reach_length_km) > 0, "flag", "ok"),
         order_flag = if_else(abs(stream_order - stream_order_g) > 0, "flag", "ok"),
         sinous_flag = if_else(abs(sinuosity - sinuosity_g) > 0.005, "flag", "ok"),
         slope_flag = if_else(abs(reach_slope - slope_g) > 0, "flag", "ok"),
         bkf_w_flag = if_else(abs(bnkfll_width_m - bnkfll_width_m_g) > 0.1, "flag", "ok"),
         bkf_d_flag = if_else(abs(bnkfll_depth_m - bnkfll_depth_m_g) > 0.1, "flag", "ok"),
         flow_flag = if_else(abs(mean_ann_flow_m3s - mean_ann_flow_m3s_g) > 12, "flag", "ok"),
         vel_flag = if_else(abs(mean_ann_vel_ms - mean_ann_vel_ms_g) > 0.01, "flag", "ok")) %>%
  rowwise() %>%
  mutate(any_flag = if_else(any(c_across(wsa_flag:vel_flag) == "flag"), "flag", "ok"))

summary(filter(phys_dat_ro,wsa_flag=="flag")) # 193 flags
summary(filter(phys_dat_ro,rch_lgth_flag=="flag")) # No flags
summary(filter(phys_dat_ro,order_flag=="flag")) # 49 flags
summary(filter(phys_dat_ro,sinous_flag=="flag")) # No flags
summary(filter(phys_dat_ro,slope_flag=="flag")) # 16551 flags
summary(filter(phys_dat_ro,bkf_w_flag=="flag")) # 16550 flags
summary(filter(phys_dat_ro,bkf_d_flag=="flag")) # 8623 flags
summary(filter(phys_dat_ro,flow_flag=="flag")) # 697 flags
summary(filter(phys_dat_ro,vel_flag=="flag")) # 8028 flags

# Visual inspection

wsa <- ggplot(data = phys_dat_ro,
              aes(x = wshd_area_km2,
                  y = wshd_area_km2_g,
                  color = wsa_flag)) +
  geom_abline() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~basin, ncol = 2)
wsa

# 167 datapoints flagged for "overestimation" of watershed area in gomz23_dat

ord <- ggplot(data = phys_dat_diff,
              aes(x = stream_order,
                  y = stream_order_g,
                  color = basin)) +
  scale_x_continuous(limits = c(1,9),
                     breaks = c(1,2,3,4,5,6,7,8,9)) +
  scale_y_continuous(limits = c(1,9),
                     breaks = c(1,2,3,4,5,6,7,8,9)) +
  geom_abline() +
  geom_point() +
  facet_wrap(~basin, ncol = 2) +
  theme(legend.position = "none")
ord 

slp <- ggplot(data = phys_dat_diff,
              aes(x = reach_slope,
                  y = slope_g,
                  color = basin)) +
  geom_abline() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~basin, ncol = 2)
slp

# Slopes in gomz23_dat are in m/km not m/m. Flags can be removed and keep the original data. 

bfw <- ggplot(data = phys_dat_diff,
              aes(x = bnkfll_width_m,
                  y = bnkfll_width_m_g,
                  color = basin)) +
  geom_abline() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~basin, ncol = 2)
bfw

# There is a correction factor applied to smaller streams. The effects of such correction are
# clear in the willamette but look more complex in the yrb. 

bfd <- ggplot(data = phys_dat_diff,
              aes(x = bnkfll_depth_m,
                  y = bnkfll_depth_m_g,
                  color = basin)) +
  geom_abline() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~basin, ncol = 2)
bfd

# Similar effects of the correction applied, but in this case, gomz23_dat depths are larger. 

maf <- ggplot(data = phys_dat_diff,
              aes(x = mean_ann_flow_m3s,
                  y = mean_ann_flow_m3s_g,
                  color = basin)) +
  geom_abline() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~basin, ncol = 2)+
  theme(legend.position = "none")
maf

# 372 locations for which differences in mean annual flow are over 12 m3/s, particularly in 
# areas with large discharge. We could keep the usgs data in this case. 

mav <- ggplot(data = phys_dat_diff,
              aes(x = mean_ann_vel_ms,
                  y = mean_ann_vel_ms_g,
                  color = as.factor(stream_order_g))) +
  geom_abline() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~basin, ncol = 2)+
  theme(legend.position = "none")
mav
