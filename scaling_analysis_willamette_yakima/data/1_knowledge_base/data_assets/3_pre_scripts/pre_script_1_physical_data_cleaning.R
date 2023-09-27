################################################################################
# SCALING WATERSHED FUNCTION: DATA CLEANING - PART I WATERSHED & CATCHMENT AREAS
################################################################################

# IMPORTANT: This script is provisional! Final data cleaning script is contingent
# upon matching between NHDPlus 2.1 and NHDPlus HR

#Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 quantreg,
                 gginnards,
                 nhdplusTools,
                 leaflet,
                 sp,
                 sf)

# Local Import-Export
raw_data <- "raw"
processed_data <- "processed"

phys_sni_dat_ro <- read_csv(paste(raw_data,"230620_ord_basin_sni_hydrogeom_pnw.csv", sep = '/'),
                        show_col_types = FALSE)

# Exploring the data via summary

summary(phys_sni_dat_ro)

# It seems that there are several datapoints with watershed area = 0

wsd0_dat <- filter(phys_sni_dat_ro,wshd_area_km2==0) # 43 observations

summary(wsd0_dat)

# These data all correspond to first order streams, so we remove them from the analysis

phys_sni_dat_mod1 <- filter(phys_sni_dat_ro,wshd_area_km2>0)

summary(phys_sni_dat_mod1)

# Next set of conflicting data catchment areas = 0

ctc0_dat <- filter(phys_sni_dat_mod1,ctch_area_km2==0) # 89 observations

summary(ctc0_dat)

# It seems to contain data across multiple river orders. We will remove first order 
# streams first

phys_sni_dat_mod2 <- phys_sni_dat_mod1 %>% 
  mutate(ctch_select = if_else(ctch_area_km2 == 0 & stream_order == 1,
                               "out",
                               "keep")) %>% 
  filter(.,ctch_select=="keep") %>% 
  select(-ctch_select)

# High order reaches with catchment areas = 0
ho_ctc0_dat <- filter(phys_sni_dat_mod2, ctch_area_km2 == 0) # 52 observations

summary(ho_ctc0_dat)

# Verifying that the high order reaches 
p <- ggplot(data = ho_ctc0_dat,
            aes(x = wshd_area_km2,
                y = tot_stream_length_km,
                color = as.factor(stream_order)))+
  geom_point()+
  scale_y_log10()+
  scale_x_log10()+
  facet_wrap(~basin,ncol = 2)
p


son_etal_dat <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv", sep = '/'),
                         show_col_types = FALSE)


ntwk_dat <- phys_dat_ro %>% 
  merge(.,
        son_etal_dat %>% 
          filter(.,time_type=="annual"),
        by = "comid",
        all.x = TRUE) %>% 
  filter(basin!="ipswich")

write.csv(ntwk_dat,paste(raw_data,"230620_guerrero_etal_network_swf.csv", sep = "/"),
          row.names = FALSE)

# Exploring the data via summary

summary(phys_dat_ro)

# We find several variables with zero or missing values.These flow lines do not have a catchment area 
# associated to them because they are not connected to the network (Blodget, comms. pers.). So we will remove thase lines from the 
# dataset. The same applies to flowlines with catchment areas = 0

# We will filter our data for stream and rivers only and check whether this takes care of many
# NAs


phys_dat_mod1 <- phys_dat_ro %>% 
  filter(reach_type!="CanalDitch" & 
           reach_type!="Connector") %>% 
  filter(reach_type!="Pipeline")

summary(phys_dat_mod1)

phys_dat_mod2 <- phys_dat_mod1 %>% 
  # filter(wshd_area_km2!=0 & 
  #          ctch_area_km2!=0) 

summary(phys_dat_mod2) 
  
write.csv(phys_dat_mod2,paste(raw_data,"230620_phys_dat_mod_2.csv", sep = "/"),
          row.names = FALSE)


# Summary

# The dataset "230430_ord_basin_hydrogeom_yrb_wrb.csv" had two modifications:

# 1. Filtering out artificial channels and features (4420 datapoints)

# 2. Removal of catchment/watershed area data with zero values (194 datapoints)



###############################################################################
# Surface area scaling

stream_sa_dat <- phys_dat_mod2 %>% 
  select(basin,
         comid,
         tocomid,
         ctch_area_km2,
         wshd_area_km2,
         mean_ann_flow_m3s,
         reach_type,
         reach_length_km,
         bnkfll_width_m) %>% 
  mutate(stream_area_m2 = reach_length_km*1000*bnkfll_width_m) %>% 
  mutate(wilk_bnkfll_width_m = if_else(wshd_area_km2<5,
                                       2.18*wshd_area_km2^0.191,
                                       if_else(wshd_area_km2<336,
                                               1.41*wshd_area_km2^0.462,
                                               if_else(wshd_area_km2>336,
                                                       7.18*wshd_area_km2^0.183,NA)))) %>% 
  mutate(wilk_stream_area_m2 = reach_length_km*1000*wilk_bnkfll_width_m)


accm_dat <- stream_sa_dat %>% 
  group_by(basin) %>% 
  select(comid,
         tocomid,
         basin,
         wshd_area_km2,
         mean_ann_flow_m3s,
         reach_type,
         stream_area_m2,
         reach_length_km,
         ctch_area_km2,
         wilk_stream_area_m2,
         bnkfll_width_m) %>% 
  mutate(across(stream_area_m2:bnkfll_width_m, ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                                toID = tocomid,
                                                                                length = .x))) %>% 
           set_names(paste0("accm_", names(select(., stream_area_m2:bnkfll_width_m))))) 


p <- ggplot(data = accm_dat,
            aes(x = wshd_area_km2,
                y = accm_ctch_area_km2,
                color = reach_type))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(reach_type~basin,ncol = 3)
p

p <- ggplot(data = accm_dat,
            aes(x = accm_ctch_area_km2,
                y = accm_wilk_stream_area_m2,
                color = reach_type))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(reach_type~basin,ncol = 3)
p


acm_ntwk_dat <- ntwk_dat %>% 
  filter(is.na(logtotco2g_m2_day)==FALSE) %>% 
  filter(reach_type!="CanalDitch" & 
           reach_type!="Connector") %>% 
  filter(reach_type!="Pipeline") %>% 
  filter(reach_type!="CanalDitch" & 
           reach_type!="Connector") %>% 
  filter(reach_type!="Pipeline") %>% 
  mutate(stream_area_m2 = reach_length_km*1000*bnkfll_width_m) %>% 
  mutate(tot_co2g_day = logtotco2g_m2_day^10 *stream_area_m2) %>% 
  select(comid,
         tocomid,
         basin,
         wshd_area_km2,
         mean_ann_flow_m3s,
         reach_type,
         stream_area_m2,
         tot_co2g_day,
         reach_length_km,
         ctch_area_km2,
         bnkfll_width_m) %>% 
  mutate(across(stream_area_m2:bnkfll_width_m, ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                                   toID = tocomid,
                                                                                   length = .x))) %>% 
           set_names(paste0("accm_", names(select(., stream_area_m2:bnkfll_width_m))))) 


p <- ggplot(data = acm_ntwk_dat,
            aes(x = wshd_area_km2,
                y = accm_tot_co2g_day/wshd_area_km2,
                color = reach_type))+
  geom_point()+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(~basin,ncol = 2)
p


