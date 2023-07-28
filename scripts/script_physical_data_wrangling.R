################################################################################
# SCALING WATERSHED FUNCTION: PHYSICAL DATA WRANGLING
################################################################################

#Author: Francisco J. Guerrero
gc()
# Loading required packages: 

librarian::shelf(tidyverse,
                 utils,
                 leaflet,
                 sp,
                 sf,
                 nhdplusTools,
                 GGally,
                 htmltools,
                 foreign)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"
local_metadata <- "./metadata"

# Loading source data

enh_nhdplus21_dat <- read_csv(paste(source_data,"enhanced_nhdplus_21","data","enhanced_nhdplus21_ywrb.csv", sep = '/'),
                                     show_col_types = FALSE)

bsn_chr_nhdplus21_dat <- read_csv(paste(source_data,"select_basin_charact_nhdplus_21","data","select_basin_charact_nhdplus_21.csv", sep = '/'),
                                  show_col_types = FALSE)

anc_attb_nhdplus21_dat <- read_csv(paste(source_data,"ancillary_hydro_attributes_nhdplus_21","data","ancillary_hydro_attributes_nhdplus_21.csv", sep = '/'),
                                   show_col_types = FALSE)

rcm_22_model_dat <- read_csv(paste(source_data,"rcm_2022_model_data","data","merged_nexss_inputs.csv", sep = '/'),
                             show_col_types = FALSE)

nsi_ssn_ntwk_dat <- st_transform(st_read(paste(source_data,"nsi_ssn_network","data","nsi_network_ywrb.shp",sep = "/")),4326)

med_bed_part_dat <- read_csv(paste(source_data,"rcm_2022_hyporheic_provisional","data","current20_hyporheic_pnw_data.csv", sep = '/'),
                             show_col_types = FALSE)

# Checking stream network connectivity for RCM model data

rcm_22_connectivity <- rcm_22_model_dat %>% 
  select(comid,
         tocomid) %>% 
  merge(.,
        enh_nhdplus21_dat %>% 
          select(comid,
                 huc_4),
        all.x = TRUE) %>%
  mutate(basin = if_else(huc_4 == "1703",
                         "yakima",
                         "willamette")) %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
rcm_22_connectivity  
# Connectivity index for Yakima River Basin is 95.02%
# Connectivity index for Willamette River Basin is 97.50%


# We are going to merge the RCM 2022 model data with the curated National Stream
# Internet database 

nsi_rcm_ntwk_dat <- nsi_ssn_ntwk_dat %>% 
  rename(comid = COMID) %>% 
  merge(.,
        rcm_22_model_dat,
        by = "comid",
        all.x = TRUE)

# The resulting dataset has 31 extra rows compared with rcm_22_model_dat

summary(filter(nsi_rcm_ntwk_dat,DUP_COMID==1))

# These extra rows correspond to the duplicated COMIDs that the nsi adds to the
# original NHDPlus 2.1 dataset to meet the requirements for spatial statistical
# analysis with the SSN package. The SSN package allows for spatial hypothesis
# testing (i.e. models) that incorporate spatial autocorrelation measured based 
# on distances along the stream network (instead of only euclidian distances)

# Let's take a look at the missing values for some of the hyporheic flow data

# Fluvial network
leaflet(nsi_rcm_ntwk_dat) %>% 
  addPolylines(weight = 2) %>% 
  addPolylines(data = filter(nsi_rcm_ntwk_dat,is.na(logRT_lateral_hz_s) == TRUE),
               color = "magenta",
               opacity = 1,
               weight = 9) %>% 
  addProviderTiles("Esri.WorldImagery")

# Watershed areas = 0
# According to Blodgett (2023, pers. comms.) there is a number of flowlines with
# both catchment and watershed areas = 0. These flowlines are mostly disconnected 
# from the stream newtork. Let's double check the location of these flowlines 
# before we remove them from the dataset. Some of these flowlines seem to also 
# appear in the nsi_ssn_ntwk_dat. Let's check

summary(filter(nsi_rcm_ntwk_dat,TotDASqKM ==0))

# We find 43 comids with TotDASqKM = 0. However, these comids have duplicated 
# length values corresponding to original stream lengths. This could be an 
# indication that these comids may correspond to connecting lines that maintain
# network integrity

leaflet(nsi_rcm_ntwk_dat) %>% 
  addPolylines(weight = 2) %>% 
  addPolylines(data = filter(nsi_rcm_ntwk_dat,TotDASqKM ==0),
               color = "magenta",
               opacity = 1,
               weight = 9) %>% 
  addProviderTiles("Esri.WorldImagery")

# After visual inspection, these comids with zero values for watershed area could
# be removed without any lost in network connectivity. Let's do so, and veryfy
# with the network connectivity calculation

nsi_rcm_ntwk_connectivity <- as.data.frame(nsi_rcm_ntwk_dat %>% 
  select(-geometry))%>% 
  filter(TotDASqKM > 0) %>%
  filter(DUP_COMID == 0) %>% # We need to remove duplicate COMIDs for calculate_arbolate_sum to work correctly
  mutate(basin = if_else(HUC4 == "1703",
                         "yakima",
                         "willamette")) %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid)*100) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
nsi_rcm_ntwk_connectivity 

# Connectivity indexes remain the same for both Willamette and Yakima River Basins

# Physical Characteristics and Hydrology

# We use the enhanced NHDPlus V.2. as the reference dataset for COMIDs (Blodgett_23_Network_Attributes)

# Merging data 

phys_dat <- enh_nhdplus21_dat %>% 
  select(comid,
         tocomid,
         lengthkm,#flowline length
         reachcode,
         areasqkm,#catchment area
         totdasqkm,#watershed area
         arbolatesu,#total upstream flowline length
         hydroseq,
         streamorde,
         slope,#flowline slope
         slopelenkm,
         ftype,
         rpuid,
         vpuid,
         roughness,
         huc_4)%>%
  rename(reach_length_km = lengthkm,
         ctch_area_km2 = areasqkm,
         wshd_area_km2 = totdasqkm,
         tot_stream_length_km = arbolatesu,
         stream_order = streamorde,
         reach_slope = slope,
         reach_slope_length_km = slopelenkm,
         reach_type = ftype,
         huc_region_raster_id = rpuid,
         huc_2_region_id = vpuid,
         huc_4_subregion_id = huc_4) %>%
  mutate(basin = if_else(huc_4_subregion_id == 1703,
                         "yakima",
                         "willamette")) %>% 
  mutate(reach_slope = reach_slope/1000) %>%
  merge(.,
        bsn_chr_nhdplus21_dat %>% 
          select(comid,
                 sinuosity,
                 TOT_STRM_DENS,
                 TOT_BASIN_SLOPE,
                 TOT_ELEV_MIN,
                 TOT_ELEV_MAX,
                 TOT_ELEV_MEAN,
                 CAT_STRM_DENS,
                 CAT_BASIN_SLOPE,
                 CAT_ELEV_MIN,
                 CAT_ELEV_MAX,
                 CAT_ELEV_MEAN,
                 ACC_BASIN_AREA,
                 ACC_BASIN_SLOPE,
                 ACC_ELEV_MIN,
                 ACC_ELEV_MAX,
                 ACC_ELEV_MEAN,
                 ACC_STREAM_LENGTH,
                 ACC_STREAM_SLOPE,
                 ACC_STRM_DENS,
                 BANKFULL_WIDTH,
                 BANKFULL_DEPTH,
                 BANKFULL_XSEC_AREA) %>% 
          rename(wshd_stream_dens = TOT_STRM_DENS,
                 wshd_basin_slope = TOT_BASIN_SLOPE,
                 wshd_min_elevation_m = TOT_ELEV_MIN,
                 wshd_max_elevation_m = TOT_ELEV_MAX,
                 wshd_avg_elevation_m = TOT_ELEV_MEAN,
                 ctch_stream_dens = CAT_STRM_DENS,
                 ctch_basin_slope = CAT_BASIN_SLOPE,
                 ctch_min_elevation_m = CAT_ELEV_MIN,
                 ctch_max_elevation_m = CAT_ELEV_MAX,
                 ctch_avg_elevation_m = CAT_ELEV_MEAN,
                 accm_basin_area_km2 = ACC_BASIN_AREA,
                 accm_basin_slope = ACC_BASIN_SLOPE,
                 accm_min_elevation_m = ACC_ELEV_MIN,
                 accm_max_elevation_m = ACC_ELEV_MAX,
                 accm_avg_elevation_m = ACC_ELEV_MEAN,
                 accm_stream_length_km = ACC_STREAM_LENGTH,
                 accm_stream_slope = ACC_STREAM_SLOPE,
                 accm_stream_dens = ACC_STRM_DENS,
                 bnkfll_width_m = BANKFULL_WIDTH,
                 bnkfll_depth_m = BANKFULL_DEPTH,
                 bnkfll_xsec_area_m2 = BANKFULL_XSEC_AREA),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        anc_attb_nhdplus21_dat %>% 
          select(comid,
                 fromnode,
                 tonode,
                 precipv,
                 tempv,
                 runoffv,
                 maflowucfs,
                 mavelufps,
                 incflwucfs) %>% 
          rename(from_node = fromnode,
                 to_node = tonode,
                 precipt = precipv,
                 temp = tempv,
                 runoff = runoffv,
                 mean_ann_flow = maflowucfs,
                 mean_ann_vel = mavelufps,
                 inc_flw = incflwucfs) %>% 
          mutate(mean_ann_pcpt_mm = precipt/100,
                 mean_ann_temp_dc = temp/100,
                 mean_ann_runf_mm = runoff,
                 mean_ann_flow_m3s = mean_ann_flow*0.0283,
                 mean_ann_vel_ms = mean_ann_vel*0.3048,
                 inc_flw_m3s = inc_flw*0.0283) %>% 
          select(comid,
                 from_node,
                 to_node,
                 mean_ann_pcpt_mm,
                 mean_ann_temp_dc,
                 mean_ann_runf_mm,
                 mean_ann_flow_m3s,
                 mean_ann_vel_ms,
                 inc_flw_m3s),
        by = "comid",
        all.x = TRUE)
          

# Reordering variables within the data set for easier sub-setting
phys_dat_ro <- phys_dat %>% 
  select(comid,
         tocomid,
         reachcode,
         hydroseq,
         from_node,
         to_node,
         huc_2_region_id,
         huc_region_raster_id,
         huc_4_subregion_id,
         basin,
         wshd_stream_dens,
         wshd_basin_slope,
         wshd_min_elevation_m,
         wshd_max_elevation_m,
         wshd_avg_elevation_m,
         ctch_stream_dens,
         ctch_basin_slope,
         ctch_min_elevation_m,
         ctch_max_elevation_m,
         ctch_avg_elevation_m,
         accm_basin_area_km2,
         accm_basin_slope,
         accm_min_elevation_m,
         accm_max_elevation_m,
         accm_avg_elevation_m,
         accm_stream_slope,
         accm_stream_dens,
         mean_ann_pcpt_mm,
         mean_ann_temp_dc,
         mean_ann_runf_mm,
         ctch_area_km2,
         wshd_area_km2,
         stream_order,
         reach_type,
         reach_length_km,
         tot_stream_length_km,
         reach_slope_length_km,
         reach_slope,
         roughness,
         sinuosity,
         bnkfll_width_m,
         bnkfll_depth_m,
         bnkfll_xsec_area_m2,
         mean_ann_flow_m3s,
         mean_ann_vel_ms)

# Merging with RCM input variables from NEXSS model (version 2020)

nsi_rcm_phys_dat_0 <- nsi_rcm_ntwk_dat %>% 
  select(comid,
         DUP_COMID,
         DUP_ArSqKM,
         DUP_Length,
         logRT_vertical_hz_s,
         logRT_lateral_hz_s,
         logq_hz_vertical_m_div_s,
         logq_hz_vertical_m_div_s,
         logq_hz_lateral_m_div_s,
         logq_hz_lateral_m_div_s,
         logw_m,
         length_m) %>% 
  merge(.,
        phys_dat_ro,
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        med_bed_part_dat %>% 
          rename(d50_m = D50_m) %>% 
          mutate(d50_mm = d50_m * 1000) %>% 
          select(comid,
                 logK_m_div_s,
                 d50_mm,
                 d50_m),
        by = "comid",
        all.x = TRUE)

# Saving data files as data and as shapefile (to include geometry)

nsi_rcm_phys_dat <- nsi_rcm_phys_dat_0 %>% 
  sf::st_drop_geometry() %>% 
  as.data.frame()

# Saving as CSV file
write.csv(nsi_rcm_phys_dat,paste(local_data,"river_corridors_physical_hyporheic_char.csv", sep = '/'),
          row.names = FALSE)

# Saving as an st object

# This file will be a simplified version  of the dataset above to meet ESRI standards

nsi_rcm_phys_sto <- nsi_rcm_phys_dat_0 %>% 
  rename(log_rt_v_s = logRT_vertical_hz_s,
         log_rt_l_s = logRT_lateral_hz_s,
         log_qz_v_ms = logq_hz_vertical_m_div_s,
         log_qz_l_ms = logq_hz_lateral_m_div_s) %>% 
  select(comid,
         tocomid,
         DUP_ArSqKM,
         DUP_COMID,
         DUP_Length,
         basin,
         log_rt_v_s,
         log_rt_l_s,
         log_qz_v_ms,
         log_qz_l_ms,
         d50_m,
         geometry)

# Determine the maximum width required for the 'tocomid' field
max_tocomid_width <- max(nchar(as.character(nsi_rcm_phys_sto$tocomid)))

# Update the field width for 'tocomid' in 'nsi_rcm_phys_sto'
nsi_rcm_phys_sto$tocomid <- format(nsi_rcm_phys_sto$tocomid, width = max_tocomid_width)

# Specify the file path for saving the shapefile
output_file <- paste(local_data,"river_corridors_physical_hyporheic_geom.shp", sep = '/')

# Write the 'nsi_rcm_phys_sto' data frame to a shapefile
st_write(nsi_rcm_phys_sto, 
         dsn = output_file, 
         delete_dsn = TRUE, 
         overwrite_layer = TRUE, 
         delete_layer = TRUE)

data <- read_csv(paste(local_data,"river_corridors_physical_hyporheic_char.csv", sep = '/'),
                 show_col_types = FALSE)


p <- ggplot(data = data,
            aes(x = reach_slope,
                y = d50_m,
                color = as.factor(stream_order)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(basin~as.factor(stream_order), ncol = 8, nrow = 2)
p

d50_mod <- lm(log10(d50_m)~(log10(reach_slope)+log10(wshd_area_km2))*basin + stream_order,
              data = filter(data, wshd_area_km2 > 0 & reach_slope > 0),
              na.action = na.omit)

summary(d50_mod)

p <- ggplot(data = data,
            aes(x = roughness,
                y = d50_m,
                color = as.factor(stream_order)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(basin~as.factor(stream_order), ncol = 8, nrow = 2)
p

reg_data <- data %>% 
  filter(wshd_area_km2 > 0 & reach_slope > 0) %>% 
  filter(bnkfll_width_m > 0 & mean_ann_flow_m3s > 0)

d50_mod_jl <- lm(log10(d50_m)~(log10(reach_slope)+log10(bnkfll_width_m)+log10(mean_ann_flow_m3s))*basin + stream_order,
              data = reg_data,
              na.action = na.omit)

summary(d50_mod_jl)

reg_data <- reg_data %>% 
  mutate(q = 3.004*mean_ann_flow_m3s^0.426,
         w = bnkfll_width_m,
         s = 100*reach_slope^-0.153,
         qs = q*s,
         ln = log(w/qs),
         p = ln/-0.002,
         d50_lj = exp(p),
         d50_lj2 = (bnkfll_width_m/ (3.004 * mean_ann_flow_m3s^0.426 * reach_slope^-0.153)) ^ (1 / -0.002))
         
summary(reg_data)


d50_dat <- reg_data %>% 
  filter(is.na(d50_m)==FALSE) %>% 
  mutate(bnkfll_lj = 3.004*mean_ann_flow_m3s^0.426*reach_slope^-0.153*d50_m^-0.002)
summary(d50_dat)


p <- ggplot(data = d50_dat,
            aes(x = bnkfll_width_m,
                y = bnkfll_lj,
                color = log(reach_slope)))+
  geom_point()+
  geom_abline()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p

d50_mod2 <- lm(log(d50_m)~(log(bnkfll_width_m)+log(reach_slope)+log(mean_ann_flow_m3s)+log(wshd_area_km2))*basin+stream_order,
               data = d50_dat,
               na.action = na.omit)

summary(d50_mod2)
  

d50_dat <- d50_dat %>% 
  mutate(p_d50_m = exp(predict.lm(d50_mod2,.)))
summary(d50_dat)

p <- ggplot(data = d50_dat,
            aes(x = d50_m,
                y = p_d50_m))+
  geom_point()+
  geom_abline()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p

reg_data <- reg_data %>% 
  mutate(d50_lj = bnkfll_depth_m^-.05*reach_slope^0.765*mean_ann_flow_m3s^0.213,
         d50_m_lj2 = bnkfll_depth_m^-500*reach_slope^76.5*mean_ann_flow_m3s^-213)


summary(reg_data)

p <- ggplot(data = reg_data,
            aes(x = d50_m,
                y = d50_lj,
                color =as.factor(stream_order)))+
  geom_point()+
  geom_abline()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p