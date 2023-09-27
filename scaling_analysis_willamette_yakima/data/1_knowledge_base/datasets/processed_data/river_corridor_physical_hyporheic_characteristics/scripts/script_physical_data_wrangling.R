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

rcm_22_model_dat <- read_csv(paste(source_data,"rcm_2022_model_data","data","RF_filled_rcm_2022_model_data.csv", sep = '/'),
                             show_col_types = FALSE)

nsi_rcm_ntwk_dat <- st_transform(st_read(paste(source_data,"rcm_2022_model_data","data","shapefiles","river_corridors_respiration_geom.shp",sep = "/")),4326)

med_bed_part_dat <- read_csv(paste(source_data,"rcm_2022_hyporheic_provisional","data","current20_hyporheic_pnw_data.csv", sep = '/'),
                             show_col_types = FALSE)

# Checking stream network connectivity for RCM model data

rcm_22_connectivity <- rcm_22_model_dat %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = nhdplusTools::calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
rcm_22_connectivity  


# Connectivity index for Yakima River Basin is 95 %
# Connectivity index for Willamette River Basin is 97.40%


# Fluvial network
leaflet(nsi_rcm_ntwk_dat) %>% 
  addPolylines(weight = 2) %>% 
  addPolylines(data = filter(nsi_rcm_ntwk_dat,is.na(t_co2_gday) == TRUE),
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

nsi_rcm_phys_dat <- rcm_22_model_dat %>% 
  select(-c(basin)) %>% 
  merge(.,
        phys_dat_ro %>% 
          select(-tocomid),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        med_bed_part_dat %>% 
          rename(d50_m = D50_m) %>% 
          select(comid,
                 logK_m_div_s,
                 d50_m),
        by = "comid",
        all.x = TRUE) 

str(nsi_rcm_phys_dat)

#checking connectivity

connectivity_test <- nsi_rcm_phys_dat %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = nhdplusTools::calculate_arbolate_sum(data.frame(ID = comid,
                                                                          toID = tocomid,
                                                                          length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
connectivity_test 

# Saving as CSV file
write.csv(nsi_rcm_phys_dat,paste(local_data,"river_corridors_physical_hyporheic_char.csv", sep = '/'),
          row.names = FALSE)




