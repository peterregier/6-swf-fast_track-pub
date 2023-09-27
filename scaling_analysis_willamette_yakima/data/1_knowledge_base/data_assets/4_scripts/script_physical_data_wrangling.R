################################################################################
# SCALING WATERSHED FUNCTION: DATA WRANGLING
################################################################################

#Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils)

# Local Import-Export
raw_data <- "raw"
processed_data <- "processed"

# Loading raw data


# NEXSS Input data
gomz23_dat <- read_csv(paste(raw_data,"HUC_17_nexss_input_base_variables.csv", sep = '/'),
                                     show_col_types = FALSE)

# National Stream Internet - Reference Hydrological Network
nagl17_dat <- read_csv(paste(raw_data,"230623_nis_network_ywrb.csv", sep = '/'),
                       show_col_types = FALSE)

# Original dataset citation
# Nagel, D., S. Wollrab, S. Parkes-Payne, E. Peterson, D. Isaak, and J. Ver Hoef. 2017. 
# National Stream Internet hydrography datasets for spatial-stream-network (SSN) analysis. 
# Rocky Mountain Research Station, U.S. Forest Service Data Archive, Fort Collins, CO.

# download script: script_nsi_network_nagl_17_rselenium_download.


# Physical Characteristics and Hydrology

# We use the enhanced NHDPlus V.2. as the reference dataset for COMIDs (Blodgett_23_Network_Attributes)
blgt23_dat <- read_csv(paste(raw_data,"230620_enhanced_nhdp_2_swf.csv", sep = '/'),
                      show_col_types = FALSE)

# Original dataset citation
#Blodgett, D.L., 2023, Updated CONUS river network attributes based on the E2NHDPlusV2 and NWMv2.1 
#networks (ver. 2.0, February 2023): U.S. Geological Survey data release,
#https://doi.org/10.5066/P976XCVT

#Moore, R.B., McKay, L.D., Rea, A.H., Bondelid, T.R., Price, C.V., Dewald, T.G., and Johnston, 
#C.M., 2019, User's guide for the national hydrography dataset plus (NHDPlus) high 
#resolution: U.S. Geological Survey Open-File Report 2019–1096, 66 p., 
#https://pubs.usgs.gov/of/2019/1096/ofr20191096.pdf


# Download script: script_nhdp2_blgt_23_enhanced_rselenium_download.R

# Physical characteristics of the river basins (Wieczeroek_21_Select_Attributes)
wczk21_dat <-  read_csv(paste(raw_data,"230620_swf_basin_characteristics.csv", sep = '/'),
                       show_col_types = FALSE)

# Original dataset citation
#Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes for 
#NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds 
#for the Conterminous United States-Select Basin Characteristics (ver. 3.0, January 2021): U.S. 
#Geological Survey data release, 
#https://www.sciencebase.gov/catalog/item/57976a0ce4b021cadec97890

#Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes for 
#NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds 
#for the Conterminous United States-Bankfull Hydraulic Geometry Related to Physiographic 
#Divisions (ver. 3.0, January 2021): U.S. Geological Survey data release, 
#https://www.sciencebase.gov/catalog/item/5cf02bdae4b0b51330e22b85

# Download script: script_nhdp2_wczk_21_basin_charct_rselenium_download.R


# Hydrological data (Schwarz_19_Ancillary_Attributes)
schz19_dat <- read_csv(paste(raw_data,"230620_main_nhdp_2_swf.csv", sep = '/'),
                      show_col_types = FALSE)

#Original dataset citation: 
#Schwarz, G.E., 2019, E2NHDPlusV2_us: Database of Ancillary Hydrologic Attributes 
#and Modified Routing for NHDPlus Version 2.1 Flowlines: U.S. Geological Survey data release, 
#https://doi.org/10.5066/P986KZEM.

#Download script: script_nhdp2_schz_19_rselenium_download.R


# Flowline slopes

# In the Enhanced Hydrologic Stream Network Based on the NHDPlus Medium resolution
# dataset (blg23_dat, in our case), the slopes were revised and re-calculated: 

# "NHDPlus slopes determined according to the original NHDPlusV2 method and the 
# revised method in relation to slopes measured at 2,846 sites indexed to NHDPlusV2 
# from the U.S. Environmental Protection Agency's Wadeable Streams Assessment
# and National River and Stream Assessment. WSA, Wadeable Streams Assessment; NRSA, 
# National River and Stream Assessment"

# The major update consisted in a better determination of the benchmark points, 
# mostly in headwater catchments, that could be used as the initial elevation, from 
# which the slopes would be determined in the downstream direction. For more info
# on the revised method go to: https://pubs.usgs.gov/sir/2019/5127/sir20195127.pdf

# comit and to_comid
# blgt_23 dataset also includes a Cleaned up network, added tocomid from 
# tonode/fromnode topology, and removed unwanted modifications from E2NHDPlusV2 and NWMv2.1.

# National Stream Internet - Reference Hydrological Network
nagl17_dat <-st_read(paste(raw_data,"shape_files","nis_reference","230623_nis_network_ywrb.shp",sep = '/'))

# Original dataset citation
# Nagel, D., S. Wollrab, S. Parkes-Payne, E. Peterson, D. Isaak, and J. Ver Hoef. 2017. 
# National Stream Internet hydrography datasets for spatial-stream-network (SSN) analysis. 
# Rocky Mountain Research Station, U.S. Forest Service Data Archive, Fort Collins, CO.

# download script: script_nsi_network_nagl_17_rselenium_download.

# Merging data 

phys_dat <- blgt23_dat %>% 
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
                         "yakima",if_else(huc_4_subregion_id==1709,
                         "willamette","ipswich"))) %>% 
  mutate(reach_slope = reach_slope/1000) %>%
  merge(.,
        wczk21_dat %>% 
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
        schz19_dat %>% 
          select(ComID,
                 FromNode,
                 ToNode,
                 PrecipV,
                 TempV,
                 RunOffV,
                 MAFlowUcfs,
                 MAVelUfps,
                 IncFlwUcfs) %>% 
          rename(comid = ComID,
                 from_node = FromNode,
                 to_node = ToNode,
                 precipt = PrecipV,
                 temp = TempV,
                 runoff = RunOffV,
                 mean_ann_flow = MAFlowUcfs,
                 mean_ann_vel = MAVelUfps,
                 inc_flw = IncFlwUcfs) %>% 
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

# Merge with SNI dataset

phys_sni_dat_ro <- nagl17_dat %>% 
  select(COMID,
         geometry) %>% 
  merge(.,
        phys_dat_ro,
        by.x = "COMID",
        by.y = "comid") %>% 
  rename(comid = COMID)


# NEXSS Input Data
gomz23_dat_ro <- gomz23_dat %>% 
# Changing units for consistency  
  mutate(wshd_area_km2_g = `DrainageArea[m2]`/1000000,
         reach_length_km_g = `Length[m]`/1000,) %>%
# Renaming variables for consistency  
  rename(stream_order_g = so,
         sinuosity_g = `sinuosity[-]`,
         slope_g = `slope[-]`,
         d50m_g = `D50[m]`,
         mean_ann_width_m_g = `w[m]`,
         bnkfll_width_m_g = `wbkf[m]`,
         mean_ann_depth_m_g = `d[m]`,
         bnkfll_depth_m_g = `dbkf[m]`,
         mean_ann_flow_m3s_g = `Qma[m3/s]`,
         bnkfll_q_m3s_g = `Qbf[m3/s]`,
         mean_ann_vel_ms_g = `Uma[m/s]`,
         hydrl_cond_ms_g = `K[m/s]`,
         porosity_ms_g = `poro[m/s]`,
         lateral_hyprh_flux_x = `Jx[-]`,
         lateral_hyprh_flux_y = `Jy[-]`,
         lateral_hyprh_flux_yx = `Jyx[-]`) %>% 
  # Calculating stream area
  mutate(stream_area_m2_g = reach_length_km_g*1000*mean_ann_width_m_g) %>% 
  # Selecting variables for merge
  select(comid,
         FromNode,
         ToNode,
         wshd_area_km2_g,
         reach_length_km_g,
         stream_order_g,
         stream_area_m2_g,
         sinuosity_g,
         slope_g,
         d50m_g,
         mean_ann_width_m_g,
         bnkfll_width_m_g,
         mean_ann_depth_m_g,
         bnkfll_depth_m_g,
         mean_ann_flow_m3s_g,
         bnkfll_q_m3s_g,
         mean_ann_vel_ms_g,
         hydrl_cond_ms_g,
         porosity_ms_g,
         lateral_hyprh_flux_x,
         lateral_hyprh_flux_y,
         lateral_hyprh_flux_yx)

# Integrating NEXSS Input Data
phys_sni_nxss_dat <- phys_sni_dat_ro %>% 
  merge(.,
        gomz23_dat_ro,
        by = "comid",
        all.x = TRUE)


write.csv(phys_dat_ro,paste(raw_data,"230620_ord_basin_hydrogeom_swf.csv", sep = '/'),
          row.names = FALSE)        

write.csv(phys_sni_dat_ro,paste(raw_data,"phys_nsi_dat_reference.csv", sep = '/'),
          row.names = FALSE)     

write.csv(phys_sni_nxss_dat,paste(raw_data,"230623_ord_basin_sni_nxss_hydrogeom_pnw.csv", sep = '/'),
          row.names = FALSE) 



summary(phys_sni_dat_ro)




