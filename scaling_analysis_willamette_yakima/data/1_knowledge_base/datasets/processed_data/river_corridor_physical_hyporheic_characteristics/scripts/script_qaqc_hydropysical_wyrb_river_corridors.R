################################################################################
# SCALING WATERSHED FUNCTION: PHYSICAL DATA QA/QC
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
                 foreign,
                 data.table,
                 betareg,
                 Hmisc)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"
local_metadata <- "./metadata"

nsi_rcm_phys_dat <- read_csv(paste(local_data,"river_corridors_physical_hyporheic_char.csv", sep = '/'),
                 show_col_types = FALSE)

# Let's take a look at the data

summary(nsi_rcm_phys_dat)

# Variables including zero values

# Calculate the number of zeros in each column
zero_counts <- colSums(nsi_rcm_phys_dat == 0, na.rm = TRUE)

# Create a new dataframe to store the report
zero_report <- data.frame(variable = names(zero_counts), Zeros = zero_counts) %>% 
  filter(Zeros > 0)

# Print the report without row names
print(zero_report, row.names = FALSE)

# We will first remove DUP_COMID that are a special feature of the NSI dataset and test 
# for connectivity

# Testing initial network connectivity

test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

################################################################################

# Let's now take a look at those comid's with zero values for watershed areas. 
# According to Blodgett (2023, pers. coms.) these zero values may correspond to 
# flowlines that are not connected to the network but were found and digitized

summary(filter(nsi_rcm_phys_dat, wshd_area_km2 == 0))

# All the flowlines with zero values in wshd area are first order streams, so let's
# run a connectivity test that ignores these values: 

test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  filter(wshd_area_km2 > 0) %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

# As suspected, we can remove these values whitout any lost in network connectivity

########## DATASET MODIFICATION ################################################ 
# We drop comid's with wshd_area_km2 == 0 from the dataset, and reaches with 
# stream order > 8, since these correspond to the Columbia River
################################################################################

nsi_rcm_phys_dat_m1 <-  nsi_rcm_phys_dat %>% 
  filter(wshd_area_km2 > 0) 

################################################################################

# Let's now look at the remaining values with catchment areas = 0

summary(filter(nsi_rcm_phys_dat_m1, ctch_area_km2 == 0))

# We find 88 additional datapoints with catchment areas = 0. These data points
# encompass multiple stream orders, have reach lengths between 6 to 11.4 m and 
# non-zero values for accumulated stream density, so they are actually drained
# and connected to the network. So, it could be expected that just removing these
# datapoints, would result in reduction of network connectivity:


test_dat_connectivity <- nsi_rcm_phys_dat_m1 %>% 
  filter(ctch_area_km2 > 0) %>% 
  group_by(basin) %>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

# As expected network connectivity decreases significantly in both basins, but more
# so in the YRB

# Let's plot catchment area vs reach_length as it is expected that these two variables
# scale with each other:

# Catchment area ~ Reach length

ctch_rch_plot <- ggplot(data = nsi_rcm_phys_dat_m1 %>% 
                          filter(ctch_area_km2 > 0),
                        aes(x = reach_length_km*1000,
                            y = ctch_area_km2,
                            color = as.factor(stream_order)))+
  geom_point()+
  geom_smooth(span = 0.10)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
ctch_rch_plot

# Although the relationship is not ideal, it would be good enough to fill the 
# small number of gaps

############################# DATASET MODIFICATION #############################
# Fill gaps for catchment area with loess model:
# model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), data = ., span = 0.15)
################################################################################

# Fiting the loess model and estimating catchment area from reach length

model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), 
               data = nsi_rcm_phys_dat_m1 %>% 
                 select(comid,
                        ctch_area_km2,
                        reach_length_km) %>% 
                 filter(ctch_area_km2 >0), 
               span = 0.15)

# Filter the data to get rows with missing 'ctch_area_km2' values
missing_data <- nsi_rcm_phys_dat_m1 %>% 
  select(comid,
         ctch_area_km2,
         reach_length_km) %>% 
  filter(is.na(ctch_area_km2) == TRUE| ctch_area_km2 == 0)


# Predict the missing 'ctch_area_km2' values using the loess model
predicted_values <- 10^predict(model, newdata = missing_data)

# Update the missing 'ctch_area_km2' values with the predicted values
missing_data$ctch_area_km2 <- predicted_values

# Combine the original data and the data with predicted values
estimated_ctch_data <- rbind(nsi_rcm_phys_dat_m1 %>% 
                               select(comid,
                                      ctch_area_km2,
                                      reach_length_km) %>% 
                               filter(ctch_area_km2>0),
                             missing_data)

# Replace original values in the dataset for their estimates:
nsi_rcm_phys_dat_m2 <- nsi_rcm_phys_dat_m1%>% 
  select(-ctch_area_km2) %>% 
  merge(.,
        estimated_ctch_data %>% 
          select(comid,ctch_area_km2),
        by = "comid",
        all.x = TRUE)

summary(nsi_rcm_phys_dat_m2)
################################################################################

# We also find 7 zero values for hydraulic geometry variables including 
# bankfull width, depth, and cross sectional area, as well as mean annual flow. We will 
# check on this values once we have removed first order streams with watershed area = 0.

filter(nsi_rcm_phys_dat_m2, bnkfll_width_m == 0) 

# These values correspond to first order streams, so we remove them from the dataset

# We also have 8 zero values for mean annual flow in first order streams, so we remove 
# them from the dataset


############################# DATASET MODIFICATION #############################
# Remove flowlines with bnkfll_width_m = 0
################################################################################

nsi_rcm_phys_dat_m3 <- nsi_rcm_phys_dat_m2 %>% 
  filter(bnkfll_width_m > 0 & mean_ann_flow_m3s > 0) %>% 
  mutate(bnkfll_xsec_area_m2 = if_else(bnkfll_xsec_area_m2 == 0,
                                       bnkfll_depth_m * bnkfll_width_m,
                                       bnkfll_xsec_area_m2))

################################################################################
# Variables with missing data

# Calculate the number of NA values in each column
na_counts <- colSums(is.na(nsi_rcm_phys_dat_m3))

# Calculate the number of negative values in each column
negative_counts <- colSums(nsi_rcm_phys_dat_m3 < 0, na.rm = TRUE)

# Create a new dataframe to store the report
report_data <- data.frame(
  variable = names(nsi_rcm_phys_dat_m3),
  NA_s = na_counts,
  Negative_s = negative_counts
) %>%
  filter(NA_s > 0 | Negative_s > 0)

# Print the report
print(report_data, row.names = FALSE)

# Besides the gaps we have in the hyporheic variables and D50 values, we 
# we also have 259 NA's for catchment stream_density (probably related to 
# 0 values in catchment area). Negative values in elevation could be expected (?)
# given the resolution of the data, but we have negative values for reach_slope, 
# in this case -99999 (i.e. missing data), 29 missing values for roughness, ~230 
# NAs for nutrient concentrations and only 10 for totco2g_day. 


############################# DATASET MODIFICATION #############################
# Recalculating catchment stream density
################################################################################

nsi_rcm_phys_dat_m4 <- nsi_rcm_phys_dat_m3 %>% 
  mutate(ctch_stream_dens = if_else(is.na(ctch_stream_dens)== TRUE,
                                    reach_length_km/ctch_area_km2,
                                    ctch_stream_dens))

summary(nsi_rcm_phys_dat_m4)
###############################################################################

# Recalculating wshd stream density and stream area
nsi_rcm_phys_dat_m5<-  nsi_rcm_phys_dat_m4 %>% 
  mutate(wshd_stream_dens = tot_stream_length_km/wshd_area_km2,
         ctch_stream_dens = reach_length_km/ctch_area_km2,
         stream_area_m2 = (reach_length_km*bnkfll_width_m)*1000) 



# Sorting and re-ordering columns 

nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_dat_m5 %>% 
  select(-c(huc_2_region_id,
            huc_region_raster_id,
            huc_4_subregion_id,
            logK_m_div_s)) %>% 
  select(comid,
         tocomid,
         reachcode,
         hydroseq,
         from_node,
         to_node,
         basin,
         wshd_area_km2,
         wshd_stream_dens,
         wshd_basin_slope,
         wshd_min_elevation_m,
         wshd_max_elevation_m,
         wshd_avg_elevation_m,
         ctch_area_km2,
         ctch_stream_dens,
         ctch_basin_slope,
         ctch_min_elevation_m,
         ctch_max_elevation_m,
         ctch_avg_elevation_m,
         mean_ann_pcpt_mm,
         mean_ann_temp_dc,
         mean_ann_runf_mm,
         stream_order,
         reach_type,
         reach_length_km,
         reach_slope,
         tot_stream_length_km,
         sinuosity,
         bnkfll_width_m,
         bnkfll_depth_m,
         bnkfll_xsec_area_m2,
         mean_ann_flow_m3s,
         mean_ann_vel_ms,
         roughness,
         d50_m,
         stream_area_m2,
         do_stream_mg_l,
         doc_stream_mg_l,
         no3_stream_mg_l,
         totco2g_day,
         tot_rt_hz_s,
         tot_q_hz_ms,
         logrt_total_hz_s,
         logq_hz_total_m_s,
         logRT_vertical_hz_s,
         logRT_lateral_hz_s,
         logq_hz_vertical_m_div_s,
         logq_hz_lateral_m_div_s)

write.csv(nsi_rcm_phys_qaqc_dat,
          paste(local_data,"qaqc_river_corridors_physical_hyporheic_char.csv", sep = '/'),
          row.names = FALSE)

################################################################################



