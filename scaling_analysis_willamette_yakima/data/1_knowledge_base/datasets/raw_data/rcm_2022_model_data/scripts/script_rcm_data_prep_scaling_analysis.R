###############################################################################
# Dataset preparation for scaling analysis
###############################################################################

gc()

librarian::shelf(tidyverse,
                 utils,
                 nhdplusTools,
                 sp,
                 sf,
                 leaflet,
                 stringr)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"

reference_comids <- read_csv(paste(source_data,"enhanced_nhdplus_21","data","enhanced_nhdplus21_ywrb.csv", sep = '/'),
                             show_col_types = FALSE)

nexss_inputs_dat <- read_csv(paste(local_data,"merged_nexss_inputs.csv", sep = '/'),
                             show_col_types = "FALSE")

no3_dat <- read_csv(paste(local_data,"merged_nhd_CR_stream_no3.csv", sep = "/"),
                    show_col_types = FALSE)

do_dat <- read_csv(paste(local_data,"merged_nhd_CR_stream_annual_DO.csv", sep = "/"),
                    show_col_types = FALSE)

doc_dat <- read_csv(paste(local_data,"merged_nhd_CR_stream_annual_DOC.csv", sep = "/"),
                    show_col_types = FALSE)

annual_resp_data <- read_csv(paste(local_data,"nhd_CR_stream_annual_resp_inputs_outputs.csv", sep = "/"),
                             show_col_types = FALSE)

resp_gap_filled_wrb_dat0 <- read_csv(paste(local_data,"son_etal_22_wrb_RF_resp_data.csv", sep = '/'),
                                show_col_types = "FALSE") %>% select(-1)

resp_gap_filled_yrb_dat0 <- read_csv(paste(local_data,"son_etal_22_yrb_RF_resp_data.csv", sep = '/'),
                                    show_col_types = "FALSE") %>% select(-1)

nsi_ssn_ntwk_dat <- st_transform(st_read(paste(source_data,"nsi_ssn_network","data","nsi_network_ywrb.shp",sep = "/")),4326)


# Checking connectivity in datasets

reference_comids <- reference_comids %>% 
  select(comid,
         tocomid,
         huc_4) %>% 
  mutate(basin = if_else(huc_4 == "1703",
                         "yakima",
                         "willamette"))

nexss_inputs_dat <- nexss_inputs_dat %>% 
  merge(.,
        reference_comids %>% 
          select(comid,basin),
        by = "comid",
        all.x = TRUE)

# Connectivity test
test_dat_connectivity <- nexss_inputs_dat %>% 
  group_by(basin)%>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

# Connectivity is 97.5% for Willamette and 95% for Yakima

# We will use nexss inputs dataset as a comid reference to merge the subsequent 
# data (e.g. no3, do, doc, etc...)

rcm_dat <- nexss_inputs_dat %>% 
  merge(.,
        do_dat %>% 
          select(comid,
                 Stream_DO) %>% 
          rename(do_stream_mg_l = Stream_DO),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        doc_dat %>% 
          select(comid,
                 Stream_DOC) %>% 
          rename(doc_stream_mg_l = Stream_DOC),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        no3_dat %>% 
          select(comid,
                 no3_conc_mg_l) %>% 
          rename(no3_stream_mg_l = no3_conc_mg_l),
        by = "comid",
        all.x = TRUE)

# Connectivity test
test_dat_connectivity <- rcm_dat %>% 
  group_by(basin)%>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

# Connectivity is maintained

################################################################################
# Repiration Data

resp_gap_filled_wrb_dat <- resp_gap_filled_wrb_dat0 %>% 
    select(comid,
         totco2g_day_fill) %>% 
  rename(totco2g_day = totco2g_day_fill) 
summary(resp_gap_filled_wrb_dat)

resp_gap_filled_yrb_dat <- resp_gap_filled_yrb_dat0 %>% 
  select(comid,
         totco2g_day_fill) %>% 
  rename(totco2g_day = totco2g_day_fill) 
summary(resp_gap_filled_yrb_dat)

resp_gap_filled_dat <- rbind(resp_gap_filled_wrb_dat,
                             resp_gap_filled_yrb_dat)

# Merging nexss inputs data with gap filled respiration data

rcm_gap_filled_dat <- rcm_dat %>% 
  merge(.,
        resp_gap_filled_dat,
        by = "comid",
        all.x = TRUE)

# Connectivity test
test_dat_connectivity <- rcm_gap_filled_dat %>% 
  filter(is.na(totco2g_day)==FALSE) %>% 
  group_by(basin)%>% 
  mutate(inc_comid = 1,
         tot_comid = sum(inc_comid),
         accm_inc_comid = calculate_arbolate_sum(data.frame(ID = comid,
                                                            toID = tocomid,
                                                            length = inc_comid)),
         connectivity_index = (max(accm_inc_comid)/tot_comid*100)) %>% 
  summarise(across(c("tot_comid", "accm_inc_comid", "connectivity_index"), max)) %>% 
  ungroup()
test_dat_connectivity

# Removing NA's for totco2g_day does not affect network connectivity as these values
# correspond to the Columbia River basin. Removing NA's from Yakima does decrease the
# overall connectivity in ~ 10%

rcm_gap_filled_dat <-  rcm_gap_filled_dat %>% 
  filter(!(basin == "willamette" & is.na(totco2g_day)))%>%
  merge(.,
        annual_resp_data %>% 
          select(comid,
                 logrt_total_hz_s,
                 logq_hz_total_m_s),
        by = "comid",
        all.x = TRUE) %>% 
  mutate(tot_rt_hz_s = 10^logrt_total_hz_s,
         tot_q_hz_ms = 10^logq_hz_total_m_s)# Expressing residence time and hyporheic exchange on arithmetic scale

summary(rcm_gap_filled_dat)

write.csv(rcm_gap_filled_dat,paste(local_data,"RF_filled_rcm_2022_model_data.csv", sep ='/'),
          row.names = FALSE)

# Generating shapefiles

nsi_rcm_ntwk_dat <- nsi_ssn_ntwk_dat %>% 
  rename(comid = COMID) %>% 
  merge(.,
        rcm_gap_filled_dat, 
        by = "comid",
        all.x = TRUE) %>% 
  filter(!(basin == "willamette" & is.na(totco2g_day)))

# Checking resulting network

leaflet(nsi_rcm_ntwk_dat) %>% 
  addPolylines(weight = 2) %>% 
  addPolylines(data = filter(nsi_rcm_ntwk_dat,is.na(do_stream_mg_l)==TRUE),
               color = "magenta",
               opacity = 1,
               weight = 9) %>% 
  addProviderTiles("Esri.WorldImagery")


# Saving resulting shape files

# Rename columns so they meet the 10 character length ESRI format
nsi_rcm_ntwk_sto <- nsi_rcm_ntwk_dat %>% 
  rename(pd_ann_do = do_stream_mg_l,
         pd_ann_doc = doc_stream_mg_l,
         no3_mgl = no3_stream_mg_l,
         t_co2_gday = totco2g_day,
         t_rthz_s = tot_rt_hz_s,
         t_qhz_ms = tot_q_hz_ms) %>% 
  select(-c(logRT_vertical_hz_s,
            logRT_lateral_hz_s,
            logq_hz_vertical_m_div_s,
            logq_hz_lateral_m_div_s,
            logrt_total_hz_s,
            logq_hz_total_m_s))

# Determine the maximum width required for the 'tocomid' field
max_tocomid_width <- max(nchar(as.character(nsi_rcm_ntwk_sto$tocomid)))

# Update the field width for 'tocomid' in 'nsi_rcm_phys_sto'
nsi_rcm_ntwk_sto$tocomid <- format(nsi_rcm_ntwk_sto$tocomid, width = max_tocomid_width)

# Maintaining the numeric format of tocomid
nsi_rcm_ntwk_sto$tocomid <- as.numeric(nsi_rcm_ntwk_sto$tocomid)

# Specify the file path for saving the shapefile
output_file <- paste(local_data,"shapefiles","river_corridors_respiration_geom.shp", sep = '/')

# Write the 'nsi_rcm_phys_sto' data frame to a shapefile
st_write(nsi_rcm_ntwk_sto, 
         dsn = output_file, 
         delete_dsn = TRUE, 
         overwrite_layer = TRUE, 
         delete_layer = TRUE)

