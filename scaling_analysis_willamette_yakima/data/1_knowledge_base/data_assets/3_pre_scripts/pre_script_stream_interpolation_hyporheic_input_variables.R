################################################################################
# NEXSS data gap filling with interpolation across the stream network
################################################################################

# Author: Francisco J. Guerrero
gc(reset = TRUE)

librarian::shelf(tidyverse,
                 ggplot2,
                 sfnetworks,
                 sp,
                 sf,
                 leaflet,
                 utils,
                 nhdplusTools,
                 htmlwidgets,
                 tidygraph,
                 sfheaders,
                 rgdal,
                 data.table)
                 
                 
# Local import / export paths

raw_data <- "raw" 
processed_data <- "processed"
shapes_data <- "shape_files"


# Reading files from raw data

newest23_pnw_dat <- read_csv(paste(raw_data,"newest23_hyporheic_pnw_data.csv", sep = '/'),
          show_col_types = FALSE)

current20_pnw_dat <- read_csv(paste(raw_data,"current20_hyporheic_pnw_data.csv", sep = '/'),
          show_col_types = FALSE)

enh_nhd21_dat <- read_csv(paste(raw_data,"230620_enhanced_nhdp_2_swf.csv", sep = '/'),
                          show_col_types = FALSE)

# Shapefiles
nsi_pnw_stream <- sf::st_transform(st_read(paste(raw_data,"shape_files", "nsi_reference","230623_nsi_network_ywrb.shp", sep = '/')),4326)

# Removing duplicated comids
nis_pnw_stream <- nis_pnw_stream %>% 
  filter(DUP_COMID == 0)

# Merging datasets

hyprh_dat <- enh_nhd21_dat %>%
  filter(huc_4 != "0107") %>%
  rename(streamorder = streamorde,
         usgs_slope = slope,
         usgs_area_km2 = totdasqkm,
         usgs_length_km = lengthkm) %>%
  select(comid,
         tocomid,
         usgs_slope,
         usgs_area_km2,
         usgs_length_km,
         streamorder,
         ftype,
         huc_4) %>%
  mutate(basin = if_else(huc_4 == "1703",
                         "yakima",
                         "willamette")) %>%
  merge(.,
        current20_pnw_dat %>%
          mutate(c_area_km2 = 10^logDA_km2,
                 c_q_m3_s = 10^logQ_m3_div_s,
                 c_RT_lateral_hz_s = 10^logRT_lateral_hz_s,
                 c_RT_vertical_hz_s = 10^logRT_vertical_hz_s,
                 c_q_hz_vertical_m_div_s = 10^logq_hz_vertical_m_div_s,
                 c_q_hz_lateral_m_div_s = 10^logq_hz_lateral_m_div_s,
                 c_w_m = 10^logw_m,
                 c_wbkf_m = 10^logwbkf_m,
                 c_depth_m = 10^logd_m,
                 c_depth_bkf_m = 10^logdbkf_m,
                 c_slope = 10^logSlope,
                 c_k_m_s = 10^logK_m_div_s,
                 c_lenght_km = length_m/1000) %>%
          rename(c_d50_m = D50_m) %>%
          select(comid,
                 c_area_km2,
                 c_q_m3_s,
                 c_RT_lateral_hz_s,
                 c_RT_vertical_hz_s,
                 c_q_hz_lateral_m_div_s,
                 c_q_hz_vertical_m_div_s,
                 c_lenght_km,
                 c_w_m,
                 c_depth_m,
                 c_wbkf_m,
                 c_depth_bkf_m,
                 c_slope,
                 c_d50_m,
                 c_k_m_s),
        by = "comid",
        all.x = TRUE) %>%
  merge(.,
        newest23_pnw_dat %>%
          mutate(n_area_km2 = `DrainageArea[m2]`/1000000,
                 n_length_km = `Length[m]`/1000) %>%
          rename(n_streamorder = streamorder,
                 n_sinuosity = `sinuosity[-]`,
                 n_slope = `slope[-]`,
                 n_d50_m = `D50[m]`,
                 n_w_m = `w[m]`,
                 n_wbkf_m = `wbkf[m]`,
                 n_depth_m = `d[m]`,
                 n_depth_bkf_m = `dbkf[m]`,
                 n_mean_annual_q_m3_s = `Qma[m3/s]`,
                 n_bnkfll_q_m3_s = `Qbf[m3/s]`,
                 n_mean_annual_vel_m_s = `Uma[m/s]`,
                 n_k_m_s = `K[m/s]`,
                 n_porosity_m_s = `poro[m/s]`,
                 n_jx = `Jx[-]`,
                 n_jy = `Jy[-]`,
                 n_jyx = `Jyx[-]`) %>%
          select(comid,
                 FromNode,
                 ToNode,
                 n_streamorder,
                 n_area_km2,
                 n_length_km,
                 n_sinuosity,
                 n_slope,
                 n_d50_m,
                 n_w_m,
                 n_wbkf_m,
                 n_depth_m,
                 n_depth_bkf_m,
                 n_mean_annual_q_m3_s,
                 n_bnkfll_q_m3_s,
                 n_mean_annual_vel_m_s,
                 n_k_m_s,
                 n_porosity_m_s,
                 n_jx,
                 n_jy,
                 n_jyx),
        by = "comid",
        all.x = TRUE)

summary(hyprh_dat)

# The max number of NAs in the newest data (2023) is 1494 and corresponded to mean annual 
# velocity. There are 182 NAs for the other variables in the dataset. 

# The number of NAs in the current dataset (2020) data varies across variables ranging
# from 281 for bankfull width to 3060 lateral hyporheic flow.


# Let's take a look at the stream characteristics for missing D50 values in the 
# Gomez's dataset

summary(filter(hyprh_dat,is.na(n_d50_m)==TRUE))

# The missing values correspond mostly to first order streams, but there is a 7 
# order stream included as well. 

summary(filter(hyprh_dat,is.na(n_d50_m)==TRUE & streamorder > 1))

# Looking at stream networks using leaflet

leaflet(nis_pnw_stream) %>% 
  addPolylines(weight = 2) %>%  
  addProviderTiles("Esri.WorldImagery")

# adding D50 data from Son et al. 2022 and Gomez 2023

nis_pnw_stream_dat <- nis_pnw_stream %>% 
  rename(comid = COMID) %>% 
  merge(.,
        hyprh_dat %>%
          select(comid,
                 tocomid,
                 basin,
                 streamorder,
                 c_d50_m,
                 c_q_m3_s,
                 c_RT_lateral_hz_s,
                 c_RT_vertical_hz_s,
                 c_q_hz_lateral_m_div_s,
                 c_q_hz_vertical_m_div_s,
                 c_lenght_km,
                 c_w_m,
                 c_depth_m,
                 c_wbkf_m,
                 c_depth_bkf_m,
                 c_slope,
                 c_d50_m,
                 c_k_m_s),
        by = "comid",
        all.x = TRUE)

leaflet(nis_pnw_stream_dat) %>% 
  addPolylines(weight = 2) %>%  
  addPolylines(data = filter(nis_pnw_stream_dat, is.na(c_q_hz_lateral_m_div_s)==TRUE),
               weight = 8,
               color = "darkorange",
               opacity = 1) %>%
  addPolylines(data = filter(nis_pnw_stream_dat, is.na(c_k_m_s)==TRUE),
               weight = 4,
               color = "darkorchid",
               opacity = 1) %>% 
  addPolylines(data = filter(nis_pnw_stream_dat, is.na(c_d50_m)==TRUE),
               weight = 2,
               color = "green",
               opacity = 2) %>%
  addProviderTiles("Esri.WorldImagery")


################################################################################
# GAP Filling
################################################################################

# Define the variable to use for filling NA values
desired_variable <- "c_d50_m"

# Read the NHDPlus flowline dataset
flowlines <- st_as_sf(nis_pnw_stream_dat %>% 
                        select(comid,
                               tocomid,
                               {{desired_variable}}))

# Create a tidygraph object from the flowlines dataset
graph <- as_tbl_graph(flowlines, directed = TRUE)

# Convert the dataset to an sfnetwork object
sfn <- as_sfnetwork(flowlines)

# Plot the river network
# plot(sfn, edge.arrow.size = 1.0, edge.color = "blue", vertex.color = "magenta", vertex.size = 0.5)


# Function to fill NA values from upstream and downstream flowlines
fill_na_values <- function(data, var) {
  updated_data <- data
  
  # Find initial NA values
  na_indices <- which(is.na(updated_data[[var]]))
  
  while (length(na_indices) > 0) {
    # Find upstream and downstream flowlines for NA values
    upstream <- filter(updated_data, comid %in% updated_data$tocomid[na_indices])
    downstream <- filter(updated_data, tocomid %in% updated_data$comid[na_indices])
    
    # Find non-NA neighbor values
    neighbor_values <- c(upstream[[var]], downstream[[var]])
    non_na_values <- neighbor_values[!is.na(neighbor_values)]
    
    if (length(non_na_values) > 0) {
      # Calculate the average of non-NA neighbor values for NA indices
      updated_data[[var]][na_indices] <- rep(mean(non_na_values), length(na_indices))
    }
    
    # Find updated NA values
    na_indices <- which(is.na(updated_data[[var]]))
  }
  
  return(updated_data)
}

# Set the number of iterations
num_iterations <- 5

# Set the percentage for the random sample size (modify as desired)
sample_percentage <- 0.1

# Calculate the sample size as a percentage of the dataset length
sample_size <- round(sample_percentage * nrow(flowlines))

# Create a list to store results of each iteration
results <- vector("list", num_iterations)

# Run iterations
for (i in 1:num_iterations) {
  # Take a random sample of flowlines with desired variable not NA
  non_na_indices <- which(!is.na(flowlines[[desired_variable]]))
  random_indices <- sample(non_na_indices, size = sample_size, replace = FALSE)
  random_sample <- flowlines[random_indices, ]
  
  # Get the remaining data as temp_flowlines
  temp_flowlines <- flowlines[-random_indices, ]
  
  # Replace the values of the desired variable with NA in the random sample
  random_sample[[desired_variable]] <- NA
  
  # Bind the random sample with temp_flowlines
  combined_data <- bind_rows(random_sample, temp_flowlines)
  
  # Fill NA values in temp_flowlines using fill_na_values function
  filled_flowlines <- fill_na_values(combined_data, {{ desired_variable }})
  
  # Remove duplicated comid's (legacy from the streametwork dataset)
  filled_flowlines <- distinct(filled_flowlines, comid, tocomid, .keep_all = TRUE)
  
  # Calculate cumulative sum for the filled desired variable
  accm_var <- calculate_arbolate_sum(data.frame(ID = filled_flowlines$comid, 
                                                toID = filled_flowlines$tocomid, 
                                                length = filled_flowlines[[desired_variable]]))
  
  # Create a new column for the cumulative sum
  filled_flowlines <- filled_flowlines %>%
    mutate(!!paste0("accm_", desired_variable) := accm_var)
  
  # Store the result in the list
  results[[i]] <- filled_flowlines
}

# Combine the results into a single data frame
combined_results <- bind_rows(results)

# Convert combined_results to a data.table
dt <- as.data.table(combined_results)

# Calculate summary statistics by comid
# Calculate summary statistics by comid
summary_stats <- dt[, .(
  !!paste0("avg_", desired_variable) == mean(get(desired_variable), na.rm = TRUE),
  !!paste0("sd_", desired_variable) == sd(get(desired_variable), na.rm = TRUE),
  !!paste0("avg_accm_", desired_variable) == mean(get(starts_with("accm_")), na.rm = TRUE),
  !!paste0("sd_accm_", desired_variable) == sd(get(starts_with("accm_")), na.rm = TRUE)
), by = comid]


# Calculate summary statistics by comid
summary_stats <- dt[, .(avg_c_d50_m = mean(c_d50_m, na.rm = TRUE),
                        sd_c_d50_m = sd(c_d50_m, na.rm = TRUE),
                        avg_accm_c_d50_m = mean(accm_c_d50_m, na.rm = TRUE),
                        sd_accm_c_d50_m = sd(accm_c_d50_m, na.rm = TRUE)),
                        by = comid]

# View the summary statistics
summary_stats

nis_pnw_stream_interpolated_dat <- nis_pnw_stream_dat %>% 
  merge(.,
        summary_stats %>% 
          select(comid,
                 avg_c_d50_m,
                 avg_accm_c_d50_m,
                 sd_c_d50_m,
                 sd_accm_c_d50_m))

eval_plot <- ggplot(data = filter(nis_pnw_stream_interpolated_dat,
                                  is.na(c_d50_m)==FALSE),
                    aes(x = c_d50_m,
                        y = avg_c_d50_m,
                        color = FTYPE))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(~basin, ncol = 2)
eval_plot
  
nis_pnw_stream_interpolated_dat <- nis_pnw_stream_interpolated_dat %>% 
  mutate(d50_diff = abs((c_d50_m - avg_c_d50_m)))



leaflet(nis_pnw_stream_interpolated_dat) %>% 
  # addPolylines(data = filter(combined_results, is.na(c_d50_m)==FALSE),
  #              weight = 4,
  #              color = "darkorange",
  #              opacity = 1) %>% 
  addPolylines(data = filter(combined_results, "d50_diff" > 0.01),
               weight = 4,
               color = "magenta",
               opacity = 1) %>%
  addPolylines(weight = 2) %>% 
  addProviderTiles("Esri.WorldImagery")
