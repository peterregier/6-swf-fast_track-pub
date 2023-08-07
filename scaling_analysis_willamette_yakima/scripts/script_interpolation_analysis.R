################################################################################
# Scaling watershed function: Data Analysis-Interpolation & Cumulative values
################################################################################

# Author: Francisco J. Guerrero

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

local_data <- "./data"

scaling_resp_raw_dat <- read_csv("https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/datasets/processed_data/river_corridor_physical_hyporheic_characteristics/data/qaqc_river_corridors_physical_hyporheic_char.csv",
                             show_col_types = FALSE)
# Exploring trends in slope and roughness with stream order: 

slope_plot_1 <- ggplot(data = scaling_resp_raw_dat,
                     aes(x = as.factor(stream_order),
                         y = reach_slope,
                         color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  labs(x = "Stream order (Strahler)", y = "Reach slope (m/m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
slope_plot_1


slope_plot_2 <- ggplot(data = scaling_resp_raw_dat,
                       aes(x = wshd_area_km2,
                           y = reach_slope,
                           color = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Watershed area (km2)", y = "Reach slope (m/m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
slope_plot_2

# Filling gaps for slope and roughness with: interpolate_missing_values()

# Let's start with reach slope, which we expect should be related to ctch_area_km2
# and ctch_basin_slope

# We will use a pair of functions to interpolate missing values (n<50) (e.g., roughness, reach_slope)
# Helper function to get immediate neighbors' median value
get_immediate_neighbors_median <- function(data, column, comid, tocomid) {
  immediate_neighbors <- c(comid, tocomid)
  values <- data[[column]][data$comid %in% immediate_neighbors & data[[column]] >= 0]
  median_value <- median(values, na.rm = TRUE)
  return(median_value)
}

# Helper function to calculate subsample based on specific conditions
calculate_subsample <- function(data, column, i) {
  same_basin <- data$basin == data$basin[i]
  pcpt_range <- range(data$mean_ann_pcpt_mm[same_basin])
  area_range <- range(data$wshd_area_km2[same_basin])
  pcpt_bin <- cut(data$mean_ann_pcpt_mm, breaks = seq(pcpt_range[1], pcpt_range[2], length.out = 10))
  area_bin <- cut(data$wshd_area_km2, breaks = seq(area_range[1], area_range[2], length.out = 10))
  subsample <- data[[column]][same_basin & (data$stream_order == data$stream_order[i]) &
                                (pcpt_bin == pcpt_bin[i]) & (area_bin == area_bin[i])]
  return(subsample)
}

# Main function to interpolate missing values
interpolate_missing_values <- function(data, column, regression = TRUE) {
  
  column <- rlang::sym(column)
  
  data <- data %>%
    mutate(!!column := ifelse(!!column < 0 | is.na(!!column), NA, !!column))
  
  for (i in seq_len(nrow(data))) {
    
    # Check if the column value is missing (represented by NA)
    if (is.na(data[[rlang::as_string(column)]][i])) {
      immediate_median <- get_immediate_neighbors_median(data, rlang::as_string(column), data$comid[i], data$tocomid[i])
      
      # If there are no immediate neighbors, replace with the median value from the defined subsample
      if (is.na(immediate_median)) {
        # Calculate the subsample based on 'stream_order', 'mean_ann_pcpt_mm', 'wshd_area_km2', and 'basin'
        subsample <- calculate_subsample(data, rlang::as_string(column), i)
        immediate_median <- median(subsample, na.rm = TRUE)
      }
      
      # If the value is still NA and regression is TRUE, replace with the predicted value from the log-linear model
      if (is.na(immediate_median) & regression) {
        # Make sure we only use rows with non-NA and positive values for 'mean_ann_pcpt_mm' and 'wshd_area_km2' 
        # to fit the model
        valid_rows <- !is.na(data[[rlang::as_string(column)]]) & data$mean_ann_pcpt_mm > 0 & data$wshd_area_km2 > 0
        # Create the formula dynamically
        formula <- as.formula(paste(rlang::as_string(column), "~ log(stream_order) + log(mean_ann_pcpt_mm) + log(wshd_area_km2)"))
        model <- lm(formula, data = data[valid_rows, ])
        # Predict the value for the current row
        immediate_median <- as.numeric(predict(model, newdata = data[i, ])[1])
        
        # If the predicted value is less than 0, replace with the minimum positive value in the column
        if (immediate_median < 0) {
          immediate_median <- min(data[[rlang::as_string(column)]][data[[rlang::as_string(column)]] > 0], na.rm = TRUE)
        }
      }
      
      # Assign the calculated value to the missing value
      data[[rlang::as_string(column)]][i] <- immediate_median
    }
  }
  
  return(data)
}

# Interpolating values:

# Roughness (29 NAs)
roughness_int <- interpolate_missing_values(data = scaling_resp_raw_dat %>% 
                                              select(comid,
                                                     tocomid,
                                                     basin,
                                                     stream_order,
                                                     mean_ann_pcpt_mm,
                                                     wshd_area_km2,
                                                     roughness),
                                            column = "roughness",
                                            regression = TRUE)
# Reach slope (NAs = 1301)
reach_slope_int <- interpolate_missing_values(data = scaling_resp_raw_dat %>% 
                                                mutate(reach_slope = ifelse(reach_slope == 0.00000001,
                                                                            NA,
                                                                            reach_slope)) %>% 
                                                select(comid,
                                                       tocomid,
                                                       basin,
                                                       stream_order,
                                                       mean_ann_pcpt_mm,
                                                       wshd_area_km2,
                                                       reach_slope),
                                              "reach_slope",
                                              regression = TRUE)
summary(reach_slope_int)

# We will also interpolate nutrient concentrations using the same algorithm.

# Let's first check if setting the regression function as FALSE generates NA values

# Total Respiration (10 NAs)
totco2_int <- interpolate_missing_values(data = scaling_resp_raw_dat %>% 
                                           select(comid,
                                                  tocomid,
                                                  basin,
                                                  stream_order,
                                                  mean_ann_pcpt_mm,
                                                  wshd_area_km2,
                                                  totco2g_day),
                                         "totco2g_day",
                                         regression = FALSE)

summary(totco2_int)

# DO
do_int <- interpolate_missing_values(data = scaling_resp_raw_dat %>% 
                                           select(comid,
                                                  tocomid,
                                                  basin,
                                                  stream_order,
                                                  mean_ann_pcpt_mm,
                                                  wshd_area_km2,
                                                  do_stream_mg_l),
                                         "do_stream_mg_l",
                                         regression = FALSE)

summary(do_int)


# DOC
doc_int <- interpolate_missing_values(data = scaling_resp_raw_dat %>% 
                                       select(comid,
                                              tocomid,
                                              basin,
                                              stream_order,
                                              mean_ann_pcpt_mm,
                                              wshd_area_km2,
                                              doc_stream_mg_l),
                                     "doc_stream_mg_l",
                                     regression = FALSE)

summary(doc_int)

# NO3
no3_int <- interpolate_missing_values(data = scaling_resp_raw_dat %>% 
                                        select(comid,
                                               tocomid,
                                               basin,
                                               stream_order,
                                               mean_ann_pcpt_mm,
                                               wshd_area_km2,
                                               no3_stream_mg_l),
                                      "no3_stream_mg_l",
                                      regression = FALSE)

summary(no3_int)



slope_plot_3 <- ggplot(data = reach_slope_int,
                       aes(x = as.factor(stream_order),
                           y = reach_slope,
                           color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  labs(x = "Stream order (Strahler)", y = "Reach slope (m/m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
slope_plot_3


slope_plot_4 <- ggplot(data = reach_slope_int,
                       aes(x = wshd_area_km2,
                           y = reach_slope,
                           color = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Watershed area (km2)", y = "Reach slope (m/m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
slope_plot_4

# Hyporheic variables associated to d50 == 0.000001, should be labelled as NA, 
# because they are linked to assigned constant slope values of 0.00001 (1 e-8 when
# expressed as m/m instead of m/km), which has been shown to produce distributional 
# anomalies when compared to measured slopes in stream channels (Schwartz et al., 2019).

scaling_resp_prcssd_dat_1 <- scaling_resp_raw_dat %>%
  mutate_at(vars(d50_m,
                 tot_rt_hz_s,
                 tot_q_hz_ms,
                 logrt_total_hz_s,
                 logq_hz_total_m_s,
                 logRT_vertical_hz_s,
                 logRT_lateral_hz_s,
                 logq_hz_vertical_m_div_s,
                 logq_hz_lateral_m_div_s),
            ~ if_else(d50_m == 0.000001, NA, .)) %>% 
  select(-c(reach_slope,
            roughness,
            totco2g_day,
            do_stream_mg_l,
            doc_stream_mg_l,
            no3_stream_mg_l)) 

scaling_resp_prcssd_dat_1 <- scaling_resp_prcssd_dat_1 %>%
  merge(.,reach_slope_int %>% select(comid,reach_slope), by = "comid",all.x = TRUE) %>%
  merge(.,roughness_int %>% select(comid,roughness), by = "comid",all.x = TRUE) %>%
  merge(.,totco2_int %>% select(comid,totco2g_day), by = "comid",all.x = TRUE) %>%
  merge(.,do_int %>% select(comid,do_stream_mg_l), by = "comid",all.x = TRUE) %>%
  merge(.,doc_int %>% select(comid,doc_stream_mg_l), by = "comid",all.x = TRUE) %>%
  merge(.,no3_int %>% select(comid,no3_stream_mg_l), by = "comid",all.x = TRUE) 
  
summary(scaling_resp_prcssd_dat_1)

# We observe a number of datapoints with reach_slope = 0.00000001. These correspond
# to default values assigned at NHDPlus when no other values were available.  Let's
# take a look

summary(filter(scaling_resp_prcssd_dat_1, reach_slope < 0.0000001))

# We find 229 of these values in this dataset (~ 1%) which seems better than the updated
# version of NHDPlus 2.1. (~6%)

# Recalculating wshd stream density, and cumulative variables
scaling_resp_prcssd_dat_2 <-  scaling_resp_prcssd_dat_1 %>% 
  mutate(stream_area_m2 = (reach_length_km*bnkfll_width_m)*1000) %>% 
  group_by(basin) %>% 
  mutate(across(c(wshd_stream_dens,
                  tot_stream_length_km,
                  wshd_area_km2,
                  ctch_area_km2,
                  ctch_stream_dens,
                  ctch_basin_slope,
                  reach_slope,
                  totco2g_day,
                  stream_area_m2), ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                       toID = tocomid,
                                                                       length = .x))) %>% 
           set_names(paste0("accm_", names(select(., wshd_stream_dens:stream_area_m2))))) %>% 
  ungroup()

summary(scaling_resp_prcssd_dat_2)

test_dat_connectivity <- scaling_resp_prcssd_dat_2 %>% 
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

# Connectivity is maintained

# Saving Interpolated dataset
write.csv(scaling_resp_prcssd_dat_2,paste(local_data,"interpolated_scaling_resp_dat.csv", sep = '/'),
          row.names = FALSE)
