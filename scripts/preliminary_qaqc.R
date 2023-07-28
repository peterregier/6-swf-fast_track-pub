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
                 data.table)

# Local Import-Export
source_data <- "../../raw_data"
local_data <- "./data"
local_metadata <- "./metadata"

nsi_rcm_phys_dat <- read_csv(paste(local_data,"river_corridors_physical_hyporheic_char.csv", sep = '/'),
                 show_col_types = FALSE)

nlcd_2001_dat <- read_csv(paste(source_data,"nlcd_2001_v2019","data","nlcd_2001_v2019_NLCD01_TOT_CONUS.csv", sep = '/'),
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
  filter(DUP_COMID == 0) %>% 
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

########## DATASET MODIFICATION ################################################ 
# We will drop the DUP_COMIDS == 1 and DUP_ArSqKM from the dataset  
################################################################################

# Let's now take a look at those comid's with zero values for watershed areas. 
# According to Blodgett (2023, pers. coms.) these zero values may correspond to 
# flowlines that are not connected to the network but were found and digitized

summary(filter(nsi_rcm_phys_dat, wshd_area_km2 == 0))

# All the flowlines with zero values in wshd area are first order streams, so let's
# run a connectivity test that ignores these values: 

test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0 & wshd_area_km2 > 0) %>% 
  filter(stream_order<9) %>% 
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
# We will drop comid's with wshd_area_km2 == 0 from the dataset, and reaches with 
# stream order > 8
################################################################################

# Let's now look at the remaining values with catchment areas = 0

summary(filter(nsi_rcm_phys_dat, wshd_area_km2 > 0 & ctch_area_km2 == 0))

# We find 89 additional datapoints with catchment areas = 0. These data points
# encompass multiple stream orders, have reach lengths between 6 to 11.4 m and 
# non-zero values for accumulated stream density, so they are actually drained
# and connected to the network. So, it could be expected that just removing these
# datapoints, would result in reduction of network connectivity:


test_dat_connectivity <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0 & wshd_area_km2 > 0) %>%
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

ctch_rch_plot <- ggplot(data = nsi_rcm_phys_dat %>% 
                          filter(DUP_COMID == 0 & wshd_area_km2 > 0) %>%
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

# We also find ~ 55 zero values for hydraulic geometry variables including 
# bankfull width, depth, and cross sectional area, as well as mean annual flow. We will 
# check on this values once we have removed first order streams with watershed area = 0.


############################# DATASET MODIFICATION #############################
# Fill gaps for hydraulic geometry variables after filling gaps in watershed and
# catchment areas
################################################################################


# Variables with missing data

# Calculate the number of NA values in each column
na_counts <- colSums(is.na(nsi_rcm_phys_dat))

# Calculate the number of negative values in each column
negative_counts <- colSums(nsi_rcm_phys_dat < 0, na.rm = TRUE)

# Create a new dataframe to store the report
report_data <- data.frame(
  variable = names(nsi_rcm_phys_dat),
  NA_s = na_counts,
  Negative_s = negative_counts
) %>%
  filter(NA_s > 0 | Negative_s > 0)

# Print the report
print(report_data, row.names = FALSE)

# Besides the gaps we have in the hyporheic characteristics and D50 values, we 
# we also have 43 missing values for stream width (logw_m), 34 missing values 
# in roughness

############################# DATASET MODIFICATION #############################
# Fill gaps for slope and roughness with: interpolate_missing_values()
################################################################################


# In summary: 
# 1. We drop the DUP_COMIDS == 1 and DUP_ArSqKM from the dataset  
# 2. For first order streams with both wshd_basin_slope or ctch_basin_slope = 0,
# we replace these values with the reach slope
# 2. We drop comid's with wshd_area_km2 == 0 from the dataset
# 3. We fill gaps for catchment area with loess model:
# model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), data = ., span = 0.15)
# 4. We fill gaps for slope and roughness with: interpolate_missing_values()


nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_dat %>% 
  filter(DUP_COMID == 0) %>% 
  select(-c(DUP_COMID, DUP_ArSqKM, DUP_Length)) %>% 
  filter(wshd_area_km2 > 0 & stream_order < 9) %>% 
  mutate(wshd_basin_slope = if_else(wshd_basin_slope == 0 & stream_order == 1,
                                    reach_slope,
                                    wshd_basin_slope),
         ctch_basin_slope = if_else(ctch_basin_slope == 0 & stream_order == 1,
                                    reach_slope,
                                    ctch_basin_slope))

summary(nsi_rcm_phys_qaqc_dat)

# Let's do a new report on zero values: 

# Calculate the number of zeros in each column
zero_counts <- colSums(nsi_rcm_phys_qaqc_dat == 0, na.rm = TRUE)

# Create a new dataframe to store the report
zero_report <- data.frame(variable = names(zero_counts), Zeros = zero_counts) %>% 
  filter(Zeros > 0)

# Print the report without row names
print(zero_report, row.names = FALSE)

# Zero values for hydraulic geometry variables including bankfull width, depth, 
# and cross sectional area, as well as mean annual flow. has decreased from around 55 to 13 (at max).
# Let's try to find a simple way to fill this gaps. We will start with mean annual flow:


p <- ggplot(data = nsi_rcm_phys_qaqc_dat,
            aes(x = wshd_area_km2,
                y = mean_ann_flow_m3s,
                color = mean_ann_runf_mm))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p

# It seems that we could predict the missing values with a simple regression of 
# mean annual flow against watershed area and mean annual runoff

maf_mod <- lm(log(mean_ann_flow_m3s)~(log(wshd_area_km2)+log(mean_ann_runf_mm))*basin,
              data = nsi_rcm_phys_qaqc_dat %>% 
                filter(mean_ann_flow_m3s>0),
              na.action = na.omit)
summary(maf_mod)
#plot(maf_mod)


nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat %>% 
  mutate(mean_ann_flow_m3s = if_else(mean_ann_flow_m3s > 0, 
                                     mean_ann_flow_m3s,
                                     exp(predict.lm(maf_mod,.))))

# Let's try a similar approach for bankfull width

bfw_mod <- lm(log(bnkfll_width_m)~(log(wshd_area_km2)+log(mean_ann_runf_mm))*basin,
              data = nsi_rcm_phys_qaqc_dat %>% 
                filter(bnkfll_width_m>0),
              na.action = na.omit)
summary(bfw_mod)
#plot(bfw_mod)

nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat %>% 
  mutate(bnkfll_width_m = if_else(bnkfll_width_m> 0, 
                                     bnkfll_width_m,
                                     exp(predict.lm(bfw_mod,.))))

# Bankfull width and bankfull depth are highly correlated:

p <- ggplot(data = nsi_rcm_phys_qaqc_dat %>% 
              filter(bnkfll_depth_m > 0),
            aes(x = bnkfll_width_m,
                y = bnkfll_depth_m))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p

# We will predict the missing values for bankfull depths from bankfull width and
# mean annual flow

bkd_mod <- lm(log(bnkfll_depth_m)~(log(wshd_area_km2)+log(mean_ann_flow_m3s)*basin),
              data = nsi_rcm_phys_qaqc_dat %>% 
                filter(bnkfll_depth_m > 0),
              na.action = na.omit)
summary(bkd_mod)
#plot(bkd_mod)

nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat %>% 
  mutate(bnkfll_depth_m = if_else(bnkfll_depth_m> 0, 
                                  bnkfll_depth_m,
                                  exp(predict.lm(bkd_mod,.))),
         bnkfll_xsec_area_m2 = if_else(bnkfll_xsec_area_m2 > 0,
                                       bnkfll_xsec_area_m2,
                                       bnkfll_depth_m*bnkfll_width_m))
summary(nsi_rcm_phys_qaqc_dat)

###############################################################################
# Fiting the loess model and estimating catchment area from reach length

model <- loess(log10(ctch_area_km2) ~ log10(reach_length_km), 
               data = nsi_rcm_phys_qaqc_dat %>% 
                 select(comid,
                        ctch_area_km2,
                        reach_length_km) %>% 
                 filter(ctch_area_km2 >0), 
               span = 0.15)

# Filter the data to get rows with missing 'ctch_area_km2' values
missing_data <- nsi_rcm_phys_qaqc_dat %>% 
  select(comid,
         ctch_area_km2,
         reach_length_km) %>% 
  filter(is.na(ctch_area_km2) | ctch_area_km2 == 0)

# Predict the missing 'ctch_area_km2' values using the loess model
predicted_values <- 10^predict(model, newdata = missing_data)

# Update the missing 'ctch_area_km2' values with the predicted values
missing_data$ctch_area_km2 <- predicted_values

# Combine the original data and the data with predicted values
estimated_ctch_data <- rbind(nsi_rcm_phys_qaqc_dat %>% 
                          select(comid,
                                 ctch_area_km2,
                                 reach_length_km) %>% 
                          filter(ctch_area_km2>0),
                          missing_data)

# Replace original values in the dataset for their estimates:
nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat%>% 
  select(-ctch_area_km2) %>% 
  merge(.,
        estimated_ctch_data %>% 
          select(comid,ctch_area_km2),
        by = "comid",
        all.x = TRUE)

# Let's take a look again at the ctch_basin_slope for stream orders > 1

summary(nsi_rcm_phys_qaqc_dat %>% 
          filter(stream_order>1 & ctch_basin_slope == 0))

# 74 datapoints, now let's check how many of those zeroes correspond to an elevation
# difference of zero

summary(nsi_rcm_phys_qaqc_dat %>% 
          filter(stream_order>1 & ctch_basin_slope == 0) %>% 
          filter(ctch_max_elevation_m - ctch_min_elevation_m!=0)) # we have a total of
# 61 zero values that are not accounted by lack of elevation changes

# we are going to build a multiple linear model to fill in the gaps
missing_data <- nsi_rcm_phys_qaqc_dat %>% 
  filter(stream_order>1 & ctch_basin_slope == 0) %>% 
  filter(ctch_max_elevation_m - ctch_min_elevation_m!=0) %>% 
  select(comid,
         ctch_basin_slope,
         ctch_max_elevation_m,
         ctch_min_elevation_m,
         basin,
         ctch_area_km2,
         reach_length_km,
         ctch_avg_elevation_m)


remaining_data <- nsi_rcm_phys_qaqc_dat %>%
  anti_join(missing_data, by = "comid") %>% 
  select(comid,
         ctch_basin_slope,
         ctch_max_elevation_m,
         ctch_min_elevation_m,
         basin,
         ctch_area_km2,
         reach_length_km,
         ctch_avg_elevation_m)


ctch_basin_mod <- lm(log(ctch_basin_slope)~log(ctch_max_elevation_m - ctch_min_elevation_m) + basin +
                       log(ctch_area_km2) + log(reach_length_km) + log(ctch_avg_elevation_m),
     data = nsi_rcm_phys_qaqc_dat %>% 
       filter((ctch_max_elevation_m - ctch_min_elevation_m)>0) %>% 
       filter(stream_order > 1 & ctch_basin_slope !=0),
     na.action = na.omit)
summary(ctch_basin_mod)

# We will fill the in ctch_basin_slope with the model above

# Predict the missing 'ctch_area_km2' values using the loess model
predicted_values <- exp(predict(ctch_basin_mod, newdata = missing_data))

# Update the missing 'ctch_area_km2' values with the predicted values
missing_data$ctch_basin_slope <- predicted_values

# Combine the original data and the data with predicted values
estimated_ctch_data <- rbind(remaining_data,
                             missing_data)

# Replace original values in the dataset for their estimates:
nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat%>% 
  select(-ctch_basin_slope) %>% 
  merge(.,
        estimated_ctch_data %>% 
          select(comid,ctch_basin_slope),
        by = "comid",
        all.x = TRUE) %>% 
  mutate(ctch_basin_slope = if_else(ctch_basin_slope < 0.00000001,
                                    0.00000001,
                                    ctch_basin_slope))

summary(nsi_rcm_phys_qaqc_dat)

###############################################################################
# Filling gaps for slope and roughness with: interpolate_missing_values()

# Let's start with reach slope, which we expect should be related to ctch_area_km2
# and ctch_basin_slope

p <- ggplot(data = nsi_rcm_phys_qaqc_dat %>% 
              filter(reach_slope>0 & ctch_basin_slope>0),
            aes(x = ctch_area_km2,
                y = reach_slope,
                color = as.factor(stream_order)))+
  # geom_boxplot()+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  # geom_smooth(method = 'lm')+
  facet_wrap(~basin, ncol = 2)+
  theme(legend.position = "none")
p

# Not much of a relationship

# We will use a pair of functions to interpolate missing values (n<50) (e.g., roughness, reach_slope)
get_immediate_neighbors_median <- function(data, column, comid, tocomid) {
  immediate_neighbors <- c(comid, tocomid)
  values <- data[[column]][data$comid %in% immediate_neighbors & data[[column]] >= 0]
  median_value <- median(values, na.rm = TRUE)
  return(median_value)
}
interpolate_missing_values <- function(data, column) {
  column = sym(column)
  
  data <- data %>%
    mutate(!!column := ifelse(!!column < 0 | is.na(!!column), NA, !!column))
  
  for (i in seq_len(nrow(data))) {
    # Check if the column value is missing (represented by NA)
    if (is.na(data[[column]][i])) {
      # Get the immediate neighbors' mean value
      immediate_median <- get_immediate_neighbors_median(data, column, data$comid[i], data$tocomid.x[i])
      
      # If there are no immediate neighbors, replace with the average value for the same 'stream_order'
      if (is.na(immediate_median)) {
        same_stream_order <- data$stream_order == data$stream_order[i]
        same_order_values <- data[[column]][same_stream_order & !is.na(data[[column]])]
        immediate_median <- median(same_order_values, na.rm = TRUE)
      }
      
      # Assign the calculated value to the missing value
      data[[column]][i] <- immediate_median
    }
  }
  return(data)
}

roughness_int <- interpolate_missing_values(data = nsi_rcm_phys_qaqc_dat %>% 
                                              select(comid,
                                                     tocomid,
                                                     stream_order,
                                                     roughness),
                                            "roughness")

reach_slope_int <- interpolate_missing_values(data = nsi_rcm_phys_qaqc_dat %>% 
                                              select(comid,
                                                     tocomid,
                                                     stream_order,
                                                     reach_slope),
                                            "reach_slope")

nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat %>%
  select(-c(reach_slope,roughness)) %>% 
  merge(.,
        roughness_int %>% 
          select(comid,
                 roughness),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        reach_slope_int %>% 
          select(comid,
                 reach_slope),
        by = "comid",
        all.x = TRUE)

summary(nsi_rcm_phys_qaqc_dat)


write.csv(nsi_rcm_phys_qaqc_dat,paste(local_data,"wyrb_hydrophysical_data.csv",sep = '/'),
          row.names = FALSE)



################################################################################

# Adding percentage forest to the model
nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat %>%
  mutate(pred_d50_m = exp(predict.lm(d50_mod4,.)),
         pred_d50_m = if_else(pred_d50_m < 0.00001,
                              0.00001,if_else(pred_d50_m>4.000,
                                              4.000,pred_d50_m)))

summary(nsi_rcm_phys_qaqc_dat)

p <- ggplot(data = nsi_rcm_phys_qaqc_dat %>%
              filter(is.na(d50_m)== FALSE),
            aes(x = d50_m,
                y = pred_d50_m,
                color = log(reach_slope)))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()+
  facet_wrap(~basin, ncol = 2)
p
  

nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat %>%
  merge(.,
        nlcd_2001_dat %>% 
          select(comid,
                 TOT_NLCD01_42) %>% 
          rename(pct_forest = TOT_NLCD01_42),
        by = "comid",
        all.x = TRUE) %>% 
  mutate(pct_forest_1 = pct_forest + 1)

d50_mod2 <- lm(log(d50_m)~(log(bnkfll_width_m)+log(reach_slope)+log(mean_ann_flow_m3s)+log(wshd_area_km2))*basin+stream_order,
               data = nsi_rcm_phys_qaqc_dat,
               na.action = na.omit)

summary(d50_mod2)

nsi_rcm_phys_qaqc_dat <- nsi_rcm_phys_qaqc_dat %>% 
  mutate(pred_d50_m = exp(predict.lm(d50_mod2,.)),
         pred_d50_m_lj = exp(predict.lm()))

summary(nsi_rcm_phys_qaqc_dat)

p <- ggplot(data = nsi_rcm_phys_qaqc_dat %>% 
              filter(is.na(d50_m)==FALSE),
            aes(x = as.factor(stream_order),
                y = pred_d50_m,
                color = as.factor(stream_order)))+
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p


summary(nsi_rcm_phys_qaqc_dat)

p <- ggplot(data = nsi_rcm_phys_qaqc_dat %>% 
              filter(is.na(d50_m)==FALSE),
            aes(x = d50_m,
                y = pred_d50_m))+
  geom_point(alpha = 0.35)+
  geom_abline(slope = 1, 
              color = "darkred", 
              linetype = "dashed")+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Existing D50(m) values",
       y = "Predicted D50(m) values")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
p

bnkfll_dat <- nsi_rcm_phys_qaqc_dat %>% 
  filter(is.na(d50_m)==FALSE) %>% 
  mutate(bnkfll_lj = 3.004+mean_ann_flow_m3s^0.426*reach_slope^-0.153*d50_m^-0.002)
summary(bnkfll_dat)


p <- ggplot(data = bnkfll_dat,
            aes(x = bnkfll_width_m,
                y = bnkfll_lj,
                color = log(reach_slope)))+
  geom_point()+
  geom_abline()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

p <- ggplot(data = bnkfll_dat,
            aes (x = bnkfll_lj,
                 y = d50_m))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
p

d50_mod3 <- lm(log(d50_m) ~ log(mean_ann_flow_m3s) + log(bnkfll_lj) + log(reach_slope),
               data = bnkfll_dat,
               na.action = na.omit)
summary(d50_mod3)


d50_mod4 <- lm(log(d50_m) ~ log(mean_ann_flow_m3s) + log(bnkfll_width_m) + log(reach_slope),
               data = bnkfll_dat,
               na.action = na.omit)
summary(d50_mod4)
