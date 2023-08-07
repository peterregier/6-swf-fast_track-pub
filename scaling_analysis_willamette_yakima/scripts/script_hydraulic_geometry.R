################################################################################
# Scaling watershed function: Data Analysis-Hydraulic Geometry
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

scaling_int_dat <- read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima/data/interpolated_scaling_resp_dat.csv",
                               show_col_types = FALSE)


local_data <- "./data"

results <- "./results"

# Checking relationship between watershed area and cumulative stream area

stream_area_plot <- ggplot(data = scaling_int_dat,
            aes(x = wshd_area_km2,
                y = accm_stream_area_m2))+
  geom_point(alpha = 0.05)+
  geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
stream_area_plot

# Checking scaling exponents

sa_exp_mod <- lm(log(accm_stream_area_m2)~log(wshd_area_km2),
                 data = filter(scaling_int_dat,basin == "yakima"))
confint(sa_exp_mod)

# Confidence interval [1.083248, 1.089011] does not include 1.00 for Willamette
# Confidence interval [1.095475, 1.106150] does not include 1.00 for Yakima

# Predicting median particle size with downstream hydraulic geometry

summary(scaling_int_dat$d50_m)

# We have 1095 NAs and with a minimum value of 1e-06

# Let's look at a plot of D50 vs stream order and watershed area.
d50_plot_1 <- ggplot(data = scaling_int_dat,
                       aes(x = as.factor(stream_order),
                           y = d50_m,
                           color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.5)+
  scale_y_log10()+
  labs(x = "Stream order (Strahler)", y = "Median particle size (m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
d50_plot_1


d50_plot_2 <- ggplot(data = scaling_int_dat,
                       aes(x = wshd_area_km2,
                           y = d50_m,
                           color = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Watershed area (km2)", y = "Median particle size (m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
d50_plot_2


# We can observe that the data is constrained by minimum and max values. We run 
# a regression based on Lee and Julien (2006) which based on hydraulic geometry 
# principles defines an equivalence between channel width and D50. We will run an 
# analysis that include the bounded values and another without them.

d50_mod <- lm(log(d50_m)~(log(bnkfll_width_m)+log(reach_slope)+
           log(mean_ann_flow_m3s)+log(wshd_area_km2))*basin+stream_order,
              data = scaling_int_dat,
              na.action = na.omit)

summary(d50_mod)


# And replace d50 values across our dataset

scaling_int_dat <-  scaling_int_dat %>% 
  mutate(pred_d50_m = exp(predict.lm(d50_mod,.)))
summary(scaling_int_dat)


# Comparing exisiting vs. predicted d50 data

p <- ggplot(data = scaling_int_dat %>% 
              filter(is.na(d50_m)==FALSE),
            aes(x = d50_m,
                y = pred_d50_m,
                color = log(mean_ann_runf_mm)))+
  geom_point(alpha = 0.55)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

# mean annual runoff separates at least two groups of values in Yakima River Basin

p <- ggplot(data = scaling_int_dat %>% 
              filter(is.na(d50_m)==FALSE),
            aes(x = d50_m,
                y = pred_d50_m,
                color = log(mean_ann_runf_mm)))+
  geom_point(alpha = 0.55)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "darkred",
              linetype = "dashed",
              linewidth = 1)+
  facet_wrap(basin~stream_order, nrow = 2)+
  theme_minimal()
p

# As expected the predictions perform better for larger particle sizes (i.e. above 1e-5)

# D50 changes with stream order
p <- ggplot(data = scaling_int_dat, 
            aes(x = as.factor(stream_order),
                y = pred_d50_m,
                color = as.factor(stream_order)))+
  geom_boxplot(alpha = 0.55)+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()
p

# Pattern with stream order is maintained

d50_plot_2 <- ggplot(data = filter(scaling_int_dat, d50_m > 0.00001),
                     aes(x = wshd_area_km2,
                         y = pred_d50_m,
                         color = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  scale_x_log10()+
  scale_y_log10()+
  labs(x = "Watershed area (km2)", y = "Predicted Median particle size (m)")+
  facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none")
d50_plot_2

# And we observe less bias in the data

# Now let's calculate the expected value for stream width according to downstream
# hydraulic geometry

scaling_int_dat <-  scaling_int_dat %>% 
  mutate(theor_stream_width_m = 1.33*(mean_ann_flow_m3s)^0.44 * (pred_d50_m)^-0.11 * (reach_slope)^-0.22,
         theor_stream_area_m2 = reach_length_km*1000*theor_stream_width_m,
         accm_theor_stream_area_m2 = calculate_arbolate_sum(data.frame(ID = comid,
                                                                       toID = tocomid,
                                                                       length = theor_stream_area_m2))) 


# Checking relationship between watershed area and cumulative stream area

# Checking scaling exponents

sa_exp_mod <- lm(log(accm_theor_stream_area_m2)~log(wshd_area_km2),
                 data = filter(scaling_int_dat,basin == "yakima"))
summary(sa_exp_mod)
confint(sa_exp_mod)

area_inset <- ggplot(data = scaling_int_dat,
                     aes(x =wshd_area_km2,
                         y = accm_theor_stream_area_m2,
                         color = basin))+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "black",
              linetype = "dashed",
              linewidth = 1,
              intercept = 3.0)+
  geom_smooth(method = 'lm',
              fullrange = TRUE)+
  # facet_wrap(~basin, ncol = 2)+
  theme_minimal()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "gray88"),
        axis.title = element_blank(),
        axis.text = element_blank())
area_ins <- ggplotGrob(area_inset)

area_plot <- ggplot(data = scaling_int_dat,
            aes(x =wshd_area_km2,
                y = accm_theor_stream_area_m2,
                color = basin))+
  geom_point(alpha = 0.50)+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(color = "black",
              linetype = "dashed",
              linewidth = 1,
              intercept = 3.0)+
  annotation_custom(grob = area_ins, xmin = -2, xmax = 0.5, ymin = 6.5, ymax = 9.5) +
  annotate("text", x = 1, y = 9200000,
            label = "paste(italic(R) ^ 2, \" ~ .95\")", parse = TRUE)+
  annotate("text", x = 0.008, y = 1250000,
           label = "Scaling exponents \nYakima = 95% c.i. [1.19 -1.20]\nWillamette = 95% c.i.[1.13 - 1.14]",
           hjust = 0)+
  theme_minimal()+
  theme(legend.position = "right")
area_plot

svglite::svglite(file = paste(results, paste0("guerrero_etal_23_stream_sa_scaling.svg"),sep = '/'),
                 width = 12,
                 height = 12,
                 bg = "transparent")
print(area_plot)
dev.off()

# Confidence interval [1.145193, 1.153920] does not include 1.00 for Willamette
# Confidence interval [1.200390, 1.213823] does not include 1.00 for Yakima

# Saving New dataset
scaling_hydraul_geom_dat <- scaling_int_dat  

write.csv(scaling_hydraul_geom_dat,paste(local_data,"hydraulic_geom_scaling_resp_dat.csv", sep = '/'),
          row.names = FALSE)
