###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River Basins
# FIGURES
###############################################################################

#By : Francisco Guerrero
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

# SETTINGS
# Loading packages:

# Run for the first time only
# install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

# Also, you may need to install the GIT credential manager following the instructions
# from: https://github.com/GitCredentialManager/git-credential-manager/blob/main/README.md
gc()

rm()

librarian::shelf(tidyverse,# for plotting
                 plot3D,# for 3D plots
                 plot3Drgl,# for interactive 3D plots
                 rgl,# required by plot3Drgl
                 entropy,#Information theory calculations
                 GGally,#pair plots
                 scales,# manipulating log scales
                 stringr,# editing text
                 Hmisc,# Harrell's miscellaneaous for stats
                 gtable,# To manipulate ggplot objects
                 car,#partial residual plots
                 caret,#variable import
                 MuMIn, #ensemble models for partial residuals
                 ggeffects,
                 sp,#reading shape files
                 sf,# reading shape files
                 leaflet,#creating html widget maps
                 fedmatch,# customizable merge
                 svglite,#save plots as svg files (lighter?)
                 knitr,#compiling r-makrdown/quarto docs
                 nhdplusTools)#across the network calculation

theme_httn<-  theme(axis.text=element_text(colour="black",size=22),
                    axis.title = element_text(size = 32, face = "bold"),
                    panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
                    panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
                    panel.border = element_rect(fill=NA, colour = "black", linewidth = 1.5),
                    panel.background=element_rect(fill="white"),
                    axis.ticks.length = unit(0.254, "cm"),
                    axis.ticks = element_line(colour = "black", linewidth = 1), 
                    axis.line = element_line(colour = "black"),
                    legend.position = c(0.85,0.25),
                    legend.direction = "vertical",
                    legend.background = element_blank(),
                    legend.key.size = unit(1.0, 'lines'),#Changing spacing between legend keys
                    legend.title = element_text())


# Creating breaks for logarithmic scale 
# (see: https://r-graphics.org/recipe-axes-axis-log)

breaks <- 10^(-10:10)
breaks_c <- 10^seq(-10,10,by=4)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

set.seed(2703)

# Landscape colors from nlcd color scale
nlcd_colors_c <- c("#5475a8","#f00f00","#d2cdc0","#38814e","#dcca8f","#fbf65d")
nlcd_colors_w <- c("#5475a8","#f00f00","#d2cdc0","#38814e","#dcca8f","#fbf65d")

# Source: https://github.com/BuzzFeedNews/us-land-cover/blob/master/nlcd_legend.csv

# NLDC Categories
nlcd_cat_c <- c("c_water_scp","c_human_scp","c_barren_scp","c_forest_scp","c_shrub_scp","c_grass_scp")
nlcd_cat_w <- c("w_water_scp","w_human_scp","w_barren_scp","w_forest_scp","w_shrub_scp","w_grass_scp")

# Assigning names to a color scale
names(nlcd_colors_c) <- nlcd_cat_c
names(nlcd_colors_w) <- nlcd_cat_w

# Creating a quasi-sequential color palette for discrete categories
# Source: https://www.ibm.com/design/language/color/

my_dcolors <- c("#a6c8ff","#78a9ff","#4589ff","#0f62fe",
                "#00539a","#003a6d","#012749","#061727")

my_rcolors <- c("#fff1f1","#ffd7d9","#ffb3b8","#fa4d56",
                "#da1e28","#a2191f","#750e13","#2d0709")

my_mcolors <- c("#ffd6e8","#ffafd2","#ff7eb6","#ee5396",
                "#d62670","#9f1853","#740937","#510224")


#Data:

scaling_hydraul_geom_dat <- read_csv("https://raw.githubusercontent.com/Scaling-Watershed-Function/2-swf-analytical.engine/main/scaling_analysis_willamette_yakima/data/hydraulic_geom_scaling_resp_dat.csv",
                               show_col_types = FALSE)
ctch_heterogeneity_dat <- read_csv("https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/datasets/processed_data/river_corridor_landscape_heterogeneity/data/catchment_landscape_heterogeneity_pnw.csv",
                                   show_col_types = FALSE)  
wshd_heterogeneity_dat <- read_csv("https://media.githubusercontent.com/media/Scaling-Watershed-Function/1-swf-knowledge.base/main/datasets/processed_data/river_corridor_landscape_heterogeneity/data/watershed_landscape_heterogeneity_pnw.csv",
                                   show_col_types = FALSE)



local_data <- "./data"

results <- "./results"

###### ADD DATA dictionary here!

# Merging data

scaling_analysis_dat <- scaling_hydraul_geom_dat %>% 
          mutate(d50_m = pred_d50_m,
                 stream_width_m = theor_stream_width_m,
                 accm_stream_area_m2 = accm_theor_stream_area_m2,
                 stream_area_m2 = theor_stream_area_m2) %>% 
  select(-c(pred_d50_m,
            theor_stream_width_m,
            accm_theor_stream_area_m2,
            theor_stream_area_m2)) %>% 
  merge(.,
        wshd_heterogeneity_dat %>% 
          select(comid,
                 forest_scp,
                 grass_scp,
                 shrub_scp,
                 water_scp,
                 human_scp,
                 barren_scp,
                 ht,
                 hmax,
                 hrel) %>% 
          rename(w_forest_scp = forest_scp,
                 w_grass_scp = grass_scp,
                 w_shrub_scp = shrub_scp,
                 w_water_scp = water_scp,
                 w_human_scp = human_scp,
                 w_barren_scp = barren_scp,
                 w_ht = ht,
                 w_hmax = hmax,
                 w_hrel = hrel),
        by = "comid",
        all.x = TRUE) %>% 
  merge(.,
        ctch_heterogeneity_dat %>% 
          select(comid,
                 forest_scp,
                 grass_scp,
                 shrub_scp,
                 water_scp,
                 human_scp,
                 barren_scp,
                 ht,
                 hmax,
                 hrel) %>% 
          rename(c_forest_scp = forest_scp,
                 c_grass_scp = grass_scp,
                 c_shrub_scp = shrub_scp,
                 c_water_scp = water_scp,
                 c_human_scp = human_scp,
                 c_barren_scp = barren_scp,
                 c_ht = ht,
                 c_hmax = hmax,
                 c_hrel = hrel),
        by = "comid",
        all.x = TRUE) 
  
summary(scaling_analysis_dat)

# Checking connectivity in the dataset

test_dat_connectivity <- scaling_analysis_dat %>% 
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

# Generating dataset for RF model

rf_scaling_analyst_dat <- scaling_analysis_dat %>% 
  mutate(logQ_m3_div_s = log10(mean_ann_flow_m3s),
         logDA_km2 = log10(wshd_area_km2),
         logw_m = log10(stream_width_m),
         length_m = reach_length_km*1000,
         logwbkf_m = log10(bnkfll_width_m),
         logdbkf_m = log10(bnkfll_depth_m),
         logSlope = log10(reach_slope),
         D50_m = d50_m)
  
summary(rf_scaling_analyst_dat)

# write.csv(rf_scaling_analyst_dat,paste(local_data,"rf_scaling_analysis_data.csv", sep = '/'),
#          row.names = FALSE)

################################################################################
# Calculating Quantiles

#I'm going to try calculating the quantiles with Hmisc::cut2, which allows
# for the inclusion of zeroes

# https://stackoverflow.com/questions/46750635/cut-and-quantile-in-r-in-not-including-zero

qlabel <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80+")

# asigning names to color scale
names(my_dcolors) <- qlabel
names(my_mcolors) <- qlabel

scaling_analysis_dat <- scaling_analysis_dat %>% 
  group_by(basin) %>% 
  mutate(ent_cat_w = factor(Hmisc::cut2(w_hrel, g = 8),labels = qlabel),
         ent_cat_c = factor(Hmisc::cut2(c_hrel, g = 8),labels = qlabel),
         rst_cat = factor(Hmisc::cut2(tot_rt_hz_s, g = 8),labels = qlabel),
         hzt_cat = factor(Hmisc::cut2(tot_q_hz_ms, g = 8),labels = qlabel),
         pct_cat = factor(Hmisc::cut2(mean_ann_pcpt_mm, g = 8),labels = qlabel),
         rnf_cat = factor(Hmisc::cut2(mean_ann_runf_mm, g = 8),labels = qlabel),
         sto_fct = as.factor(stream_order),
         basin = if_else(basin == "yakima",
                         "Yakima River",
                         "Willamette River"))

# write.csv(scaling_analysis_dat,paste(local_data,"scaling_analysis_quantiles_data.csv", sep = '/'),
#          row.names = FALSE)

################################################################################
# Raw plots
################################################################################

# Note: I'm saving these plots as svg files because is not only the appropriate
# format for plot resolution, but also because is three times ligther than the
# PNG version.

# To save any of these plots as a png you can use: 

# ggsave(file=paste(results, paste0("guerrero_etal_23_scaling_local_respiration_rates.png"),sep = '/'),
#        width = 18,
#        height = 12,
#        units = "in")

# With the correspondent modifications to the file name and dimensions. 

# Local respiration rates and stream order

local_rates_plot <- ggplot(data = scaling_analysis_dat,
                           aes(x = as.factor(stream_order),
                               y = totco2g_day/stream_area_m2,
                               color = sto_fct))+
  xlab(expression(bold("Stream order (Strahler)")))+
  ylab(expression(bold(paste("Local respiration rates"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)+
  theme_httn+
  theme(legend.position = "none",
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))
local_rates_plot
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_scaling_local_respiration_rates.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(local_rates_plot)
dev.off()

# Local respiration rates and runoff

local_rates_runf_plot <- ggplot(data = scaling_analysis_dat,
                                aes(x = wshd_area_km2,
                                    y = totco2g_day/stream_area_m2,
                                    color = rnf_cat))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
  ylab(expression(bold(paste("Local respiration rates"," ","(",gCO[2]*m^-2*d^-1,")"))))+
  scale_color_manual(values = my_dcolors, name = "Mean annual \nrunoff (mm) \n(quantiles)") +
  facet_wrap(~basin, ncol = 2)+
  theme_httn+
  theme(legend.position = "right",
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))
local_rates_runf_plot
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_scaling_local_respiration_rates_runoff.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(local_rates_runf_plot)
dev.off()

# Raw scaling plot with missing values

raw_dat_plot <- ggplot(data = scaling_analysis_dat,
                     aes(x = wshd_area_km2,
                         y = accm_totco2g_day / wshd_area_km2,
                         color = rst_cat)) +
  geom_point(size = 2.5, alpha = 0.35) +
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(slope = 1, intercept = 3, linewidth = 2, linetype = "dashed") +
  scale_color_manual(values = my_dcolors) +
  xlab(expression(bold(paste("Watershed Area"," ","(", km^2, ")")))) +
  ylab(expression(bold(paste(" Cumulative"," ", Respiration[Sed],"(", gCO[2] * network^-1 * d^-1, ")")))) +
  guides(color = guide_legend(title = "Hyporheic \nresidence time (s) \n(quantiles)")) +
  facet_wrap(~basin, ncol = 2)+
  theme_httn+
  theme(legend.position = "right",
        plot.title = element_text(size = 16),
        strip.text = element_text(size = 16, face = "bold"))
local_rates_runf_plot
raw_dat_plot
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_scaling_cumulative_respiration_rates_residence_time.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(raw_dat_plot)
dev.off()

################################################################################
# Data sub-setting for plots
################################################################################

# For easier visualization we plot data from a reduced dataset that excludes 
# missing values for hyporheic variables

scaling_plot_dat <- filter(scaling_analysis_dat, is.na(tot_rt_hz_s)==FALSE)


################################################################################
# WATERSHED SCALING PLOTS
################################################################################
generate_inset_plot <- function(data, basin, color_var, color_scale, legend_title) {
  plot_data <- filter(data, basin == !!basin)
  
  quant_i <- ggplot(data = plot_data,
                    aes(x = wshd_area_km2,
                        y = accm_totco2g_day / wshd_area_km2,
                        color = .data[[color_var]])) +
    geom_smooth(method = "lm", fullrange = TRUE, alpha = 0.3) +
    scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
    scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
    xlab(expression(bold(paste("Watershed area"," ","(", km^2, ")")))) +
    ylab(expression(bold(paste("Sediment respiration"," ","(", gCO[2]*m^-2*d^-1, ")")))) +
    annotation_logticks(size = 0.75, sides = "tblr") +
    scale_color_manual(values = color_scale) +
    geom_abline(slope = 1, intercept = 2.5, linewidth = 2, linetype = "dashed") +
    guides(color = guide_legend(title = legend_title)) +
    theme_httn +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(size = 16),
          panel.background = element_blank()) +
    guides(alpha = "none")
  
  # Convert quant_i ggplot into grob
  quant_grob <- ggplotGrob(quant_i)
  
  return(quant_grob)
}

generate_plot <- function(data, basin, color_var, legend_title, color_scale, plot_title, faceting = FALSE) {
  if (faceting) {
    main_quant <- ggplot(data = data %>%
                           filter(basin == !!basin),
                         aes(x = wshd_area_km2,
                             y = accm_totco2g_day / wshd_area_km2,
                             color = .data[[color_var]])) +
      geom_point(size = 2.5, alpha = 0.35) +
      geom_point(data = data %>%
                   filter(basin == !!basin, .data[[color_var]] == "Q80+"),
                 aes(x = wshd_area_km2,
                     y = accm_totco2g_day / wshd_area_km2),
                 size = 2.5) +
      geom_abline(slope = 1, intercept = 3, linewidth = 2, linetype = "dashed") +
      scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
      scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
      scale_color_manual(values = color_scale) +
      xlab(expression(bold(paste("Watershed Area"," ","(", km^2, ")")))) +
      ylab(expression(bold(paste(" Cumulative"," ", Respiration[Sed],"(", gCO[2] * network^-1 * d^-1, ")")))) +
      guides(color = guide_legend(title = legend_title)) +
      annotation_logticks(size = 0.75, sides = "tblr") +
      theme_httn +
      theme(legend.position = "right",
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 18, face = "bold")) +
      ggtitle(plot_title) +
      facet_wrap(as.formula(paste("~", color_var)), ncol = 4)  # Faceting based on color_var (rst_cat)
  } else {
    quant_ins <- generate_inset_plot(data, basin, color_var, color_scale, legend_title)
    
    plot_data <- filter(data, basin == !!basin)
    
    main_quant <- ggplot(data = plot_data,
                         aes(x = wshd_area_km2,
                             y = accm_totco2g_day / wshd_area_km2,
                             color = .data[[color_var]])) +
      geom_point(size = 2.5, alpha = 0.35) +
      geom_point(data = plot_data %>%
                   filter(.data[[color_var]] == "Q80+"),
                 aes(x = wshd_area_km2,
                     y = accm_totco2g_day / wshd_area_km2),
                 size = 2.5) +
      geom_abline(slope = 1, intercept = 3, linewidth = 2, linetype = "dashed") +
      scale_x_log10(breaks = breaks, labels = trans_format("log10", math_format(10^.x))) +
      scale_y_log10(breaks = breaks_c, labels = trans_format("log10", math_format(10^.x))) +
      scale_color_manual(values = color_scale) +
      xlab(expression(bold(paste("Watershed Area"," ","(", km^2, ")")))) +
      ylab(expression(bold(paste(" Cumulative"," ", Respiration[Sed],"(", gCO[2] * network^-1 * d^-1, ")")))) +
      guides(color = guide_legend(title = legend_title)) +
      annotation_logticks(size = 0.75, sides = "tblr") +
      annotation_custom(grob = quant_ins, xmin = 2.25, xmax = 4.35, ymin = -1.25, ymax = 2) +
      theme_httn +
      theme(legend.position = c(0.15, 0.75),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 18, face = "bold")) +
      ggtitle(plot_title)
  }
  
  return(main_quant)
}
################################################################################
# Landscape entropy, residence time, hyporheic exchange, mean annual runoff, and scaling

# Yakima River Basin

# Landscape entropy
plot_basin_abbv <- "yrb"
plot_quant <- generate_plot(data = scaling_plot_dat,
                               basin = "Yakima River",
                               color_var = "ent_cat_w",
                               legend_title = "Landscape Entropy\n(quantiles)",
                               color_scale = my_mcolors,
                               plot_title = "Yakima River Basin",
                               faceting = FALSE)

print(plot_quant)
# Save the plot as an SVG file
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_scaling_respiration_entropy.svg"),sep = '/'),
                 width =12,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()

# If faceting = TRUE
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_facet_scaling_respiration_entropy.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()


# Mean Annual Runoff
plot_quant <- generate_plot(data = scaling_plot_dat,
                               basin = "Yakima River",
                               color_var = "rnf_cat",
                               legend_title = "Mean annual\nrunoff (mm)\n(quantiles)",
                               color_scale = my_dcolors,
                               plot_title = "Yakima River Basin",
                               faceting = TRUE)

print(plot_quant)
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_scaling_respiration_mean_runoff.svg"),sep = '/'),
                 width =12,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()

# If faceting = TRUE
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_facet_scaling_respiration_mean_runoff.svg"),sep = '/'),
                 width =20,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()

# Willamette River Basin

# Landscape entropy
plot_basin_abbv <- "wrb"
plot_quant <- generate_plot(data = scaling_plot_dat,
                            basin = "Willamette River",
                            color_var = "ent_cat_w",
                            legend_title = "Landscape Entropy\n(quantiles)",
                            color_scale = my_mcolors,
                            plot_title = "Willamette River Basin",
                            faceting = FALSE)

print(plot_quant)
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_scaling_respiration_entropy.svg"),sep = '/'),
                 width =12,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()

# If faceting = TRUE
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_facet_scaling_respiration_entropy.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()


# Mean annual runoff
plot_quant <- generate_plot(data = scaling_plot_dat,
                               basin = "Willamette River",
                               color_var = "rnf_cat",
                               legend_title = "Mean annual\nrunoff (mm)\n(quantiles)",
                               color_scale = my_dcolors,
                               plot_title = "Willamette River Basin",
                               faceting = FALSE)

print(plot_quant)
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_scaling_mean_runoff_entropy.svg"),sep = '/'),
                 width =12,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()

# If faceting = TRUE
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_facet_scaling_mean_runoff_entropy.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(plot_quant)
dev.off()

