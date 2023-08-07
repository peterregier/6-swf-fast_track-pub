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
                 nhdplusTools,#across the network calculation
                 htmlwidgets,
                 RCurl)

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


################################################################################
# Data:

local_data <- "./data"

results <- "./results"

# URL location
github_path <- "https://raw.githubusercontent.com"
organization <- "Scaling-Watershed-Function"
repository <- "2-swf-analytical.engine/main"
folder_path <- "scaling_analysis_willamette_yakima/data"
file_name <- "scaling_analysis_quantiles_data.csv"

scaling_analysis_dat <- read_csv(paste(github_path,
                                       organization,
                                       repository,
                                       folder_path,
                                       file_name,sep = '/'),
                                       show_col_types = FALSE)
################################################################################
# Data sub-setting for plots
################################################################################

# For easier visualization we plot data from a reduced dataset that excludes 
# missing values for hyporheic variables

scaling_plot_dat <- filter(scaling_analysis_dat, is.na(tot_rt_hz_s)==FALSE)


#########################################################################################
# Interaction between landscape heterogeneity, residence time, and hyporheic exchange
# as related to scaling behavior
#########################################################################################

# Forestcapes: forest_scp =forest_evg + forest_dcd + forest_mxd
# Grasslandscapes: grass_scp = grass
# Shrublandscapes: shrub_scp = shrub
# Waterscapes: water_scp = snow + water + wetland_wood + wetland_herb
# Humanscapes: human_scp = pasture + crops + developed_op + developed_lw + developed_md + developed_hg
# Barrenscapes: barren_scp = barren


landuse_scaling_dat <- scaling_plot_dat %>% 
  select(wshd_area_km2,
         basin,
         accm_totco2g_day,
         w_forest_scp,
         w_water_scp,
         w_barren_scp,
         w_shrub_scp,
         w_grass_scp,
         w_human_scp,
         ent_cat_w,
         w_hrel,
         mean_ann_runf_mm,
         rst_cat,
         hzt_cat,
         rnf_cat) %>% 
  mutate(p_frst_t = w_forest_scp + w_water_scp,
         p_anth_t = w_human_scp,
         p_grss_t = w_grass_scp,
         p_shrb_t = w_shrub_scp) %>% 
  gather(c(16:19),key="use",value = "fraction") %>% 
  mutate(use = fct_relevel(use,c("p_frst_t","p_shrb_t","p_grss_t","p_anth_t"))) %>% 
  arrange(use) 

# Creating the plot

new.labs <- c("10th-Percentile",
              "20th-Percentile",
              "30th-Percentile",
              "40th-Percentile",
              "50th-Percentile",
              "60th-Percentile",
              "70th-Percentile",
              "80th+-Percentile")

names(new.labs) <- c("Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80+")

generate_lnd_plot <- function(data,basin,wrap_cat_var,plot_title){
  ggplot_obj <- ggplot(data = data %>% 
                   filter(basin == !!basin),
                 aes(x = wshd_area_km2,
                     y = accm_totco2g_day/wshd_area_km2,
                     color = use))+
    facet_wrap(as.formula(paste("~", wrap_cat_var)), nrow = 2, labeller = labeller(!!wrap_cat_var := new.labs)) +
    geom_abline(slope=1.0, color = "red", linetype = "solid", linewidth  = 0.75)+
    geom_smooth(aes(x = wshd_area_km2,
                    y = accm_totco2g_day/wshd_area_km2),method = "lm", inherit.aes = FALSE,
                fullrange = TRUE, color = "black", size = 0.65, se = TRUE, fill = "gray",
                alpha = 0.7)+
    geom_point(aes(alpha = fraction),size = 2.5)+
    scale_alpha_continuous(guide = "none") + 
    scale_x_log10(breaks = breaks_c, 
                  labels = trans_format("log10", math_format(10^.x)))+
    scale_y_log10(breaks = breaks_c, 
                  labels = trans_format("log10", math_format(10^.x)))+
    xlab(expression(bold(paste("Watershed area"," ","(",km^2,")"))))+
    ylab(expression(bold(paste("Cumulative total respiration"," ","(",gCO[2]*km^-2*d^-1,")"))))+
    scale_color_manual(name = "Land use",
                       values = c("#008837","#FFC618","darkorange","#7b3294"),
                       labels = c("Forestscapes", "Shrublandscapes","Grasslandscapes","Humanscapes"))+
    annotation_logticks(size = 0.75, sides = "tblr")+
    theme_httn+
    theme(legend.position = "right",
          legend.text = element_text(size=12),
          legend.title = element_text(size=16),
          plot.title = element_text(size = 16),
          strip.text = element_text(size = 16, face = "bold", hjust = 0))+
    ggtitle(plot_title)
  
  return(ggplot_obj)
  
}


# Generate the plot using the function
plot_basin_abbv <- "wrb"
land_quant <- generate_lnd_plot(data = landuse_scaling_dat,
                                    basin = "Willamette River",
                                    wrap_cat_var = "ent_cat_w",
                                    plot_title = "Willamette River Basin")

print(land_quant)
svglite::svglite(file = paste(results, paste0("guerrero_etal_23_",plot_basin_abbv,"_facet_landuse_entropy.svg"),sep = '/'),
                 width = 20,
                 height = 12,
                 bg = "transparent")
print(land_quant)
dev.off()




# Entropy plots

generate_ent_plot <- function(data,basin,wrap_cat_var,plot_title){
  ggplot_obj <- ggplot(data = data %>% 
                         filter(basin == !!basin & fraction > 0.01),
                       aes(x = w_human_scp,
                           y = w_hrel,
                           color = use))+
    geom_point(aes(alpha = fraction),size = 2.5)+
    scale_alpha_continuous(range = c(0.1, 1.00), breaks = c(0, .25, .50, .75, 1.00))+
    # scale_x_log10(breaks = breaks_c,
    #               labels = trans_format("log10", math_format(10^.x)))+
    scale_x_log10(labels = label_comma())+
    xlab("Humanscapes (%)")+
    ylab("Landscape Heterogeneity \n(Shannon's entropy*")+
    scale_color_manual(values = c("#008837","#dfc27d","darkorange","#7b3294"))+
    theme_httn+
    theme(legend.position = "right",
          legend.text = element_text(size=12),
          legend.title = element_text(size=16))+
    ggtitle(plot_title)
  
  return(ggplot_obj)
  
}


# Yakima River Basin
plot_basin_abbv <- "wrb"
land_hzt_quant <- generate_ent_plot(data = landuse_scaling_dat,
                                    basin = "Yakima River",
                                    wrap_cat_var = "ent_cat_w",
                                    plot_title = "Yakima River Basin")

print(land_hzt_quant)