###############################################################################
# Field Spatial Study - YRB Respiration Model Match
###############################################################################

# Author: Francisco J. Guerrero
# Date created: 04-20-2023

librarian::shelf(tidyverse)

# Local import
raw_data <- "raw"
input_data <- "raw/pre-processing/model_inputs" 
processed_data <- "processed"

# Data

spt_dat <- read_csv(paste(raw_data,"230110_yrb_spatial_camp.csv",sep = '/'),
                    show_col_types = FALSE)
fld_dat <- read_csv(paste(raw_data,"combined_results_updated_040623.csv", sep = '/'),
                    show_col_types = FALSE)
mod_dat <- read_csv(paste(raw_data,"230406_son_etal_22_results_zen.csv", sep = '/'),
                    show_col_types = FALSE)

colnames(fld_dat) <- c("SiteID",
                       "daysofdata",
                       "er_mean_go2_m2_day",
                       "gpp_mean_go2_m2_day",
                       "k600_mean_m_day",
                       "mean_depth_m",
                       "er_mean_go2_m3_day",
                       "gpp_mean_go2_m3_day",
                       "k600_v_ersq",
                       "k600_v_erp",
                       "er_water_go2_m2_day",
                       "er_water_go2_m3_day",
                       "er_sed_go2_m2_day",
                       "er_sed_go2_m3_day")


fld_mrg <- merge(spt_dat,
                 fld_dat,
                 by.x="site_ID",
                 by.y="SiteID",
                 all.x = TRUE)

fld_mod <- merge(fld_mrg,
                 mod_dat,
                 by.x = "COMID",
                 by.y = "comid")

# Local Scaling Relationships

fld_scl <- fld_mod %>% 
  filter(is.na(er_sed_go2_m2_day)==FALSE) %>% 
  ggplot(aes(x = tot_ups_area_km2,
             y = er_sed_go2_m2_day,
             color = time_type,
             size = -er_sed_go2_m2_day))+
  geom_point()+
  scale_x_log10()+
  # scale_y_log10()+
  facet_wrap(~time_type,ncol=3)
fld_scl

mod_scl <- fld_mod %>% 
  filter(is.na(er_sed_go2_m2_day)==FALSE) %>% 
  filter(is.na(time_type)==FALSE) %>% 
  ggplot(aes(x = tot_ups_area_km2,
             y = logtotco2g_m2_day,
             color = time_type,
             size = logtotco2g_m2_day))+
  geom_point()+
  scale_x_log10()+
  # scale_y_log10()+
  facet_wrap(~time_type,ncol=3)
mod_scl



a <- filter(mod_dat,
            comid=="24423347")
a
summary(a)
