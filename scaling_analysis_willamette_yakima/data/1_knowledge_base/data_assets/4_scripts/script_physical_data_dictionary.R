################################################################################
# SCALING WATERSHED FUNCTION: Hydrogeomorphological Data Dictionary
################################################################################

#Author: Francisco J. Guerrero

# Loading required packages: 

librarian::shelf(tidyverse,
                 utils)

# Local Import-Export
raw_data <- "raw"
processed_data <- "processed"

# Hydrogeomorphological Characteristics 

# Data preparation script: "script_physical_data_wrangling"

phys_dat_ro <- read_csv(paste(raw_data,"230620_ord_basin_hydrogeom_swf.csv", sep = '/'),
                     show_col_types = FALSE)

variable <-  c("comid",
               "tocomid",
               "reachcode",
               "hydroseq",
               "from_node",
               "to_node",
               "huc_2_region_id",
               "huc_region_raster_id",
               "huc_4_subregion_id",
               "basin",
               "wshd_stream_dens",
               "wshd_basin_slope",
               "wshd_min_elevation_m",
               "wshd_max_elevation_m",
               "wshd_avg_elevation_m",
               "ctch_stream_dens",
               "ctch_basin_slope",
               "ctch_min_elevation_m",
               "ctch_max_elevation_m",
               "ctch_avg_elevation_m",
               "accm_basin_area_km2",
               "accm_basin_slope",
               "accm_min_elevation_m",
               "accm_max_elevation_m",
               "accm_avg_elevation_m",
               "accm_stream_slope",
               "accm_stream_dens",
               "mean_ann_pcpt_mm",
               "mean_ann_temp_dc",
               "mean_ann_runf_mm",
               "ctch_area_km2",
               "wshd_area_km2",
               "stream_order",
               "reach_type",
               "reach_length_km",
               "tot_stream_length_km",
               "reach_slope_length_km",
               "reach_slope",
               "roughness",
               "sinuosity",
               "bnkfll_width_m",
               "bnkfll_depth_m",
               "bnkfll_xsec_area_m2",
               "mean_ann_flow_m3s",
               "mean_ann_vel_ms")

orig_name <- c("comid",
               "tocomid", #bldg_23
               "reachcode",
               "hydroseq",
               "FromNode",#schz19
               "ToNode",
               "vpuid",#bldg_23 (*assigned based on first four digits of reachcode)
               "rpuid",
               "huc_4*",
               "basin*",
               "TOT_STREAM_DENS",#wczk21
               "TOT_BASIN_SLOPE",
               "TOT_ELEV_MIN",
               "TOT_ELEV_MAX",
               "TOT_ELEV_MEAN",
               "CAT_STREAM_DENS",
               "CAT_BASIN_SLOPE",
               "CAT_ELEV_MIN",
               "CAT_ELEV_MAX",
               "CAT_ELEV_MEAN",
               "ACC_BASIN_AREA",
               "ACC_BASIN_SLOPE",
               "ACC_ELEV_MIN",
               "ACC_ELEV_MAX",
               "ACC_ELEV_MEAN",
               "ACC_STREAM_SLOPE",
               "ACC_STRM_DENS",
               "PrecipV",#schz_19
               "TempV",
               "RunOffV",
               "areasqkm",#bldg_23
               "totdasqkm",
               "streamorde",
               "Ftype",
               "lengthkm",
               "arbolatesu",
               "slopelenkm",
               "slope",
               "roughness",
               "sinuosity",#wczk21
               "BANKFULL_WIDTH",
               "BANKFULL_DEPTH",
               "BANKFULL_XSEC_AREA",
               "MAFlowUcfs",#schz19
               "MAVelUfps")
              
description = c("Unique feature identifier from NHDPlus source data. USGS defined",
                "integer derived from tonode/fromnode topology of NHDPlusV2, E2NHDPlusV2, and tocomid attributes of the NWMv2.1 routelink file",
                "Unique flowline identifier. The first eight digits are the Watershed Boundary Dataset(WBD) HUC8.The next six digits are randomly assigned, sequential numbers that are unique within a HUC8.",
                "Hydrosequence number (assigned in ascending order)",
                "Original NHDPlus V2 from node identifier",
                "Original NHDPlus V2 to node identifier",
                "Vector Processing Unit ID (For flowline features)",
                "Raster Processing Unit ID (For landscape features)",
                "4-Digit Hydrologic Unit Code (*assigned based on first four digits of reachcode)",
                "River Basin (*assigned based on first four digits of reachcode)",
                "Flowline catchment stream density calculated as all upstream stream lengths (meters) divided by all upstream catchment areas (square kilometers). Upstream is defined by total upstream routing.",
                "Average slope in percent of all upstream NHDPlusV2 flowline catchments, based on total upstream routing.",
                "Minimum elevation in meters of all upstream NHDPlusV2 flowline catchments, based on total upstream routing.",
                "Maximum elevation in meters of all upstream NHDPlusV2 flowline catchments, based on total upstream routing.",
                "Mean elevation in meters of all upstream NHDPlusV2 flowline catchments, based on total upstream routing.",
                "Flowline catchment stream density calculated as stream length (meters) divided by catchment(s) area (square kilometers).",
                "NHDPlusV2 flowline catchment's average slope in percent.",
                "NHDPlusV2 flowline catchment's minimum elevation in meters.",
                "NHDPlusV2 flowline catchment's maximum elevation in meters.",
                "NHDPlusV2 flowline catchment's mean elevation in meters.",
                "Accumulated upstream area of NHDPlusV2 flowline catchments in square kilometers, based on divergence routing.",
                "Average slope in percent of all upstream NHDPlusV2 flowline catchments, based on divergence routing.",
                "Minimum elevation in meters of all upstream NHDPlusV2 flowline catchments, based on divergence routing.",
                "Maximum elevation in meters of all upstream NHDPlusV2 flowline catchments, based on divergence routing.",
                "Mean elevation in meters of all upstream NHDPlusV2 flowline catchments, based on divergence routing.",
                "Average slope in percent NHDPlusV2 flowlines, based on divergence routing",
                "Flowline catchment stream density calculated as all upstream stream lengths (meters) divided by all upstream catchment areas (square kilometers). Upstream is defined by divergence routing.",
                "Mean annual precitipation (mm)* orignal values in mm*100",
                "Mean annual temperature (degree Celsius)* original values in C*100",
                "Mean annual runoff (mm)",
                "Total catchment area (local drainage to flowline)",
                "Total drainage area recalculated with nhdplus Tools",
                "Modified Strahler stream order",
                "An NHDFlowline feature that is part of a series of consecutive flowlines that does not ultimately flow to a coast and has an FType of StreamRiver, Artificial Path, or Connector; otherwise",
                "Flowline length. USGS Defined",
                "The sum of the lengths of all digitized flowlines upstream from the downstream end of the immediate flowline, in kilometers",
                "Flow line length used to calculate slope in kilometers (in some cases differents from lengthkm",
                "Slope of the flowline from smoothed elevation (unitless-in m/m, instead of m/km as reported in NHDPlus 2.0)",
                "Numeric Manning's N estimate for flowline",
                "Flowline reach's sinuosity calculated as the reach length (in meters) divided by its Euclidean distance (straight line in meters). Straight-line length is measured from the beginning node of a reach to the end node of the reach.",
                "Estimated bankfull width of NHDPlus version 2.1's flowline reach calculated using Bieger 's regression equation (Bieger et al, 2015)",
                "Estimated bankfull depth of NHDPlus version 2.1's flowline reach calculated using Bieger 's regression equation (Bieger et al, 2015)",
                "Estimated bankfull cross sectional area of NHDPlus version 2.1's flowline reach calculated using Bieger 's regression equation (Bieger et al, 2015)",
                "Cumulative mean annual flow using unit flow runoff method",
                "Stream velocity at mean annual flow")
                
references = c("Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Schwarz, G. E., 2019",
              "Schwarz, G. E., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Schwarz, G. E., 2019",
              "Schwarz, G. E., 2019",
              "Schwarz, G. E., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Blodgett, 2023; Moore et al., 2019",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Wieczorek, et al., 2018-Select Basin Characteristics",
              "Schwarz, G. E., 2019",
              "Schwarz, G. E., 2019")

data_dictionary <- tibble(variable,
                              orig_name,
                              description,
                              references)
              
write.csv(data_dictionary,paste(processed_data,"230620_dd_basin_char_hydr_geom_swf.csv", sep = '/'),
          row.names = FALSE)





