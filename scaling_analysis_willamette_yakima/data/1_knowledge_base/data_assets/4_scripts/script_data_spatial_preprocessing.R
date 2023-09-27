###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima and Willamette River 
# Basins
# DATA PRE-PROCESSING
###############################################################################
# RESPIRATION DATA
###############################################################################

#By : Francisco Guerrero (Modified from Kyongho Son)
#Data source: SWAT-NEXXS Model simulations (By Kyongho Son)

librarian::shelf(sp,
                 sf,
                 raster,
                 rgdal,
                 rasterVis,
                 rgeos,
                 lattice,
                 grid,
                 spatstat,
                 plotKML,
                 fasterize,
                 egg,
                 nhdplusTools,
                 nhdR,
                 colorspace,
                 stars,
                 pals,
                 foreign,
                 tidyverse,
                 readr,
                 chron,
                 lubridate,
                 hydroGOF,
                 gtools,
                 zoo,
                 openair,
                 leaflet,
                 tidyverse)



#Note: Verify that the sf package was successfully installed. Otherwise try install.packages("sf)
#and answer "no" to the following prompt:

#Do you want to install from sources the package which needs compilation? (Yes/no/cancel) 

set.seed(2703)

#########################################################################################################
# Import / Export of assets

# External Import
#TBD

# Local import
raw_data <- "raw"
input_data <- "raw/pre-processing/model_inputs" 
processed_data <- "processed"

############ loading NHDPlus Data (Shapefiles) ##################################################

# For compatibility with the latest NHDPlus database, the coordinate system for the 
# original shapefiles has to be modified from NAD83 to WGS84. We do so by using the 
# st_transform() function from the package sf

# Shapefiles
# Yakima River Basin
nhd_yrb_stream<-st_transform(st_read(paste(input_data,"shapes/nhd_CR_stream_sub9.shp",sep = "/")),4326)
# Willamette River Basin
nhd_wrb_stream<-st_transform(st_read(paste(input_data,"shapes/nhd_CR_stream_sub8.shp",sep = "/")),4326)

# The willamette file contains two extra variables: Name and HUC4. Adding these columns to 
# the yakima file

nhd_yrb_stream <-  nhd_yrb_stream %>% 
  mutate(Name = "Yakima",
         HUC4 = 1703) 

# csv files with physical hydrological data
nhdp_2_pnw_raw <- read_csv(paste(raw_data,"230410_hydro_info_pnw.csv", sep = "/"),
                           show_col_types = FALSE)

nhd_wrb_map <- nhd_wrb_stream %>% 
  left_join(.,
            nhdp_2_pnw_raw %>% 
              filter(HUC_4 == 1709) %>% 
              dplyr::select(ComID,
                            LENGTHKM,
                            StreamOrde) %>% 
              mutate(COMID = ComID),
            by = "COMID") %>% 
  filter(is.na(ComID)==FALSE)

# 26 COMIDs extra in Shapefile

nhd_yrb_map <- nhd_yrb_stream %>% 
  left_join(.,
            nhdp_2_pnw_raw %>% 
              filter(HUC_4 == 1703) %>% 
              dplyr::select(ComID,
                            LENGTHKM,
                            StreamOrde) %>% 
              mutate(COMID = ComID),
            by = "COMID") %>% 
  filter(is.na(ComID)==FALSE)

# Using leaflet to look into maps:

# With the function 'addTiles' you get a base map by default that contain streets names
# and major features. If you want  a different base map, here is hwo to do it:

# A tutorial video: https://www.youtube.com/watch?v=sX9jwUAXShs

# OR follow this instructions:  
# Go to the website: https://leaflet-extras.github.io/leaflet-providers/preview/
# Look through the gallery and pick the base map you'd like to add
# Copy the name that appears below "Provider names for leaflet-providers.js" on the top
# dialogue box
# Call the function 'addProviderTiles' and paste the name chosen between ""
# In the chunk below I chose "Esri.WorldImagery"

# Willamette River Basin
leaflet(nhd_wrb_map) %>% 
  addPolylines(weight = 3) %>% 
  addPolylines(data = filter(nhd_wrb_map,is.na(StreamOrde)),
               color = "magenta",
               opacity = 1,
               weight = 3) %>% 
  addProviderTiles("Esri.WorldImagery")
# addTiles()

# Yakima River Basin
leaflet(nhd_yrb_map) %>% 
  addPolylines(weight = 3) %>% 
  addPolylines(data = filter(nhd_yrb_map,is.na(StreamOrde)),
               color = "magenta",
               opacity = 1,
               weight = 3) %>% 
  addProviderTiles("Esri.WorldImagery")
# addTiles()

# 60 COMIDs extra in shapefile

# We are going to merge these data sets, but we need to add and remove a few
# columns first

nhd_ywb_stream <- rbind(dplyr::select(nhd_yrb_stream,COMID,ID,geometry,Name),
                        dplyr::select(nhd_wrb_stream,COMID,ID,geometry,Name))

# Checking original coordinate reference system
st_crs(nhd_ywb_stream)

# Using leaflet package
leaflet(nhd_ywb_stream) %>% 
  addPolylines() %>% 
  addTiles()



# We will now load all the substrate concentration data for the Columbia River Basin to then extract
# values for both the YRB and WRB

stream_annDO<-read_csv(paste(assets_data,"nhd_CR_stream_annual_DO.csv",sep="/"),show_col_types = FALSE)
stream_annno3<-read.csv(paste(assets_data,"nhd_CR_stream_no3.csv",sep="/"),header=T,sep=',',skip=0)
stream_annDOC<-read_csv(paste(assets_data,"nhd_CR_stream_annual_DOC.csv",sep="/"),show_col_types = FALSE)
stream_nexss<-read_csv(paste(assets_data,"nexss_inputs.csv",sep="/"),show_col_types = FALSE)

# We need to rename the COMID in nexss:
stream_nexss <- rename(stream_nexss,
                       COMID = comid_nhd)

## merging the model input data with NHDPLUS stream reach shapefiles
# from : https://www.statology.org/merge-multiple-data-frames-in-r/

df_list <- list(nhd_ywb_stream,
                stream_annDO,
                stream_annDOC,
                stream_annno3,
                stream_nexss)

nhd_ywb_resp <- df_list %>% reduce(full_join, by = "COMID")

nhd_ywb_resp <- rename(nhd_ywb_resp,
                       id = ID,
                       name = Name,
                       stream_do_mg_l = `Stream DO`,
                       stream_doc_mg_l = `Stream DOC`)


# Saving data set as processed data

write_csv(nhd_ywb_resp,file=paste(assets_processed,"230313_wlm_ykm_stream_resp_dat.csv",sep = "/"))


# figures<-"ESS-DIVE/figures"

# jpeg("Stream DOC_YRB_annual_DOC.jpeg", width = 6, height = 6, units = 'in', res = 300) 
# par(cex.main=1.5,cex.axis=1.5) 
# plot(nhd_yrb_stream_resp[,"Stream DOC"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=brewer.reds(10),reset=FALSE)
# # plot(st_geometry(nhd_yrb_stream_resp),add=T)
# 
# title("(a) Stream DOC (mg/l)",line=-24, adj = 0.2)
# 
# 
# dev.off()