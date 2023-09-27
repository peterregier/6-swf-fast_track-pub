# reading nhd databases
# reading NHD data for each stream orders
#https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47
#Select Attributes for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds for the Conterminous United States
library(chron)
library(lubridate)
library(hydroGOF)
library(gtools)
library(zoo)
library(openair)

## yakima river basin shapefile
home<-getwd()

home_gis<-"C:/Project/SFA/Columbia_SWAT/NHD_YRB_SWAT/codes/RC2sampling/gis"

## nhc_CR_stream_sub9 ## old nhdplus stream networks
## so updated one (nhd_yrb_stream3_vaa_wgs84.shp") was used.
#file_yakima<-"nhd_CR_stream_sub9.shp"
#nhd_sub9_stream<-st_read(paste(home_gis,file_yakima,sep="/"))

nhd_folder<-"C:/Project/Columbia_SWAT/NHD_YRB_SWAT/inputs/nhd"
nhd_stream_file<-"nhd_yrb_stream3_vaa_wgs84.shp"


nhd_sub9_stream_wgs84<-st_read(paste(nhd_folder,nhd_stream_file,sep="/"))
nhd_sub9_stream_wgs84=nhd_sub9_stream_wgs84[,c("Prmnnt_")]
colnames(nhd_sub9_stream_wgs84)=c("COMID","geometry")

## creating the landcover/use parameters 
####################-- channel width, drainage area and flow --------
# dir_nexss<-"C:/Project/Columbia_SPARROW/data/nxss/"
# file<-"dataHUC17_May29_2020.rds"
# nexss<-readRDS((paste0(dir_nexss,file)))
# nexss_info=nexss[,c("comid_nhd","logwbkf_m","logd_m","D50_m")]


nhd_stream_yrb_data<-nhd_sub9_stream_wgs84
nhd_stream_yrb_data$geometry<-NULL

## Land Cover Attributes##############
########### Percent NLCD 2011 Tree Canopy################3
home_nhd_stream<-"D:/Project/SFA/Columbia_SPARROW/data/streamdatabase"

tree_canopy<-"Percent NLCD 2011 Tree Canopy/CNPY11_BUFF100_CONUS"

tree_canopy_file<-"CNPY11_BUFF100_CONUS.txt"
tree_canopy_info<-read.delim(paste(home_nhd_stream,tree_canopy,tree_canopy_file,sep="/"),header=T,sep=",")
tree_canopy_info<-tree_canopy_info[,c(1,2,6)]

nhd_stream_yrb_land<-merge(nhd_stream_yrb_data,tree_canopy_info,by.x="COMID",by.y="COMID")
rm(tree_canopy_info)

## Wildfire 2000-2012 ###############
library(plyr)
### unzip files ########################
setwd(paste(home_nhd_stream,"Wildfire 2000-2012",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
########################################

setwd(paste(home_nhd_stream,"Wildfire 2000-2012",sep="/"))
files <- list.files(pattern="*.txt")
files<-mixedsort(files)

for (i in 1:length(files)){
wildfire_info<-read.delim(paste(home_nhd_stream,"Wildfire 2000-2012",files[i],sep="/"),header=T,sep=",")

wildfire_info<-wildfire_info[,c(1,2,4)]

nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,wildfire_info,by.x="COMID",by.y="COMID")

rm(wildfire_info)

}


## seasonal  EVI ##############
### unzip files ########################
setwd(paste(home_nhd_stream,"Enhanced Vegetation Index 2012",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
#########################################

setwd(paste(home_nhd_stream,"Enhanced Vegetation Index 2012",sep="/"))
files <- list.files(pattern="*.txt")
files<-mixedsort(files)

for (i in 1:length(files)){
  evi_info<-read.delim(paste(home_nhd_stream,"Enhanced Vegetation Index 2012",files[i],sep="/"),header=T,sep=",")
  
  evi_info<-evi_info[,c(1,2,6)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,evi_info,by.x="COMID",by.y="COMID")
  rm(evi_info)
}

## National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016 ##############
### unzip files ########################
setwd(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
######################################


setwd(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",sep="/"))


files <- list.files(pattern=glob2rx("*CAT_CONUS.TXT"))
files<-mixedsort(files)

  print(files[1])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[1],sep="/"),header=T,sep=",")
  
  land_info$CAT_urban01<-land_info$CAT_NLCD01_21+land_info$CAT_NLCD01_22+land_info$CAT_NLCD01_23+land_info$CAT_NLCD01_24
  land_info$CAT_forest01<-land_info$CAT_NLCD01_41+land_info$CAT_NLCD01_42+land_info$CAT_NLCD01_43
  land_info$CAT_wetland01<-land_info$CAT_NLCD01_90+land_info$CAT_NLCD01_95
  land_info$CAT_agrc01<-land_info$CAT_NLCD01_81+land_info$CAT_NLCD01_82
  land_info$CAT_shrub01<-land_info$CAT_NLCD01_52
  
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  
  rm(land_info)
  
  
  print(files[2])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[2],sep="/"),header=T,sep=",")
  
  land_info$CAT_urban04<-land_info$CAT_NLCD04_21+land_info$CAT_NLCD04_22+land_info$CAT_NLCD04_23+land_info$CAT_NLCD04_24
  land_info$CAT_forest04<-land_info$CAT_NLCD04_41+land_info$CAT_NLCD04_42+land_info$CAT_NLCD04_43
  land_info$CAT_wetland04<-land_info$CAT_NLCD04_90+land_info$CAT_NLCD04_95
  land_info$CAT_agrc04<-land_info$CAT_NLCD04_81+land_info$CAT_NLCD04_82
  land_info$CAT_shrub04<-land_info$CAT_NLCD04_52
  
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  
  print(files[3])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[3],sep="/"),header=T,sep=",")
  
  land_info$CAT_urban06<-land_info$CAT_NLCD06_21+land_info$CAT_NLCD06_22+land_info$CAT_NLCD06_23+land_info$CAT_NLCD06_24
  land_info$CAT_forest06<-land_info$CAT_NLCD06_41+land_info$CAT_NLCD06_42+land_info$CAT_NLCD06_43
  land_info$CAT_wetland06<-land_info$CAT_NLCD06_90+land_info$CAT_NLCD06_95
  land_info$CAT_agrc06<-land_info$CAT_NLCD06_81+land_info$CAT_NLCD06_82
  land_info$CAT_shrub06<-land_info$CAT_NLCD06_52
  
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  print(files[4])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[4],sep="/"),header=T,sep=",")
  
  land_info$CAT_urban08<-land_info$CAT_NLCD08_21+land_info$CAT_NLCD08_22+land_info$CAT_NLCD08_23+land_info$CAT_NLCD08_24
  land_info$CAT_forest08<-land_info$CAT_NLCD08_41+land_info$CAT_NLCD08_42+land_info$CAT_NLCD08_43
  land_info$CAT_wetland08<-land_info$CAT_NLCD08_90+land_info$CAT_NLCD08_95
  land_info$CAT_agrc08<-land_info$CAT_NLCD08_81+land_info$CAT_NLCD08_82
  land_info$CAT_shrub08<-land_info$CAT_NLCD08_52
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  print(files[5])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[5],sep="/"),header=T,sep=",")
  
  land_info$CAT_urban11<-land_info$CAT_NLCD11_21+land_info$CAT_NLCD11_22+land_info$CAT_NLCD11_23+land_info$CAT_NLCD11_24
  land_info$CAT_forest11<-land_info$CAT_NLCD11_41+land_info$CAT_NLCD11_42+land_info$CAT_NLCD11_43
  land_info$CAT_wetland11<-land_info$CAT_NLCD11_90+land_info$CAT_NLCD11_95
  land_info$CAT_agrc11<-land_info$CAT_NLCD11_81+land_info$CAT_NLCD11_82
  land_info$CAT_shrub11<-land_info$CAT_NLCD11_52
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  print(files[6])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[6],sep="/"),header=T,sep=",")
  
  land_info$CAT_urban13<-land_info$CAT_NLCD13_21+land_info$CAT_NLCD13_22+land_info$CAT_NLCD13_23+land_info$CAT_NLCD13_24
  land_info$CAT_forest13<-land_info$CAT_NLCD13_41+land_info$CAT_NLCD13_42+land_info$CAT_NLCD13_43
  land_info$CAT_wetland13<-land_info$CAT_NLCD13_90+land_info$CAT_NLCD13_95
  land_info$CAT_agrc13<-land_info$CAT_NLCD13_81+land_info$CAT_NLCD13_82
  land_info$CAT_shrub13<-land_info$CAT_NLCD13_52
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  print(files[7])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[7],sep="/"),header=T,sep=",")
  
  land_info$CAT_urban16<-land_info$CAT_NLCD16_21+land_info$CAT_NLCD16_22+land_info$CAT_NLCD16_23+land_info$CAT_NLCD16_24
  land_info$CAT_forest16<-land_info$CAT_NLCD16_41+land_info$CAT_NLCD16_42+land_info$CAT_NLCD16_43
  land_info$CAT_wetland16<-land_info$CAT_NLCD16_90+land_info$CAT_NLCD16_95
  land_info$CAT_agrc16<-land_info$CAT_NLCD16_81+land_info$CAT_NLCD16_82
  land_info$CAT_shrub16<-land_info$CAT_NLCD16_52
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  ###################################################################
  files <- list.files(pattern=glob2rx("*TOT_CONUS.TXT"))
  files<-mixedsort(files)
  
  print(files[1])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[1],sep="/"),header=T,sep=",")
  
  land_info$TOT_urban01<-land_info$TOT_NLCD01_21+land_info$TOT_NLCD01_22+land_info$TOT_NLCD01_23+land_info$TOT_NLCD01_24
  land_info$TOT_forest01<-land_info$TOT_NLCD01_41+land_info$TOT_NLCD01_42+land_info$TOT_NLCD01_43
  land_info$TOT_wetland01<-land_info$TOT_NLCD01_90+land_info$TOT_NLCD01_95
  land_info$TOT_agrc01<-land_info$TOT_NLCD01_81+land_info$TOT_NLCD01_82
  land_info$TOT_shrub01<-land_info$TOT_NLCD01_52
  
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  
  print(files[2])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[2],sep="/"),header=T,sep=",")
  
  land_info$TOT_urban04<-land_info$TOT_NLCD04_21+land_info$TOT_NLCD04_22+land_info$TOT_NLCD04_23+land_info$TOT_NLCD04_24
  land_info$TOT_forest04<-land_info$TOT_NLCD04_41+land_info$TOT_NLCD04_42+land_info$TOT_NLCD04_43
  land_info$TOT_wetland04<-land_info$TOT_NLCD04_90+land_info$TOT_NLCD04_95
  land_info$TOT_agrc04<-land_info$TOT_NLCD04_81+land_info$TOT_NLCD04_82
  land_info$TOT_shrub04<-land_info$TOT_NLCD04_52
  
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)  
  
  
  print(files[3])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[3],sep="/"),header=T,sep=",")
  
  land_info$TOT_urban06<-land_info$TOT_NLCD06_21+land_info$TOT_NLCD06_22+land_info$TOT_NLCD06_23+land_info$TOT_NLCD06_24
  land_info$TOT_forest06<-land_info$TOT_NLCD06_41+land_info$TOT_NLCD06_42+land_info$TOT_NLCD06_43
  land_info$TOT_wetland06<-land_info$TOT_NLCD06_90+land_info$TOT_NLCD06_95
  land_info$TOT_agrc06<-land_info$TOT_NLCD06_81+land_info$TOT_NLCD06_82
  land_info$TOT_shrub06<-land_info$TOT_NLCD06_52

  land_info=land_info[,c(1,19,20,21,22,23)]
  

  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  print(files[4])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[4],sep="/"),header=T,sep=",")
  
  land_info$TOT_urban08<-land_info$TOT_NLCD08_21+land_info$TOT_NLCD08_22+land_info$TOT_NLCD08_23+land_info$TOT_NLCD08_24
  land_info$TOT_forest08<-land_info$TOT_NLCD08_41+land_info$TOT_NLCD08_42+land_info$TOT_NLCD08_43
  land_info$TOT_wetland08<-land_info$TOT_NLCD08_90+land_info$TOT_NLCD08_95
  land_info$TOT_agrc08<-land_info$TOT_NLCD08_81+land_info$TOT_NLCD08_82
  land_info$TOT_shrub08<-land_info$TOT_NLCD08_52
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  print(files[5])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[5],sep="/"),header=T,sep=",")
  
  land_info$TOT_urban11<-land_info$TOT_NLCD11_21+land_info$TOT_NLCD11_22+land_info$TOT_NLCD11_23+land_info$TOT_NLCD11_24
  land_info$TOT_forest11<-land_info$TOT_NLCD11_41+land_info$TOT_NLCD11_42+land_info$TOT_NLCD11_43
  land_info$TOT_wetland11<-land_info$TOT_NLCD11_90+land_info$TOT_NLCD11_95
  land_info$TOT_agrc11<-land_info$TOT_NLCD11_81+land_info$TOT_NLCD11_82
  land_info$TOT_shrub11<-land_info$TOT_NLCD11_52
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  print(files[6])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[6],sep="/"),header=T,sep=",")
  
  land_info$TOT_urban13<-land_info$TOT_NLCD13_21+land_info$TOT_NLCD13_22+land_info$TOT_NLCD13_23+land_info$TOT_NLCD13_24
  land_info$TOT_forest13<-land_info$TOT_NLCD13_41+land_info$TOT_NLCD13_42+land_info$TOT_NLCD13_43
  land_info$TOT_wetland13<-land_info$TOT_NLCD13_90+land_info$TOT_NLCD13_95
  land_info$TOT_agrc13<-land_info$TOT_NLCD13_81+land_info$TOT_NLCD13_82
  land_info$TOT_shrub13<-land_info$TOT_NLCD13_52
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  rm(land_info)
  
  print(files[7])
  land_info<-read.delim(paste(home_nhd_stream,"National Land Cover Database 2016 Versions for the Years 2001, 2004, 2006, 2008, 2011, 2013, and 2016",files[7],sep="/"),header=T,sep=",")
  
  land_info$TOT_urban16<-land_info$TOT_NLCD16_21+land_info$TOT_NLCD16_22+land_info$TOT_NLCD16_23+land_info$TOT_NLCD16_24
  land_info$TOT_forest16<-land_info$TOT_NLCD16_41+land_info$TOT_NLCD16_42+land_info$TOT_NLCD16_43
  land_info$TOT_wetland16<-land_info$TOT_NLCD16_90+land_info$TOT_NLCD16_95
  land_info$TOT_agrc16<-land_info$TOT_NLCD16_81+land_info$TOT_NLCD16_82
  land_info$TOT_shrub16<-land_info$TOT_NLCD16_52
  
  land_info=land_info[,c(1,19,20,21,22,23)]
  
  nhd_stream_yrb_land<-merge(nhd_stream_yrb_land,land_info,by.x="COMID",by.y="COMID")
  
  
  
  rm(land_info)
  
  ### saving files
  
  saveRDS(nhd_stream_yrb_land,file=paste(home_data,"nhd_stream_yrb_land.rds",sep="/"))
  
  

######### Climate and Water Balance Model ##############
##Annual Average Actual Evapotranspiration (millimeters) from 2014 - 2015 ##############

setwd(paste(home_nhd_stream,"Annual Average Actual Evapotranspiration (millimeters) from 2014 - 2015",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
#########################
###################################################################

setwd(paste(home_nhd_stream,"Annual Average Actual Evapotranspiration (millimeters) from 2014 - 2015",sep="/"))

files <- list.files(pattern=glob2rx("*_CONUS.TXT"))
files<-mixedsort(files)

for (i in 1:length(files)){
  print(files[i])

  ET_info<-read.delim(paste(home_nhd_stream,"Annual Average Actual Evapotranspiration (millimeters) from 2014 - 2015",files[i],sep="/"),header=T,sep=",")
  ET_info<-ET_info[,c(1,2,4)]

  nhd_stream_yrb_climate<-merge(nhd_stream_yrb_data,ET_info,by.x="COMID",by.y="COMID")
  rm(ET_info)
}




##Annual Average Potential Evapotranspiration (millimeters) from 2014 - 2015 ##############
setwd(paste(home_nhd_stream,"Annual Average Potential Evapotranspiration (millimeters) from 2014 - 2015",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
#########################
###################################################################
setwd(paste(home_nhd_stream,"Annual Average Potential Evapotranspiration (millimeters) from 2014 - 2015",sep="/"))

files <- list.files(pattern=glob2rx("*_CONUS.TXT"))
files<-mixedsort(files)

for (i in 1:length(files)){
  
  print(files[i])
  
  PET_info<-read.delim(paste(home_nhd_stream,"Annual Average Potential Evapotranspiration (millimeters) from 2014 - 2015",files[i],sep="/"),header=T,sep=",")
  PET_info<-PET_info[,c(1,2,4)]
  
  nhd_stream_yrb_climate<-merge(nhd_stream_yrb_climate,PET_info,by.x="COMID",by.y="COMID")
  rm(PET_info)  
}




###Annual Average Precipitation (millimeters) from 1945-2015##############
setwd(paste(home_nhd_stream,"Annual Average Precipitation (millimeters) from 1945-2015",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
###################################################################

setwd(paste(home_nhd_stream,"Annual Average Precipitation (millimeters) from 1945-2015",sep="/"))

files <- list.files(pattern=glob2rx("*_CONUS.txt"))

files<-mixedsort(files)
print(files)

for (i in 1:length(files)){
  
  print(files[i])
  
  ppt_info<-read.delim(paste(home_nhd_stream,"Annual Average Precipitation (millimeters) from 1945-2015",files[i],sep="/"),header=T,sep=",")
  ppt_info<-ppt_info[,c(1,2,4)]
  
  nhd_stream_yrb_climate<-merge(nhd_stream_yrb_climate,ppt_info,by.x="COMID",by.y="COMID")

  rm(ppt_info)  
}



### Annual Average Runoff (millimeters) from 1945 - 2015 ############
setwd(paste(home_nhd_stream,"Annual Average Runoff (millimeters) from 1945 - 2015",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)

###################################################################
setwd(paste(home_nhd_stream,"Annual Average Runoff (millimeters) from 1945 - 2015",sep="/"))
files <- list.files(pattern=glob2rx("*_CONUS.txt"))

files<-mixedsort(files)
print(files)
      
for (i in 1:length(files)){
  
  print(files[i])
  
  flow_info<-read.delim(paste(home_nhd_stream,"Annual Average Runoff (millimeters) from 1945 - 2015",files[i],sep="/"),header=T,sep=",")
  flow_info<-flow_info[,c(1,2,4)]
  
  nhd_stream_yrb_climate<-merge(nhd_stream_yrb_climate,flow_info,by.x="COMID",by.y="COMID")
  
  rm(flow_info)
  
}

######### Annual Average Temperature (Celsius) from 1945 – 2015 #################
setwd(paste(home_nhd_stream,"Annual Average Temperature (Celsius) from 1945 - 2015",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
###################################################################
setwd(paste(home_nhd_stream,"Annual Average Temperature (Celsius) from 1945 - 2015",sep="/"))
files <- list.files(pattern=glob2rx("*_CONUS.TXT"))

files<-mixedsort(files)

print(files)

for (i in 1:length(files)){
  
  print(files[i])
  
  temp_info<-read.delim(paste(home_nhd_stream,"Annual Average Temperature (Celsius) from 1945 - 2015",files[i],sep="/"),header=T,sep=",")
  temp_info<-temp_info[,c(1,2,4)]
  
  nhd_stream_yrb_climate<-merge(nhd_stream_yrb_climate,temp_info,by.x="COMID",by.y="COMID")
  rm(temp_info)
  
}

### saving files
saveRDS(nhd_stream_yrb_climate,file=paste(home_data,"nhd_stream_yrb_climate.rds",sep="/"))



########Hydrologic Attributes ###################################
## Contact Time #######
setwd(paste(home_nhd_stream,"Contact Time",sep="/"))

files <- list.files(pattern=glob2rx("*_CONUS.txt"))
files<-mixedsort(files)

print(files)

for (i in 1:length(files)){
  
  print(files[i])
  
  contact_info<-read.delim(paste(home_nhd_stream,"Contact Time",files[i],sep="/"),header=T,sep=",")
  contact_info<-contact_info[,c(1,2,6)]
  
  nhd_stream_yrb_hydro<-merge(nhd_stream_yrb_data,contact_info,by.x="COMID",by.y="COMID")
  rm(contact_info)
  
}


## Estimated Mean Annual Natural Groundwater Recharge ########
setwd(paste(home_nhd_stream,"Estimated Mean Annual Natural Groundwater Recharge",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)

###################################################################
setwd(paste(home_nhd_stream,"Estimated Mean Annual Natural Groundwater Recharge",sep="/"))

files <- list.files(pattern=glob2rx("*_CONUS.txt"))

files<-mixedsort(files)

print(files)

for (i in 1:length(files)){
  
  print(files[i])
  
  recharge_info<-read.delim(paste(home_nhd_stream,"Estimated Mean Annual Natural Groundwater Recharge",files[i],sep="/"),header=T,sep=",")
  recharge_info<-recharge_info[,c(1,2,6)]
  
  nhd_stream_yrb_hydro<-merge(nhd_stream_yrb_hydro,recharge_info,by.x="COMID",by.y="COMID")
  rm(recharge_info)
  
}


### saving files
saveRDS(nhd_stream_yrb_hydro,file=paste(home_data,"nhd_stream_yrb_hydro.rds",sep="/"))


######### other variables #################
#Population Infrastructure:Housing Density 1940-2010
#Regional Attributes:Enhanced Vegetation Index 2012*
#Water Use:estimated fresh-water withdrawal, 2010

####### Housing Density 1940-2010 #########
setwd(paste(home_nhd_stream,"Housing Density 1940-2010",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
#########################################

setwd(paste(home_nhd_stream,"Housing Density 1940-2010",sep="/"))

files <- list.files(pattern=glob2rx("*_CONUS.txt"))
files<-mixedsort(files)
print(files)


for (i in 1:length(files)){
  
  print(files[i])
  
  house_info<-read.delim(paste(home_nhd_stream,"Housing Density 1940-2010",files[i],sep="/"),header=T,sep=",")
  house_info<-house_info[,c(1,2,6)]
  
  nhd_stream_yrb_others<-merge(nhd_stream_yrb_data,house_info,by.x="COMID",by.y="COMID")
  
  rm(house_info)
  
}


######################Enhanced Vegetation Index 2012################################################
setwd(paste(home_nhd_stream,"Enhanced Vegetation Index 2012",sep="/"))
zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
##########################
setwd(paste(home_nhd_stream,"Enhanced Vegetation Index 2012",sep="/"))
files <- list.files(pattern=glob2rx("*_CONUS.txt"))

files<-mixedsort(files)
print(files)

for (i in 1:length(files)){
  
  print(files[i])
  
  evi_info<-read.delim(paste(home_nhd_stream,"Enhanced Vegetation Index 2012",files[i],sep="/"),header=T,sep=",")
  evi_info<-evi_info[,c(1,2,6)]
  
  nhd_stream_yrb_others<-merge(nhd_stream_yrb_others,evi_info,by.x="COMID",by.y="COMID")
  rm(evi_info)
  
}


#################estimated fresh-water withdrawal, 2010##############
setwd(paste(home_nhd_stream,"estimated fresh-water withdrawal, 2010",sep="/"))

zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
##########################
setwd(paste(home_nhd_stream,"estimated fresh-water withdrawal, 2010",sep="/"))

files <- list.files(pattern=glob2rx("*.txt"))

files<-mixedsort(files)

print(files)

for (i in 1:length(files)){
  
  print(files[i])
  
 water_info<-read.delim(paste(home_nhd_stream,"estimated fresh-water withdrawal, 2010",files[i],sep="/"),header=T,sep=",")
 water_info<-water_info[,c(1,2)]
  
 nhd_stream_yrb_others<-merge(nhd_stream_yrb_others,water_info,by.x="COMID",by.y="COMID")
 rm(water_info)
 
}


### saving files
saveRDS(nhd_stream_yrb_others,file=paste(home_data,"nhd_stream_yrb_others.rds",sep="/"))

############ Select Basin Characteristics #####################

setwd(paste(home_nhd_stream,"Select Basin Characteristics",sep="/"))

zipF <- list.files(pattern="*.zip", full.names = TRUE)
ldply(.data=zipF,.fun=unzip)
##########################

setwd(paste(home_nhd_stream,"Select Basin Characteristics",sep="/"))

files <- list.files(pattern=glob2rx("*.TXT"))

files<-mixedsort(files)
print(files)

  basin_cat_info<-read.delim(paste(home_nhd_stream,"Select Basin Characteristics",files[2],sep="/"),header=T,sep=",")
  basin_tot_info<-read.delim(paste(home_nhd_stream,"Select Basin Characteristics",files[3],sep="/"),header=T,sep=",")
  nhd_stream_yrb_basin<-merge(nhd_stream_yrb_data,basin_cat_info,by.x="COMID",by.y="COMID")
  nhd_stream_yrb_basin<-merge(nhd_stream_yrb_basin,basin_tot_info,by.x="COMID",by.y="COMID")
  
  rm(basin_cat_info)
  rm(basin_tot_info)
  
  road_cat_info<-read.delim(paste(home_nhd_stream,"Select Basin Characteristics",files[7],sep="/"),header=T,sep=",")
  road_tot_info<-read.delim(paste(home_nhd_stream,"Select Basin Characteristics",files[8],sep="/"),header=T,sep=",")
  road_cat_info<-road_cat_info[,c(2,17)]
  road_tot_info<-road_tot_info[,c(2,17)]
  nhd_stream_yrb_basin<-merge(nhd_stream_yrb_basin,road_cat_info,by.x="COMID",by.y="COMID")
  nhd_stream_yrb_basin<-merge(nhd_stream_yrb_basin,road_tot_info,by.x="COMID",by.y="COMID")

  rm(road_tot_info)
  rm(road_cat_info)
  
  sinuousity_info<-read.delim(paste(home_nhd_stream,"Select Basin Characteristics",files[9],sep="/"),header=T,sep=",")
  nhd_stream_yrb_basin<-merge(nhd_stream_yrb_basin,sinuousity_info,by.x="COMID",by.y="COMID")
  rm(sinuousity_info)
  
  stream_density_info<-read.delim(paste(home_nhd_stream,"Select Basin Characteristics",files[10],sep="/"),header=T,sep=",")
  stream_density_info<-stream_density_info[,c(1,2,4)]
  nhd_stream_yrb_basin<-merge(nhd_stream_yrb_basin,stream_density_info,by.x="COMID",by.y="COMID")
    rm(stream_density_info)

    ### saving files
  saveRDS(nhd_stream_yrb_basin,file=paste(home_data,"nhd_stream_yrb_basin.rds",sep="/"))
  
  

