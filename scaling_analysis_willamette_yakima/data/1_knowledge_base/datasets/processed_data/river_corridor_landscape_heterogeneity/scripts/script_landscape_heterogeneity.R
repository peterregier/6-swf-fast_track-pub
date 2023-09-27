###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# LANDSCAPE HETEROGENEITY ANALYSIS
###############################################################################

#By : Francisco Guerrero
#Data source: Data sets generated with "script_comid_ref_landuse.R"

#Loading packages:

#Run for the first time only
#install.packages(librarian)

# To run this code in macos it is necessary to install XQuartz from 
#www.xquartz.org

librarian::shelf(tidyverse,#(includes ggplot2, readr, dplyr, tidyr, and more...)
                 entropy, usethis)

set.seed(2703)


# NLDC Legend Colors
# Source: https://github.com/BuzzFeedNews/us-land-cover/blob/master/nlcd_legend.csv

# Open water	#5475a8
# Perenn. ice/snow	#ffffff
# Open space	#e8d1d1
# Low intens. dev.	#e29e8c
# Med. intens. dev.	#f00f00
# High intens. dev.	#b50000
# Barren	#d2cdc0
# Deciduous forest	#85c77e
# Evergreen forest	#38814e
# Mixed forest	#d4e7b0
# Shrub/scrub	#dcca8f
# Grassland	#e2e2c1
# Pasture/hay	#fbf65d
# Cultivated crops	#ca9146
# Woody wetland	#c8e6f8
# Other wetland	#64b3d5

# NLDC Colors

nlcd_colors <- c("#5475a8","#ffffff","#e8d1d1","#e29e8c","#f00f00",
                 "#b50000","#d2cdc0","#85c77e","#38814e","#d4e7b0",
                 "#dcca8f","#e2e2c1","#fbf65d","#ca9146","#c8e6f8",
                 "#64b3d5")

nlcd_cat <- c("water",
              "snow",
              "developed_op",
              "developed_lw",
              "developed_md",
              "developed_hg",
              "barren",
              "forest_dcd",
              "forest_evg",
              "forest_mxd",
              "shrub",
              "grass",
              "pasture",
              "crops",
              "wetland_wood",
              "wetland_herb")

# Data

# Local Import-Export
source_data <- "../../raw_data/nlcd_2001_v2019/data"
local_data <- "./data"
local_metadata <- "./metadata"
figures <- "../../../visual_assets/3_plots"



# Data inputs

ctch_lnd_dat <- read_csv(paste(source_data,"nlcd_2001_v2019_NLCD01_CAT_CONUS.csv", sep = '/'),
                    show_col_types = FALSE)

wshd_lnd_dat <- read_csv(paste(source_data,"nlcd_2001_v2019_NLCD01_TOT_CONUS.csv", sep = '/'),
                         show_col_types = FALSE)

# Pre-processing data inputs

# Catchment data

colnames(ctch_lnd_dat) <- gsub("CAT_","",colnames(ctch_lnd_dat))

colnames(ctch_lnd_dat) <- gsub("NLCD01_11","water",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_12","snow",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_21","developed_op",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_22","developed_lw",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_23","developed_md",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_24","developed_hg",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_31","barren",colnames(ctch_lnd_dat))

colnames(ctch_lnd_dat) <- gsub("NLCD01_41","forest_dcd",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_42","forest_evg",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_43","forest_mxd",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_52","shrub",colnames(ctch_lnd_dat))

colnames(ctch_lnd_dat) <- gsub("NLCD01_71","grass",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_81","pasture",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_82","crops",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_90","wetland_wood",colnames(ctch_lnd_dat))
colnames(ctch_lnd_dat) <- gsub("NLCD01_95","wetland_herb",colnames(ctch_lnd_dat))

ctch_lnd_dat$level <- "catchment"

# Watershed data

colnames(wshd_lnd_dat) <- gsub("TOT_","",colnames(wshd_lnd_dat))

colnames(wshd_lnd_dat) <- gsub("NLCD01_11","water",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_12","snow",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_21","developed_op",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_22","developed_lw",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_23","developed_md",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_24","developed_hg",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_31","barren",colnames(wshd_lnd_dat))

colnames(wshd_lnd_dat) <- gsub("NLCD01_41","forest_dcd",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_42","forest_evg",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_43","forest_mxd",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_52","shrub",colnames(wshd_lnd_dat))

colnames(wshd_lnd_dat) <- gsub("NLCD01_71","grass",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_81","pasture",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_82","crops",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_90","wetland_wood",colnames(wshd_lnd_dat))
colnames(wshd_lnd_dat) <- gsub("NLCD01_95","wetland_herb",colnames(wshd_lnd_dat))

wshd_lnd_dat$level <- "watershed"

# Dataset for analysis

swf_land_dat <- rbind(ctch_lnd_dat,
                      wshd_lnd_dat) %>% 
  select(-NODATA) %>% 
  mutate(tot_cover = rowSums(across(where(is.numeric)))-comid)

summary(swf_land_dat)

################################################################################
# Information content analysis
################################################################################

# The NLCD 2001 includes 16 land use categories for both the catchment scale and 
# the watershed scale. It is common practice for statistical analysis to reduce
# the number of land use types to a more manageable quantity (5-6). The aggregation
# criteria may vary among researchers and it is not necessarily guided by data. 

# Here, we use an information-theory derived criteria to identify not only the
# categories that contribute the most to the spatial heterogeneity across the 
# landscape but also data-driven criteria for aggregation of these categories for 
# modeling purposes. 

# We are going to use re sampling to estimate the uncertainty about the information
# contribution from the land use components.

# We will run this analysis across the entire data set, including the Willamette and 
# the Yakima data in the estimations.

# You can select the level of analysis you want to start with: 

###############################################################################
# Analysis level 
###############################################################################

# It takes the values ("catchment" or "watershed")

analysis_level <- "catchment"

if (analysis_level== "catchment") {
  lnd_inf <- swf_land_dat %>% 
    filter(level == "catchment")%>% 
    select(c(3:18))
} else {
  lnd_inf <- swf_land_dat %>% 
    filter(level == "watershed")%>% 
    select(c(3:18))
}

################################################################################
#Estimating original values for information content
################################################################################

colnames(lnd_inf) <- nlcd_cat

ncols <- ncol(lnd_inf)
nrows <- nrow(lnd_inf)
use <- as.data.frame(colnames(lnd_inf))
colnames(use) <- "land_use"

inf_mat<-matrix(1:ncols, ncols, 4,dimnames=list(NULL,c("Yj","H","Hmax","I")))#Matrix with information content 
#content results

im0<-lnd_inf/sum(lnd_inf)  #Normalizing matrices

#Calculating information contributions
for (j in 1:ncols){
  yjn=sum(im0[,j])
  hn=entropy(im0[,j],unit="log2")
  hmaxn=log2(nrows)
  inf_mat[j,1]= yjn
  inf_mat[j,2]= hn
  inf_mat[j,3]= hmaxn
  inf_mat[j,4]=yjn%*%(hmaxn-hn)
}
inf_mat

inf_mat <- cbind(use,inf_mat)

inf_mat$land_use <- factor(inf_mat$land_use,levels = nlcd_cat)

p <- ggplot(inf_mat,aes(x=reorder(land_use,I), y = I, fill = land_use))+
  geom_bar(position="dodge",stat="identity") +
  ylab("Information contribution")+
  xlab("Land use (NLDC-2001 (2019))")+
  scale_fill_manual(values = nlcd_colors)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(paste("Information contribution per land use category:",analysis_level,"scale",sep = " "))+
  theme_minimal()+
  coord_flip()
p

# geom bar required a workaround as explained by: 
# https://stackoverflow.com/questions/29525130/ggplot2-plotting-a-single-value-in-a-bar-graph

#Estimating bootsrapped information content

b_inf_mat= matrix(1:ncols, ncols, 4,dimnames=list(NULL,c("Yjn","Hn","Hmaxn","In")))

list_yj=list()# in this list we will store the results from the 1000 iterations
list_hj=list()
list_ic=list()

for (i in 1:1000){
  if (i==1001){
    break
  }
  nmp=lnd_inf[sample(nrow(lnd_inf),size=nrows,replace=TRUE),]
  im=nmp/sum(nmp)  #Normalizing matrices
  #Calculating information contributions
  for (j in 1:ncols){
    yjn=sum(im[,j])
    hn=entropy(im[,j],unit="log2")
    hmaxn=log2(nrows)
    b_inf_mat[j,1]= yjn
    b_inf_mat[j,2]= hn
    b_inf_mat[j,3]= hmaxn
    b_inf_mat[j,4]=yjn%*%(hmaxn-hn)
  }
  
  list_ic[[i]]<-b_inf_mat[,4]
  
}

inf_dat=do.call("rbind", list_ic)# it combines all the lists in the same file
inf_dat=as.data.frame(inf_dat)
colnames(inf_dat) <- nlcd_cat

inf_dat_long <- inf_dat %>% 
  gather(key="land_use",
         value="information_contribution",
         factor_key = TRUE) %>% 
  group_by(land_use) %>% 
  mutate(inf_avg = mean(information_contribution),
         inf_sd = sd(information_contribution))


p <- ggplot(inf_dat_long,aes(x=reorder(land_use,inf_avg), y = inf_avg, fill = land_use))+
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin=inf_avg-inf_sd, ymax=inf_avg+inf_sd), width=.2,
                position=position_dodge(.9)) +
  ylab("Information contribution (bootstrapped average)")+
  xlab("Land use (NLDC-2001 (2019))")+
  scale_fill_manual(values = nlcd_colors, name = "Land Use")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(paste("Information contribution per land use category (bootstrapped):",analysis_level,"scale",sep = " "))+
  theme_minimal()+
  coord_flip()
p

plot_name <- paste0("inf_contrib_",analysis_level,".jpg")

ggsave(paste(figures,plot_name,sep = '/'),
       width = 12,
       height = 10,
       units = "in")


################################################################################
# Reduced dimensionality -scapes
################################################################################

# We can combine the results from the information contribution analysis to reduce
# the dimensionality of the land use data without loosing important information. 

# In the case of qualitative variables, our first category should
# start with the land cover with the highest information contribution, in this case,
# forest_evg. Since other forest types contribute with less information, we can group 
# them within the same category, that we will refer to as "Forestcapes". The next 
# land use with the second largest information contribution is grass, although followed
# immediately by pastures and crops, these last two are managed by humans and should not
# be grouped together. These landscapes units will correspond to "grasslandscapes". 
# Although grasses and shrublands can co-occur, they represent two different types of 
# communities between the Willamette valley and the Yakima River Basin. The shrubland category, 
# will be indexed as "shrublandscapes". 

# Following this approach we will have the following new categories for land use at both
# the catchment and the watershed scales.

# Forestcapes: forest_scp =forest_evg + forest_dcd + forest_mxd
# Grasslandscapes: grass_scp = grass
# Shrublandscapes: shrub_scp = shrub
# Waterscapes: water_scp = snow + water + wetland_wood + wetland_herb
# Humanscapes: human_scp = pasture + crops + developed_op + developed_lw + developed_md + developed_hg
# Barrenscapes: barren_scp = barren

# We modify our initial data set accordingly

swf_land_new_dat <- swf_land_dat %>% 
  filter(level == analysis_level) %>% 
  mutate(forest_scp =forest_evg + forest_dcd + forest_mxd,
         grass_scp = grass,
         shrub_scp = shrub,
         water_scp = snow + water + wetland_wood + wetland_herb,
         human_scp = pasture + crops + developed_op + developed_lw + developed_md +developed_hg,
         barren_scp = barren)


swf_land_ent_dat <- swf_land_new_dat %>% 
  rowwise() %>% 
  mutate(ht = entropy(c(forest_scp,
                        grass_scp,
                        shrub_scp,
                        water_scp,
                        human_scp,
                        barren_scp),
                      unit = "log2"),
         hmax = log(6,2),
         hrel = ht/hmax) %>%
  ungroup()

write.csv(swf_land_ent_dat,paste(local_data,paste0(analysis_level,"_landscape_heterogeneity_pnw.csv"),sep = '/'),
          row.names = FALSE)
