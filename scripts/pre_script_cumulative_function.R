###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# CUMULATIVE CALCULATIONS
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

librarian::shelf(tidyverse,
                 nhdplusTools, 
                 purrr)

#Data:

# Local import and export paths

raw_data <- "../1-swf-knowledge.base/assets/data/raw" 
processed_data <- "../1-swf-knowledge.base/assets/data/processed"
assets_figs <- "../1-swf-knowledge.base/assets/plots"

#header info (data dictionary)

heading_dat <- read_csv(paste(processed_data,"guerrero_etal_swf_dd.csv", sep = '/'),
                        show_col_types = FALSE)

#values
bgc_dat_c8 <- read_csv(paste(processed_data,"230505_bgc_dat_c7_entropy.csv", sep = "/"),
                       show_col_types = FALSE) %>% 
  mutate(totco2g_day = 10^logtotco2g_m2_day*stream_area_m2,
         RT_total_hz_s = 10^logRT_total_hz_s,
         qhz_total_m_s = 10^logq_hz_total_m_s)

p <- ggplot(data = bgc_dat_c8,
            aes(x = sinuosity,
                y = RT_total_hz_s,
                color = basin))+
  geom_smooth()+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p

# Cummulative values

library(purrr)
library(dplyr)

accm_dat <- bgc_dat_c8 %>% 
  group_by(basin) %>% 
  select(comid,
         tocomid,
         basin,
         reach_type,
         stream_order,
         wshd_stream_dens,
         tot_stream_length_km,
         wshd_area_km2,
         stream_area_m2,
         RT_total_hz_s,
         qhz_total_m_s,
         wshd_forest_scp,
         wshd_grass_scp,
         wshd_shrub_scp,
         wshd_human_scp,
         hrel,
         totco2g_day) %>% 
  mutate(across(stream_area_m2:totco2g_day, ~ calculate_arbolate_sum(data.frame(ID = comid,
                                                                                toID = tocomid,
                                                                                length = .x))) %>% 
           set_names(paste0("accm_", names(select(., stream_area_m2:totco2g_day))))) 



# p <- ggplot(filter(accm_dat,wshd_area_km2 > 0.25),
# p <- ggplot(filter(accm_dat,
#                    reach_type == "StreamRiver" &
#                    wshd_area_km2 > 0.25),
p <- ggplot(accm_dat,
            aes(x = wshd_area_km2,
                y = accm_totco2g_day/wshd_area_km2, 
                color = as.factor(stream_order)))+
  geom_point(alpha = 0.5)+
  # geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(intercept = 0.6, 
              linetype = "dashed", 
              color = "black",
              linewidth = 1.0)+
  geom_abline(intercept = 0.9, 
              slope = 1.7,
              linetype = "dashed", 
              color = "black",
              linewidth = 1.0)+
  facet_wrap(~basin, ncol = 2, scales = "free_y")
p


p <- ggplot(filter(accm_dat,
                   reach_type == "StreamRiver" &
                     wshd_area_km2 > 0.25),
            # p <- ggplot(accm_dat,
            aes(x = wshd_area_km2,
                y = accm_totco2g_day/wshd_area_km2, 
                color = log(stream_area_m2)))+
  geom_point(alpha = 0.5)+
  # geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(intercept = 0.6, 
              linetype = "dashed", 
              color = "black",
              linewidth = 1.0)+
  geom_abline(intercept = 0.9, 
              slope = 1.7,
              linetype = "dashed", 
              color = "black",
              linewidth = 1.0)+
  facet_wrap(~basin, ncol = 2, scales = "free_y")
p



p1 <- ggplot(filter(accm_dat,
                   reach_type == "StreamRiver"|
                     wshd_area_km2 > 0.5),
            aes(x = wshd_area_km2,
                y = accm_stream_area_m2, 
                color = basin))+
  geom_point(alpha = 0.5)+
  # geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(intercept = 3.6, 
              linetype = "dashed", 
              color = "black",
              linewidth = 1.0)+
  facet_wrap(basin~as.factor(stream_order), nrow = 2)
p1

















p <- ggplot(filter(accm_dat,
                   reach_type != "StreamRiver"),
            aes(x = wshd_area_km2,
                y = (accm_totco2g_day/wshd_area_km2)*(1/wshd_stream_dens), 
                color = basin))+
  geom_point(alpha = 0.5)+
  # geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(intercept = 3.6, 
              linetype = "dashed", 
              color = "black",
              linewidth = 1.0)+
  facet_wrap(~basin, ncol = 2)
p



p2 <- ggplot(bgc_dat_c8,
            aes(x = wshd_area_km2,
                y = wshd_stream_dens, 
                color =hrel))+
  geom_point(alpha = 0.85)+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~basin, ncol = 2)
p2

p3 <- ggplot(filter(bgc_dat_c8,
                    reach_type == "StreamRiver"),
             aes(x = wshd_area_km2,
                 y = stream_area_m2, 
                 color = basin))+
  geom_point(alpha = 0.5)+
  # geom_smooth(method = 'lm')+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline(intercept = 3.6, 
              linetype = "dashed", 
              color = "black",
              linewidth = 1.0)+
  facet_wrap(basin~stream_order, nrow = 2)
p3





area_mod <- lm(log(accm_stream_area_m2,10) ~ log(wshd_area_km2,10) + basin, 
               data = accm_dat)

summary(area_mod)
confint(area_mod)

# The scaling exponent for stream surface area is significantly higher in the
# Willamette watershed (1.06) than in the Yakima River Basin (0.93). However, 
# in both cases cumulative stream area scales linearly with watershed area, and 
# as such watershed function.

area_mod$effects

accm_dat <- bgc_dat_c8 %>% 
  group_by(basin) %>% 
  select(comid, tocomid, basin, stream_area_m2, RT_total_hz_s, qhz_total_m_s, wshd_forest_scp, wshd_grass_scp, wshd_shrub_scp, wshd_human_scp, totco2g_day) %>% 
  mutate_at(vars(stream_area_m2:totco2g_day), ~ calculate_arbolate_sum(data.frame(ID = accm_dat$comid, toID = accm_dat$tocomid, length = .x))) %>% 
  rename_with(~ paste0("accm_", .), starts_with("stream_area_m2"), everything())


# Yakima River Basin

accm_resp_yrb <- bgc_dat_c8 %>%
  filter(basin == "yakima") %>% 
  select(comid, tocomid, totco2g_day) %>% 
  rename(ID = comid,
         toID = tocomid,
         length = totco2g_day) 

bgc_co2_accm <- accm_resp_yrb %>%
  mutate(acc_totco2g_day = calculate_arbolate_sum(accm_resp_yrb)) %>%
  select(ID, acc_totco2g_day) %>% 
  rename(comid = ID)









bgc_dat_c9 <- bgc_dat_c8 %>% 
  filter(basin == "yakima") %>% 
  left_join(.,
            bgc_co2_accm,
            by = "comid") %>% 
  mutate(acc_totco2g_km2_day = acc_totco2g_day/wshd_area_km2)


# Cumulative stream surface area

accm_stream_area_yrb <- bgc_dat_c9 %>%
  filter(basin == "yakima") %>% 
  select(comid, tocomid, stream_area_m2) %>% 
  rename(ID = comid,
         toID = tocomid,
         length = stream_area_m2) 

acc_str_area_yrb <- accm_stream_area_yrb %>%
  mutate(acc_stream_area_m2 = calculate_arbolate_sum(accm_stream_area_yrb)) %>%
  select(ID, acc_stream_area_m2 ) %>% 
  rename(comid = ID)

bgc_dat_c10 <- bgc_dat_c9 %>% 
  filter(basin == "yakima") %>% 
  left_join(.,
            acc_str_area_yrb,
            by = "comid") %>% 
  mutate(acc_totco2g_stream_m2_day = acc_totco2g_day/acc_stream_area_m2)


scl_plot <- ggplot(bgc_dat_c10,
                   aes(x = wshd_area_km2,
                       y = acc_stream_area_m2))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()+
  geom_abline()
scl_plot



bgc_dat_c_test_accm <- left_join(bgc_dat_c_test, bgc_dat_c_test_accm, by = "ID") %>% 
  rename(comid = ID)





















################# Finding upstream reaches ########################################3
findSrc <- function(endNode, ret = NULL) {
  w <- which(to_node %in% endNode)
  
  if (length(w) == 0) return(c(ret, endNode))
  else {
    return(findSrc(from_node[w], c(ret, endNode)))
  }
}

################# function validation ##############################################

#We will test the performance of this function by calculating the cumulative stream 
#length, which should correspond to "tot_stream_length_km" in the original dataset. 


######### YAKIMA RIVER BASIN ############################################
yrb_dat <- filter(bgc_dat_c8, basin == "yakima")

to_node<-yrb_dat$to_node
from_node<-yrb_dat$from_node

##########################################################################
#Cumulative stream length

tmp_comid_strm_l<-matrix(0,ncol=2,nrow=nrow(yrb_dat))

for (i in 1:nrow(yrb_dat)){
  j_id<-findSrc(yrb_dat$from_node[i],yrb_dat$to_node[i])
  ## select the downstream reach
  tmp<-j_id[1]
  
  tmp_comid<-yrb_dat[yrb_dat$from_node==j_id[1],]
  tmp_comid<-tmp_comid$comid
  
  if(length(j_id)==1){
    ## no upstream
    tmp1_strm_l<-yrb_dat[yrb_dat$from_node==j_id[1],]
  }
  
  else {
    
    tmp1_strm_l= subset(yrb_dat, from_node %in% c(j_id))
  }
  
  tmp_comid_strm_l[i,]<-cbind(tmp_comid[1],sum(tmp1_strm_l$reach_length_km))
  
  print(i)
  
}

yrb_acc_strm_l<- as_tibble(unlist(tmp_comid_strm_l)) %>% 
  rename(comid = V1,
         acc_stream_length_km = V2) %>% 
  distinct(comid, .keep_all = TRUE) 

# It results in the same number of NAs, so there is something wrong with the function

# Arbolate Sum in NHDPlus are calculated using COMID, to toCOMID instead of nodes, with
# the NHDPlus package

# Calculating cumulative stream length


bgc_dat_c_test <- bgc_dat_c8 %>%
  select(comid, tocomid, mean_ann_runf_mm) %>% 
  rename(ID = comid,
         toID = tocomid,
         length = mean_ann_runf_mm) 

bgc_dat_c_test_accm <- bgc_dat_c_test %>%
  mutate(accm_ann_runf = calculate_arbolate_sum(bgc_dat_c_test)) %>%
  select(ID, accm_ann_runf)

bgc_dat_c_test_accm <- left_join(bgc_dat_c_test, bgc_dat_c_test_accm, by = "ID") %>% 
  rename(comid = ID)

bgc_dat_t2 <- bgc_dat_c8 %>% 
  select(comid, wshd_area_km2, mean_ann_runf_mm) %>% 
  left_join(.,
            bgc_dat_c_test_accm,
            by = "comid") %>% 
  ggplot(aes(x= wshd_area_km2,
             y = accm_ann_runf))+
  scale_x_log10()+
  scale_y_log10()+
  geom_point()





















bgc_dat_c_test <- bgc_dat_c8 %>%
  select(comid, tocomid, reach_length_km) %>% 
  rename(seg_id = comid,
         toID = tocomid,
         length_km = reach_length_km) %>% 
  mutate(accm_strm_length = calculate_arbolate_sum(., 
                                                   id_col = "seg_id", 
                                                   len_col = "length_km"))








strm_length_plot <- bgc_dat_c8 %>% 
  select(comid, tot_stream_length_km) %>% 
  merge(.,
        bgc_dat_c_test,
        by = "comid") %>% 
  ggplot(aes(x = tot_stream_length_km,
             y = accm_strm_length))+
  geom_point()+
  geom_abline()+
  scale_x_log10()+
  scale_y_log10()
strm_length_plot

# There are a few outliers in our dataset, so we probably need to replace the 
# values with the new nhdplusTools calculation

# Testing for concentrations

bgc_dat_c_test1 <- bgc_dat_c8 %>%
  group_by(basin) %>% 
  select(ID = comid, toID = tocomid, length = logRT_total_hz_s) %>% 
  mutate(accm_res_time  = calculate_arbolate_sum(bgc_dat_c_test1))%>% 
  rename(comid = ID)

rst_plot <- bgc_dat_c8 %>% 
  select(comid, stream_order, wshd_area_km2) %>% 
  merge(.,
        bgc_dat_c_test1,
        by = "comid") %>% 
  ggplot(aes(x = wshd_area_km2,
             y = accm_res_time))+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()
rst_plot

# Testing example from ChatGPT

cgpt_stream_length <- bgc_dat_c8 %>% 
  select(reach_length_km,comid,tocomid)%>% 
  mutate(ro = order(tocomid),
         accm_stream_length = ave(reach_length_km, ro, FUN = cumsum))

strm_length_plot <- bgc_dat_c8 %>% 
  select(comid, tot_stream_length_km) %>% 
  merge(.,
        cgpt_stream_length,
        by = "comid") %>% 
  ggplot(aes(x = tot_stream_length_km,
             y = accm_stream_length))+
  geom_point()+
  geom_abline()+
  scale_x_log10()+
  scale_y_log10()
strm_length_plot


######### YAKIMA RIVER BASIN ############################################
yrb_dat <- filter(bgc_dat_c8, basin == "yakima")

to_node<-yrb_dat$to_node
from_node<-yrb_dat$from_node

tmp_comid_resp<-matrix(0,ncol=2,nrow=nrow(yrb_dat))

for (i in 1:nrow(yrb_dat)){
  j_id<-findSrc(yrb_dat$from_node[i],yrb_dat$to_node[i])
  ## select the downstream reach
  tmp<-j_id[1]
  
  tmp_comid<-yrb_dat[yrb_dat$from_node==j_id[1],]
  tmp_comid<-tmp_comid$comid
  
  if(length(j_id)==1){
    ## no upstream
    tmp1_resp<-yrb_dat[yrb_dat$from_node==j_id[1],]
  }
  
  else {
    
    tmp1_resp= subset(yrb_dat, from_node %in% c(j_id))
  }
  
  tmp_comid_resp[i,]<-cbind(tmp_comid[1],sum(tmp1_resp$totco2g_day))
  
  print(i)
  
}

yrb_acc_rsp <- as_tibble(unlist(tmp_comid_resp)) %>% 
  rename(comid = V1,
         acc_totco2g_day = V2) %>% 
  distinct(comid, .keep_all = TRUE) 

yrb_acc_dat <- merge(yrb_dat,
                     yrb_acc_rsp,
                     by = "comid",
                     all.x = TRUE)

# This operation results in a total 2486 NA values. 

# Let's check what are the attributes of this subset: 

na_acc_dat <- filter(yrb_acc_dat, is.na(acc_totco2g_day)==TRUE)

summary(na_acc_dat)

# This dataset covers a wide range of values for most attributes except for stream
# order, which correspond to mostly first order streams

# Let's check if this number encompasses all first order streams in the original 
# dataset

tot_1_order_streams <- nrow(filter(yrb_acc_dat,stream_order == 1))

# we have a total of 3223 first order streams. 

# Let's now check how many first order streams we have in the NAs data subset

tot_1_order_streams_na <- nrow(filter(na_acc_dat,stream_order == 1))

# 2480; only 6 streams corresponding to a different order. 




yrb_acc_dat_t <- yrb_acc_dat %>% 
  group_by(stream_order) %>% 
  mutate(acc_totco2g_day = if_else(is.na(acc_totco2g_day) & stream_order == 1,
                                   totco2g_day,
                                   if_else(is.na(acc_totco2g_day) & stream_order>1,
                                           median(acc_totco2g_day, na.rm = TRUE),
                                           acc_totco2g_day)),
         acc_totco2g_km2_day = acc_totco2g_day/wshd_area_km2,
         acc_totco2g_ntw_day = acc_totco2g_day*wshd_stream_dens)



#################################### Willamette River ##########################

wrb_dat <- filter(bgc_dat_c8, basin == "willamette")

to_node<-wrb_dat$to_node
from_node<-wrb_dat$from_node

tmp_comid_resp<-matrix(0,ncol=2,nrow=nrow(wrb_dat))

for (i in 1:nrow(wrb_dat)){
  j_id<-findSrc(wrb_dat$from_node[i],wrb_dat$to_node[i])
  ## select the downstream reach
  tmp<-j_id[1]
  
  tmp_comid<-wrb_dat[wrb_dat$from_node==j_id[1],]
  tmp_comid<-tmp_comid$comid
  
  if(length(j_id)==1){
    ## no upstream
    tmp1_resp<-wrb_dat[wrb_dat$from_node==j_id[1],]
  }
  
  else {
    
    tmp1_resp= subset(wrb_dat, from_node %in% c(j_id))
  }
  
  tmp_comid_resp[i,]<-cbind(tmp_comid[1],sum(tmp1_resp$totco2g_day))
  
  print(i)
  
}

wrb_acc_rsp <- as_tibble(unlist(tmp_comid_resp)) %>% 
  rename(comid = V1,
         acc_totco2g_day = V2) %>% 
  distinct(comid, .keep_all = TRUE) 

wrb_acc_dat <- merge(wrb_dat,
                     wrb_acc_rsp,
                     by = "comid",
                     all.x = TRUE)

summary(filter(wrb_acc_dat, is.na(acc_totco2g_day)==TRUE))

# Mostly first order streams with no upstream connections result in NA values 
# the cumulative estimation needs to be updated


wrb_acc_dat_t <- wrb_acc_dat %>% 
  group_by(stream_order) %>% 
  mutate(acc_totco2g_day = if_else(is.na(acc_totco2g_day) & stream_order == 1,
                                   totco2g_day,
                                   if_else(is.na(acc_totco2g_day) & stream_order>1,
                                           median(acc_totco2g_day, na.rm = TRUE),
                                           acc_totco2g_day)),
         acc_totco2g_km2_day = acc_totco2g_day/wshd_area_km2,
         acc_totco2g_ntw_day = acc_totco2g_day*wshd_stream_dens)


bgc_dat_c10 <- rbind(yrb_acc_dat_t,
                     wrb_acc_dat_t)

write.csv(bgc_dat_c10,paste(processed_data,"230507_bgc_dat_c10.csv", sep = '/'),
          row.names = FALSE)

################################## IN PROGRESS ##################################

# define function to find source nodes
findSrc <- function(endNode, ret = NULL) {
  w <- which(to_node %in% endNode)
  
  if (length(w) == 0) return(c(ret, endNode))
  else {
    return(findSrc(from_node[w], c(ret, endNode)))
  }
}

# define function to process data for a single basin
process_basin <- function(basin) {
  bgc_dat_c8 <- read_csv(paste(processed_data,"230505_bgc_dat_c7_entropy.csv", sep = "/"),
                         show_col_types = FALSE) %>% 
    mutate(totco2g_day = 10^logtotco2g_m2_day*stream_area_m2)
  
  
  to_node <- bgc_dat_c8$to_node
  from_node <- bgc_dat_c8$from_node
  
  tmp_comid_resp <- matrix(0, ncol = 2, nrow = nrow(bgc_dat_c8))
  
  for (i in 1:nrow(bgc_dat_c8)) {
    j_id <- findSrc(bgc_dat_c8$from_node[i], bgc_dat_c8$to_node[i])
    tmp_comid <- bgc_dat_c8[bgc_dat_c8$from_node == j_id[1], "comid"]
    
    if (length(j_id) == 1) {
      tmp1_resp <- bgc_dat_c8[bgc_dat_c8$from_node == j_id[1], "totco2g_day"]
    } else {
      tmp1_resp <- bgc_dat_c8[from_node %in% j_id, "totco2g_day"] %>% sum()
    }
    
    tmp_comid_resp[i, ] <- c(tmp_comid[1], tmp1_resp)
  }
  
  acc_rsp <- as_tibble(tmp_comid_resp) %>% 
    rename(comid = V1, acc_totco2g_day = V2) %>% 
    distinct(comid, .keep_all = TRUE)
  
  acc_dat <- merge(bgc_dat_c8, acc_rsp, by = "comid", all.x = TRUE) %>% 
    group_by(stream_order) %>% 
    mutate(acc_totco2g_day = if_else(
      is.na(acc_totco2g_day) & stream_order == 1,
      totco2g_day,
      if_else(
        is.na(acc_totco2g_day) & stream_order > 1,
        median(acc_totco2g_day, na.rm = TRUE),
        acc_totco2g_day
      )
    ), 
    acc_totco2g_km2_day = acc_totco2g_day / wshd_area_km2,
    acc_totco2g_ntw_day = acc_totco2g_day * wshd_stream_dens)
  
  return(acc_dat)
}

# define vector of basin names
basins <- c("yakima", "willamette")

# process data for each basin
all_acc_dat <- lapply(basins, process_basin) %>% bind_rows()































