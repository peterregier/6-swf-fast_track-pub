## Dataset: Attributes for NHDPlus Version 2.1 Catchments and Modified Routing of Upstream 
Watersheds for the Conterminous United States: National Land Cover Database 2001 (NLCD 2001):
Yakima and WillametteRiver Basins

### Source Citation: Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream Watersheds for the Conterminous United States (ver. 3.0, January 2021): U.S. Geological Survey data release, https://doi.org/10.5066/F7765D7V.


This dataset was clipped from the CONUS dataset to obtain values corresponding to 
the Yakima and Willamette River Watershed only. To clip these data, we used a 
reference list of "comid's" after subsetting the the enhanced NHDPlus V.2. as the 
reference dataset for COMIDs (Blodgett_23_Network_Attributes). We subsetted the 
enhanced NHDPlus V.2. using the huc_4, which we derived as the first four digits
of the "reachcode" variable. Thus, most our datasets, unless indicated otherwise
correspond the the huc_4 1703 and 1709 for the Yakima and Willamette River Basins
respectively. 

The original metadata xml file is stored in the "metadata" folder, the "raw_data" folder
Contains three different files: 

1) NLCD_01_CAT: Individual reach catchments 
2) NLCD_01_ACC: reach catchments accumulated upstream through the river network.
3) NLCD_01_TOT: total drainage area closing at the COMID location
