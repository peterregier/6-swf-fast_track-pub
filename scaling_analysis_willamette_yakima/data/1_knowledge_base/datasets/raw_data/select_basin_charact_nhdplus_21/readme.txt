# README

## Dataset: NHDPlus v. 2.1: Selected Basin Characteristcis-Yakima and Willamette
River Basins

### Source Citation: This dataset merges two sources of data from the series:
Select Attributes for NHDPlus Version 2.1 Reach Catchments and Modified Network 
Routed Upstream Watersheds for the Conterminous United States:

1.Select Basin Characteristics (ver. 3.0, January 2021): U.S. Geological Survey 
data release, https://www.sciencebase.gov/catalog/item/57976a0ce4b021cadec97890

2.Bankfull Hydraulic Geometry Related to Physiographic Divisions (ver. 3.0, 
January 2021): U.S. Geological Survey data release, 
https://www.sciencebase.gov/catalog/item/5cf02bdae4b0b51330e22b85 


This dataset was clipped from the CONUS dataset to obtain values corresponding to 
the Yakima and Willamette River Watershed only. To clip these data, we used a 
reference list of "comid's" after subsetting the the enhanced NHDPlus V.2. as the 
reference dataset for COMIDs (Blodgett_23_Network_Attributes). We subsetted the 
enhanced NHDPlus V.2. using the huc_4, which we derived as the first four digits
of the "reachcode" variable. Thus, most our datasets, unless indicated otherwise
correspond the the huc_4 1703 and 1709 for the Yakima and Willamette River Basins
respectively. 

