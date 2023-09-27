README

We use the enhanced NHDPlus V.2. as the reference dataset for COMIDs 
(Blodgett_23_Network_Attributes)

Original dataset citation
Blodgett, D.L., 2023, Updated CONUS river network attributes based on the 
E2NHDPlusV2 and NWMv2.1 networks (ver. 2.0, February 2023): U.S. Geological 
Survey data release, https://doi.org/10.5066/P976XCVT

Moore, R.B., McKay, L.D., Rea, A.H., Bondelid, T.R., Price, C.V., Dewald, T.G., 
and Johnston, C.M., 2019, User's guide for the national hydrography dataset plus 
(NHDPlus) high resolution: U.S. Geological Survey Open-File Report 2019–1096, 66 p., 
https://pubs.usgs.gov/of/2019/1096/ofr20191096.pdf

Flowline slopes

In the Enhanced Hydrologic Stream Network Based on the NHDPlus Medium resolution
dataset (blg23_dat, in our case), the slopes were revised and re-calculated: 
"NHDPlus slopes determined according to the original NHDPlusV2 method and the 
revised method in relation to slopes measured at 2,846 sites indexed to NHDPlusV2 
from the U.S. Environmental Protection Agency's Wadeable Streams Assessment
and National River and Stream Assessment. WSA, Wadeable Streams Assessment; NRSA, 
# National River and Stream Assessment"

The major update consisted in a better determination of the benchmark points, 
mostly in headwater catchments, that could be used as the initial elevation, from 
which the slopes would be determined in the downstream direction. For more info
on the revised method go to: https://pubs.usgs.gov/sir/2019/5127/sir20195127.pdf

comit and to_comid
Blodgett eta al.'s dataset also includes a Cleaned up network, added tocomid from 
tonode/fromnode topology, and removed unwanted modifications from E2NHDPlusV2 and NWMv2.1.