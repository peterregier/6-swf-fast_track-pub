## This script makes the map figure (Figure 1) for the Guerrero et al. scaling paper
## 
## 2023-09-07
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

require(pacman)

p_load(tidyverse, 
       rnaturalearth, # for pulling in US states
       nhdplusTools, # for pulling NHD data
       ggthemes, #theme_map()
       sf)

shp_path <- "data/shapefiles/"

## Set a common crs
common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"


# 2. Read in watershed shapefiles ----------------------------------------------

nsi <- read_sf(paste0(shp_path, "nsi_network_ywrb.shp")) %>% 
  st_transform(crs = common_crs)

nsi

ggplot() + 
  geom_sf(data = nsi)


# 3. Set up bounding boxes
yk_bbox = 

# 4. Pull in states for reference ----------------------------------------------

conus <- ne_states(country = "united states of america", returnclass = "sf") %>% 
  st_transform(crs = common_crs) %>% 
  st_crop(xmin = -125, xmax = -60, ymin = 20, ymax = 50) 

pnw <- conus %>% 
  filter(name == "Washington" | 
           name == "Oregon")

x <- get_huc(AOI = nsi, type = "huc08")

ggplot() + 
  geom_sf(data = pnw) + 
  geom_sf(data = nsi) + 
  theme_minimal() + 
 # theme_map() + 
  coord_sf(crs = coord_sf_crs)


nhdplusTools::


