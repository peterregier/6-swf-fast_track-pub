## This script makes the map figure (Figure 1) for the Guerrero et al. scaling paper
## 
## 2023-09-07
## Peter Regier
## 
# ########### #
# ########### #
#
## 9/8: Willamette boundary is wrong, rolling with it for now but will want to
## fix (ideally get source from Fco) asap


# 1. Setup ---------------------------------------------------------------------

require(pacman)

p_load(tidyverse, 
       rnaturalearth, # for pulling in US states
       nhdplusTools, # for pulling NHD data
       cowplot,
       ggthemes, #theme_map()
       sf)

shp_path <- "data/shapefiles/"

## Set a common crs
common_crs = 4326

## Coordinate projection for coord_sf to make things look cool
coord_sf_crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100"

## Set watershed color scheme
y_color = "forestgreen"
w_color = "blue"

# 2. Read in watershed shapefiles ----------------------------------------------

nsi <- read_sf(paste0(shp_path, "nsi_network_ywrb/nsi_network_ywrb.shp")) %>% 
  st_transform(crs = common_crs)

nsi

ggplot() + 
  geom_sf(data = nsi) + 
  theme_minimal()

## There isn't a convenient way to break these apart, so I'll use st_crop
yakima_flowlines <- st_crop(nsi, xmin = -122, xmax = -119, ymin = 45.9, ymax = 48)
willamette_flowlines <- st_crop(nsi, xmin = -124, xmax = -121, ymin = 43, ymax = 46)

## Now squish em back together with watershed labeled
flowlines <- bind_rows(yakima_flowlines %>% mutate(watershed = "Yakima"), 
                       willamette_flowlines %>% mutate(watershed = "Willamette"))

## Double-check things are labeled right
ggplot() + 
  geom_sf(data = flowlines, aes(color = watershed)) + 
  theme_minimal()


# 3. Pull watershed boundaries -------------------------------------------------

yakima_boundary <- get_huc(AOI = st_union(yakima_flowlines), type = "huc08") %>% 
  filter(id != "wbd08_20201006.1872") %>% # manually remove one that doesn't belong
  st_union() %>% 
  st_as_sf()

## I tried the same approach with the Willamette, but it was pretty resistant, 
## primarily on trying hard to include the Columbia moving NW. Let's use sf
## tools instead to simply outline
willamette_boundary <- read_sf(paste0(shp_path, "mmt_willametteinitiative/WillametteBasin.shp")) %>% 
  st_transform(crs = common_crs)

## These polygons didn't want to merge, can troubleshoot later, but for now will
## just treat as two separate layers. Would be ideal to match format for flowlines
## above, will be easier if I get equivalent outline shapefile(s) from Fco


# 4. Pull in states for reference ----------------------------------------------

conus <- ne_states(country = "united states of america", returnclass = "sf") %>% 
  st_transform(crs = common_crs) %>% 
  st_crop(xmin = -125, xmax = -60, ymin = 20, ymax = 50) 

pnw <- conus %>% 
  filter(name == "Washington" | 
           name == "Oregon")


# 5. Panel A: regional context -------------------------------------------------

## Standardize the alpha for watershed shading across plots
basin_fill_alpha = 0.2

plot_a <- ggplot() + 
  geom_sf(data = pnw, fill = "gray97") + 
  geom_sf(data = yakima_boundary, color = "black", fill = y_color, alpha = basin_fill_alpha) + 
  geom_sf(data = willamette_boundary, color = "black", fill = w_color, alpha = basin_fill_alpha) + 
  theme_map() + 
  coord_sf(crs = coord_sf_crs) #+ 
  #ggtitle("Content only, formatting later!")

# 6. Panel B: Yakima Basin -----------------------------------------------------

## Standardize flowline color
flowline_color = "darkblue"

plot_b <- ggplot() + 
  geom_sf(data = yakima_boundary, color = "black", 
          fill = y_color, alpha = basin_fill_alpha) + 
  geom_sf(data = yakima_flowlines, color = flowline_color, alpha = 0.6) + 
  theme_map() + 
  coord_sf(crs = coord_sf_crs)


# 6. Panel C: Willamette Basin -------------------------------------------------

plot_c <- ggplot() + 
  geom_sf(data = willamette_boundary, color = "black", 
          fill = w_color, alpha = basin_fill_alpha) + 
  geom_sf(data =  willamette_flowlines, color = flowline_color, alpha = 0.6) + 
  theme_map() + 
  coord_sf(crs = coord_sf_crs)


# 7. Panel D: Basin comparisons ------------------------------------------------

## I'm not sure what would be most useful for this panel, and maybe that makes
## the figure too crowded, but the idea is that we can show (and then reference)
## important characteristics that are similar or different between basins

## Calculate area
w_area <- as.numeric(st_area(willamette_boundary) / (1000*1000)) #convert m2 to km2
y_area <- as.numeric(st_area(yakima_boundary) / (1000*1000)) #convert m2 to km2

area <- tibble(basin = c("Willamette", "Yakima"), 
               area = c(w_area, y_area))

plot_d <- ggplot(area, aes(basin, area, fill = basin)) + 
  geom_col(color = "black", alpha = basin_fill_alpha, show.legend = F) + 
  scale_fill_manual(values = c(w_color, y_color)) + 
  labs(x = "", y = "Basin area (km2)") +
  theme_minimal() 


# 8. Merge and export figure ---------------------------------------------------

plot_grid(plot_a, plot_b, plot_c, plot_d, labels = c("A", "B", "C", "D"), 
          nrow = 2)
ggsave("results/guerrero_etal_23_Fig1_map.png", width = 12, height = 10)


# 9. Write out individual figure layers to manipulate later --------------------
  
plot_a
ggsave("results/Figure 1 Layers/guerrero_etal_23_Fig1A.pdf", width = 4, height = 6)

plot_b
ggsave("results/Figure 1 Layers/guerrero_etal_23_Fig1B.pdf", width = 4, height = 6)

plot_c
ggsave("results/Figure 1 Layers/guerrero_etal_23_Fig1C.pdf", width = 4, height = 6)

plot_d
ggsave("results/Figure 1 Layers/guerrero_etal_23_Fig1D.pdf", width = 3, height = 2)



