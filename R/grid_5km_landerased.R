# The 5km, equal area grid used for the risk assessment was developed by Blake Feist
grid.5km.geom <- st_read(
  here("data", "5x5 km grid shapefile", "five_km_grid_polys_geo.shp"))

# This script takes this 5km grid and erases land area using a hi-res shapefile:
#   https://github.com/jameals/raimbow/blob/master/grid-prep/Grid5km_landerase.R
# In this repo, we simply use the output of the above-linked script:
grid.5km.lno <- readRDS(here("data", "Grid_5km_landerased.rds"))
