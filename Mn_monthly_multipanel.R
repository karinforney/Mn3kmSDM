### Make multipanel plot of monthly Mn 3km preds

###############################################################################
library(dplyr)
library(eSDM)
library(RColorBrewer)
library(rnaturalearth)
library(sf)
library(tmap)

source("User_script_local.R")
if (user == "KAF") {
  
  
} else if (user == "SMW") {
  path.mn.monthly <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_monthly.RDS"
  file.out.all <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_monthly_multiplanel_all.png"
  file.out.sub <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_monthly_multiplanel_all.png"
}


###############################################################################
### Create sf object
x <- readRDS(path.mn.monthly)

x.geom <- x %>% 
  select(mlon, mlat, pixel) %>% 
  distinct() %>% 
  pts2poly_centroids(0.027, crs = 4326, agr = "constant")

x.sf <- x %>% #~2 minutes
  mutate(month_fac = lubridate::month(month, label = TRUE, abbr = FALSE)) %>% 
  left_join(x.geom, by = "pixel") %>% 
  st_as_sf()


###############################################################################
# Prep for multipanel, and plot

#------------------------------------------------------------------------------
### Prep
print("plotting")
col.breaks <- c(0, 0.01, 0.02, 0.03, 0.05, 0.08)
col.pal <- brewer.pal(length(col.breaks) - 1, "Blues")

rmap.base <- st_geometry(ne_states(country = "United States of America", returnclass = "sf")) %>% 
  st_geometry() %>% 
  st_crop(x.sf)
# rmap.base2 <- ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>% 
#   filter(admin %in% c("Canada", "Mexico")) %>% 
#   st_geometry()

tmap_mode("plot")


#------------------------------------------------------------------------------
### Plot all of them
x.toplot.all <- x.sf %>%
  # mutate(month_fac = lubridate::month(month, label = TRUE, abbr = FALSE)) %>% 
  filter(year %in% c(2005, 2006), #tmp
         pixel %in% sample(unique(x.sf$pixel), 2000))

tm.obj <- tm_shape(rmap.base, bbox = st_bbox(x.toplot.all)) + 
  tm_polygons() +
  tm_shape(x.toplot.all) + 
  tm_fill(col = "mn_avgdens", border.col = "transparent",
          style = "fixed", breaks = col.breaks, palette = col.pal, 
          colorNA = NULL, showNA = FALSE, 
          title = "Whales / km2", legend.reverse = TRUE) + 
  tm_facets(by = c("year", "month_fac"), 
            free.coords = FALSE, free.scales = FALSE) +
  tm_legend(outside = TRUE, position = c("right", "center"), outside.size = 0.2,
            text.size = 2, title.size = 3) +
  # # Issue: graticules span the facet boxes, but the map does not..
  # tm_graticules(ticks = TRUE, lines = FALSE, n.y = 5, n.x = 5, labels.size = 1)
  tm_layout(panel.label.size = 2.3)

tmap_save(tm.obj, filename = file.out.all, width = 14, height = 4, units = "in")


#------------------------------------------------------------------------------
### Plot subset
x.toplot.sub <- x.sf %>%
  # mutate(month_fac = lubridate::month(month, label = TRUE, abbr = FALSE)) %>% 
  filter(month %in% c(1, 4, 7, 10),
         year %in% c(2005:2007), 
         pixel %in% sort(sample(unique(x.sf$pixel), 2000)))

tm.obj.sub <- tm_shape(rmap.base, bbox = st_bbox(x.toplot.sub)) + 
  tm_polygons() +
  tm_shape(x.toplot.sub) + 
  tm_fill(col = "mn_avgdens", border.col = "transparent",
          style = "fixed", breaks = col.breaks, palette = col.pal, 
          colorNA = NULL, showNA = FALSE, 
          title = "Whales / km2", legend.reverse = TRUE) + 
  tm_facets(by = c("year", "month_fac"), 
            free.coords = FALSE, free.scales = FALSE) +
  tm_legend(outside = TRUE, position = c("right", "center"), outside.size = 0.2,
            text.size = 2, title.size = 3) +
  # # Issue: graticules span the facet boxes, but the map does not..
  tm_graticules(ticks = TRUE, lines = FALSE, n.y = 5, n.x = 3, labels.size = 1) + 
  tm_layout(panel.label.size = 2.3)

tmap_save(tm.obj.sub, filename = file.out.sub, width = 10, height = 10, units = "in")

###############################################################################
