### Code from 'Mn_multipanel_prey_compare.Rmd' as script to be run as a job
# By Sam Woodman

###############################################################################
library(eSDM)
library(lubridate)
library(RColorBrewer)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(tmap)

plot.path <- "C:/SMW/RAIMBOW/raimbow-local/Plots/Plots_KAF/Multipanel/Mn_3km_Apr-Jun_all.png" #NULL

source("C:/SMW/RAIMBOW/whale-model-prep/Whalepreds_aggregate.R")
source("C:/SMW/RAIMBOW/whale-model-prep/Whalepreds_aggregate_dates.R")


###############################################################################
### Read in whale preds and process

# x.orig <- read_csv(
#   "../raimbow-local/Data/Humpback 3km models/Model1_PredictionGrids/WEAR3km_76_2005-01-01to2019-08-14_daily_dens.csv", 
#   col_types = cols(.default = col_double())
# )
# saveRDS(x.orig, file = "../raimbow-local/RDATA_files/")
x.3km.bidaily <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/3km_daily.rds")

x.orig <- x.3km.bidaily %>% 
  select(pixel, mlat, mlon, areakm, starts_with("76.dens.2"))

range.dates <- ymd(
  paste(rep(2005:2018, each = 2), sprintf("%02d", c(4, 7)), "01", sep = "_")
)
x.agg <- whalepreds_aggregate(
  x.orig, 5:ncol(x.orig), 9:18, aggr.level = NULL, range.dates = range.dates, 
  se.calc = FALSE #Note: would need to change naming below if se.calc = TRUE
) %>% 
  select(pixel, mlat, mlon, areakm, ends_with("_04_01")) %>% 
  set_names(c("pixel", "mlat", "mlon", "areakm", 
              paste("mn", 2005:2018, "0406", sep = "_")))

# # Sanity testing
# d <- x.orig %>% 
#   select(contains(".dens.2014.04"), contains(".dens.2014.05"), contains(".dens.2014.06"))
# d.avg <- data.frame(pixel = x.orig$pixel, mn_dens_avg = apply(d, 1, mean))
# all.equal(d.avg$mn_dens_avg, x.exp$Avg_user_2014_04_01)

# Make data long and get total mean
x <- x.agg %>% 
  gather(-pixel, -mlat, -mlon, -areakm, key = "col_name", value = "mn_avgdens") %>% 
  mutate(year = substr(col_name, 4, 7)) #%>% 
  # filter(between(mlat, 36, 40))


x.meanall <- x %>% 
  group_by(pixel, mlat, mlon, areakm) %>% 
  summarise(mn_avgdens = mean(mn_avgdens)) %>% 
  ungroup() %>% 
  mutate(year = "All")

# Make sf object
x.geom <- x.orig %>% 
  select(mlon, mlat, pixel) %>% 
  pts2poly_centroids(0.027, crs = 4326, agr = "constant")

x.sf <- x.meanall %>% 
  bind_rows(select(x, -col_name)) %>% 
  mutate(year = factor(year, levels = c("All", 2005:2018))) %>% 
  left_join(x.geom, by = "pixel") %>% 
  st_as_sf(crs = 4326, agr = "constant") %>% 
  select(year, mn_avgdens, pixel)

# x.sf2 <- x %>% 
#   select(-col_name) %>% 
#   bind_rows(x.meanall) %>% 
#   select(mlon, mlat, year, mn_avgdens) %>% 
#   pts2poly_centroids(0.027, crs = 4326, agr = "constant")
# all.equal(x.sf$mn_avgdens, x.sf2$mn_avgdens)
# identical(st_geometry(x.sf), st_geometry(x.sf2))


###############################################################################
### Plot things prep
tmap_mode("plot")
col.breaks <- c(0, 0.01, 0.02, 0.03, 0.05, 0.08)
col.pal <- rev(brewer.pal(length(col.breaks) - 1, "YlGnBu"))

rmap.base <- st_geometry(ne_states(country = "United States of America", returnclass = "sf")) %>% 
  st_geometry() %>% 
  st_crop(x.sf)
# rmap.base2 <- ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>% 
#   filter(admin %in% c("Canada", "Mexico")) %>% 
#   st_geometry()


### Plot things

# x.test <- x.sf %>% 
#   filter(pixel %in% 1:1000)
# #!is.na(mn_avgdens))

x.toplot <- x.sf %>% 
  filter(!is.na(mn_avgdens))

tm.obj <- tm_shape(rmap.base, bbox = st_bbox(x.sf)) + 
  tm_polygons() + 
  tm_shape(x.toplot) + 
  tm_fill(col = "mn_avgdens", border.col = "transparent",
          style = "fixed", breaks = col.breaks, palette = col.pal, 
          colorNA = NULL, showNA = FALSE, 
          title = "Whales / km2", legend.reverse = TRUE) + 
  tm_facets(by = "year", nrow = 3, ncol = 5, 
            free.coords = FALSE, free.scales = FALSE) +
  tm_legend(outside = TRUE, position = c("right", "center"), outside.size = 0.2,
            text.size = 2, title.size = 3) +
  # # Issue: graticules span the facet boxes, but the map does not..
  # tm_graticules(ticks = TRUE, lines = FALSE, n.y = 5, n.x = 5, labels.size = 1) +
  tm_layout(panel.label.size = 2.3)


if (!is.null(plot.path)) 
  tmap_save(tm.obj, filename = plot.path, width = 17, height = 9, units = "in", dpi = 300) #16 for _zoomed

# tm.obj
###############################################################################
