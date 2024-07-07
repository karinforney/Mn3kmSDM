# Code for aggregating Mn predictions by month, and then overlaying them
#   onto 5km grid with land erased

###############################################################################
library(tidyverse)
library(sf)
library(eSDM)
library(here)

source(here::here("R", "functions", "Whalepreds_aggregate.R"))
monthly.from.rds <- TRUE #If FALSE, aggregates the bidaily predictions by month and saves

path.mnpreds.bidaily.rds <- here("data", "Mn_3km_2005-01-01_to_2020-09-29_bidaily_dens.rds")
path.mnpreds.monthly.rds <- here("data", "Mn_3km_2005-01-01_to_2020-09-29_monthly_dens.rds")
path.mn.long <- here("data", "products", "Humpback_5km_long_monthly.rds")


###############################################################################
# Switch for if monthly preds have been created, and so can just be read from RDS
if (monthly.from.rds) {
  mn.monthly <- readRDS(path.mnpreds.monthly.rds)
} else {
  # Read in KAF 3km Mn predictions
  mn.bidaily <- readRDS(path.mnpreds.bidaily.rds)
  
  head(names(mn.bidaily), 20)
  tail(names(mn.bidaily))
  
  # Aggregate them by month
  mn.bidaily.prep <- mn.bidaily %>%
    select(pixel, mlat, mlon, areakm, starts_with("76.dens"))
  
  # # Option 1: provide date vector
  # date.max <- as.Date("2020-09-27")
  # range.dates <- seq(from = as.Date("2005-01-01"), to = date.max, by = aggr.by)
  # Option 2: use aggr.level. Takes ~3 min
  aggr.by <- "months"
  mn.monthly <- whalepreds_aggregate(
    mn.bidaily.prep, 5:ncol(mn.bidaily.prep), 9:18, aggr.level = "monthly",
    range.dates = NULL, se.calc = TRUE
  ) %>%
    set_names(gsub("Avg_monthly_", "Mn_dens_", names(.))) %>%
    set_names(gsub("SE_monthly_", "Mn_se_", names(.))) %>%
    relocate(pixel, mlat, mlon, areakm)
  
  saveRDS(mn.monthly, file = path.mnpreds.monthly.rds)
}


###############################################################################
# Remove rows that only have NA values. Then create sf object to overlay
# ~1.5 min
mn.monthly.nona <- mn.monthly %>% 
  # select(mlon, mlat, areakm, starts_with("Mn_dens")) %>% 
  rowwise() %>% 
  filter(!all(is.na(c_across(starts_with("Mn_dens"))))) %>% 
  ungroup()

str_which(names(mn.monthly.nona.sf), "Mn_")
mn.monthly.nona.sf <- mn.monthly.nona %>% 
  eSDM::pts2poly_centroids(0.027 / 2, crs = 4326, agr = "constant") %>% 
  select(-areakm)

# # Sanity check
# d.area <- as.numeric(units::set_units(st_area(mn.monthly.nona.sf), "km^2"))
# summary(d.area - mn.monthly.nona.sf$areakm)


###############################################################################
# Overlay predictions onto land-erased, 5km grid
grid.5km.lno <- readRDS( here("data", "Grid_5km_landerased.rds"))

sf_use_s2(FALSE) # <2 min with sf_use_s2(FALSE); otherwise >15 min
tmp.over <- eSDM::overlay_sdm( 
  st_geometry(grid.5km.lno), mn.monthly.nona.sf, 
  str_which(names(mn.monthly.nona.sf), "Mn_"), #ignores geometry column
  overlap.perc = 0
)


###############################################################################
# Process overlaid data
mn.preds.lno.5kmover <- tmp.over %>%
  mutate(GRID5KM_ID = grid.5km.lno$GRID5KM_ID,
         area_km_lno = as.numeric(units::set_units(st_area(geometry), "km^2"))) %>%
  select(GRID5KM_ID, area_km_lno, starts_with("Mn_")) 

mn.long <- mn.preds.lno.5kmover %>% 
  st_drop_geometry() %>%
  pivot_longer(cols = starts_with("Mn_dens"), values_to = "Humpback_dens_mean") %>% 
  filter(!is.na(Humpback_dens_mean)) %>% 
  mutate(date = ymd(str_sub(name, 9, 18)), 
         Humpback_dens_se = NA_real_) %>%
  select(GRID5KM_ID, area_km_lno, date, Humpback_dens_mean, Humpback_dens_se) %>% 
  arrange(date, GRID5KM_ID)

saveRDS(mn.long, file = path.mn.long)

# # Sanity check
# d <- readRDS(file.path("C:/SMW/SMW-Misc/Forney-dynamic-mn/raimbow-local", 
#                        "Outputs/Humpback_5km_long_monthly.rds"))
# summary(d$Humpback_dens_mean - mn.long$Humpback_dens_mean)
