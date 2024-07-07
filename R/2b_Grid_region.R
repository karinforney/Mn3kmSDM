### Define and save regions used in other parts of humpback_risk
# Uses the output from "2_Whale_risk.Rmd", hence the 2b

# These regions are visualized (plotted) in "3_Whale_risk_timeseries.Rmd" and 
#   "3_Whale_risk_county_timeseries.Rmd"

# NOTE: This script is based on the file of the same name in
#   https://github.com/jameals/raimbow/tree/master/humpback_risk

###############################################################################
library(dplyr)
library(here)

all.df <- readRDS(here("data", "products", "Whale_risk_long_nona.rds"))

# Large regions: WA, OR, CA-N, CA-Cen, CA-SCen, CA-S
# These region boundaries must match those in 'grid-prep/Grid5km_raimbow_prep.Rmd'
reg.bound <- c(32.5, 34.4, 36.3, 38.833, 42, 46.25, 50)
reg.names <- c("CA-S", "CA-SCen", "CA-Cen", "CA-N", "OR", "WA")
stopifnot(length(reg.names) == length(reg.bound) - 1)

grid.region <- all.df %>% 
  filter(!duplicated(GRID5KM_ID)) %>% 
  arrange(GRID5KM_ID) %>% 
  mutate(region.idx = findInterval(LATITUDE, reg.bound, left.open = TRUE), 
         region = factor(reg.names[region.idx], levels = rev(reg.names))) %>% 
  select(GRID5KM_ID, region)

# save(reg.bound, reg.names, grid.region, file = file.out.region)
saveRDS(grid.region, here("data", "products", "Grid_region.rds"))
saveRDS(reg.names, here("data", "products", "Grid_region_names.rds"))
# file = paste0(path.rdata, "Grid_region.Rdata")


###############################################################################
# Counties
reg.bound.county <- c(
  42, 41.5, 40, 38.83, 38.3, 
  37.8, 37.7, 37.1, 36.83, 35.8, 
  35, 34.375, 34, 33.75, 33.35, 
  32.5
)

reg.list.county <- list(
  "Del_Norte"       = c(reg.bound.county[1], reg.bound.county[2]), 
  "Humboldt"        = c(reg.bound.county[2], reg.bound.county[3]), 
  "Mendocino"       = c(reg.bound.county[3], reg.bound.county[4]), 
  "Sonoma"          = c(reg.bound.county[4], reg.bound.county[5]), 
  "Marin"           = c(reg.bound.county[5], reg.bound.county[6]), 
  "San_Francisco"   = c(reg.bound.county[6], reg.bound.county[7]), 
  "San_Mateo"       = c(reg.bound.county[7], reg.bound.county[8]), 
  "Santa_Cruz"      = c(reg.bound.county[8], reg.bound.county[9]), 
  "Monterey"        = c(reg.bound.county[9], reg.bound.county[10]), 
  "San_Luis_Obispo" = c(reg.bound.county[10], reg.bound.county[11]), 
  "Santa_Barbara"   = c(reg.bound.county[11], reg.bound.county[12]), 
  "Ventura"         = c(reg.bound.county[12], reg.bound.county[13]), 
  "Los_Angeles"     = c(reg.bound.county[13], reg.bound.county[14]), 
  "Orange"          = c(reg.bound.county[14], reg.bound.county[15]), 
  "San_Diego"       = c(reg.bound.county[15], reg.bound.county[16])
)

reg.names.county <- names(reg.list.county)

# Add in WA and OR
reg.bound.county <- rev(c(50, 46.25, reg.bound.county))
reg.names.county <- rev(c("WA", "OR", reg.names.county))
stopifnot(length(reg.bound.county) == length(reg.names.county) + 1)

grid.region.county <- all.df %>%
  filter(!duplicated(GRID5KM_ID)) %>%
  arrange(GRID5KM_ID) %>%
  mutate(region.idx = findInterval(LATITUDE, reg.bound.county, 
                                   left.open = TRUE),
         region = factor(reg.names.county[region.idx], 
                         levels = rev(reg.names.county))) %>%
  select(GRID5KM_ID, region)

# save(
#   reg.bound.county, reg.names.county, grid.region.county, 
#   file = file.out.region.county
# )
saveRDS(grid.region.county, here("data", "products", "Grid_region_county.rds"))

###############################################################################
