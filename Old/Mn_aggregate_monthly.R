# Aggregate humpback predictions by month, remove cells with 
#   only NA predictions, and create/save long data

###############################################################################
library(dplyr)
library(lubridate)
library(readr)
library(purrr)
library(tidyr)

source("User_script_local.R")
if (user == "KAF") {
  
  
} else if (user == "SMW") {
  path.mn.bidaily <- "C:/SMW/RAIMBOW/raimbow-local/Data/Humpback 3km models/WEAR3km_76_2005-01-01to2019-08-14_Bidaily_dens.csv"
  path.mn.bidaily.rds <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_bidaily_wide.rds"
  path.aggr1 <- "C:/SMW/RAIMBOW/whale-model-prep/Whalepreds_aggregate.R"
  path.aggr2 <- "C:/SMW/RAIMBOW/whale-model-prep/Whalepreds_aggregate_dates.R"
  
  file.out <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_monthly.RDS"
}

source(path.aggr1)
source(path.aggr2)


###############################################################################
### Read in Mn preds
# # Because csv file takes a long time to read in, it has been saved as an RDS file
# tmp <- read_csv(path.mn.bidaily, col_types = cols(.default = col_double()))
# saveRDS(tmp, path.mn.bidaily.rds)
# all.equal(data.frame(tmp), data.frame(mn.bidaily))
# #^differences are due to class of NA columns

mn.bidaily <- readRDS(path.mn.bidaily.rds)
head(names(mn.bidaily), 20)
tail(names(mn.bidaily))

x.orig <- mn.bidaily %>% 
  select(pixel, mlat, mlon, areakm, starts_with("76.dens.2"))

range.dates <- ymd(
  paste(rep(2005:2018, each = 12), sprintf("%02d", 1:12), "01", sep = "_"), 
  "2019_01_01"
)

### Average predictions by month, from Jan 2005 to Dec 2018
x.aggr <- whalepreds_aggregate(
  x.orig, 5:ncol(x.orig), 9:18, aggr.level = NULL, range.dates = range.dates, 
  se.calc = FALSE #Note: would need to change naming below if se.calc = TRUE
) %>% 
  set_names(c("pixel", "mlat", "mlon", "areakm", 
              paste("mn", rep(2005:2018, each = 12), sprintf("%02d", 1:12), sep = "_")))


### Make data long and save
x.aggr.long <- x.aggr %>% 
  gather(-pixel, -mlat, -mlon, -areakm, key = "col_name", value = "mn_avgdens") %>% 
  mutate(year = as.numeric(substr(col_name, 4, 7)), 
         month = as.numeric(substr(col_name, 9, 10)))

# # Shows that the NA values are all in the same pixels, and thus we can filter them out
# tmp2 <- x.aggr.long %>% 
#   group_by(pixel) %>% 
#   summarise(na_count = sum(is.na(mn_avgdens)), 
#             .groups = "drop")
# table(tmp2$na_count)

x.aggr.long.nona <- x.aggr.long %>% filter(!is.na(mn_avgdens))

saveRDS(x.aggr.long.nona, file = file.out)

# ### Plot things if desired
# library(sf)
# d <- x.aggr.long %>% 
#   filter(year == 2005, month == 1) %>% 
#   sf::st_as_sf(coords = c("mlon", "mlat"), crs = 4326)
# plot(d[4], axes = T, pch = 19, cex = 0.4)

###############################################################################
