# Plot time series of humpback abundance coastwide

###############################################################################
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

source("User_script_local.R")
if (user == "KAF") {
  
  
} else if (user == "SMW") {
  path.mn.bidaily <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_bidaily_wide.rds"
  path.mn.monthly <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_monthly.RDS"
  
  file.out.uswc <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_timeseries_coastwide.png"
}


###############################################################################
# Data prep for monthly predictions

### Data prep 
x.monthly.orig <- readRDS(path.mn.monthly)

# Assign regions - consistent with RAIMBOW work, CDFW fishing blocks
#   A grid cell is included in a latitude band if it is greater than or equal to 
#   the minimum value and less than the max value, e.g. a [, ) interval
reg.bound <- c(32, 34.5, 36.3, 38.833, 42, 46.25, 50)
reg.names <- c("CA-S", "CA-SCen", "CA-Cen", "CA-N", "OR", "WA")
stopifnot(length(reg.names) == length(reg.bound) - 1)

x <- x.monthly.orig %>% 
  mutate(mn_abund = mn_avgdens * areakm, 
         region_idx = findInterval(mlat, reg.bound, left.open = TRUE), 
         region = factor(reg.names[region_idx], levels = rev(reg.names))) 


x.summ <- x %>% 
  group_by(region, year, month) %>% 
  summarise(mn_abund_sum = sum(mn_abund, na.rm = TRUE), 
            .groups = "drop") %>% 
  mutate(year_mo = paste(year, sprintf("%02d", month), sep = "-"))


###############################################################################
# Plot complete time series of Mn abundance along US West Coast

x.max <- length(unique(x.summ$year_mo))
x.lab.idx <- seq(1, to = x.max, by = 3)
x.lab <- sort(unique(x.summ$year_mo))[x.lab.idx]

region.cols <- c(brewer.pal(5, "Set1"), "gray50")


### Plot and save
x.timeseries <- x.summ %>% 
  # filter(Region %in% c()) %>% #filter for regions if desired
  ggplot(aes(year_mo, mn_abund_sum, colour = region, group = region)) + 
  geom_point() + 
  geom_path() + 

  scale_colour_manual(values = region.cols, name = "Region", drop = TRUE) + 
  # scale_colour_brewer(palette = "Set1", name = "Region", drop = TRUE) +
  
  coord_cartesian(xlim = c(-1, x.max)) + 
  scale_x_discrete(breaks = x.lab) + 
  xlab (NULL) + 
  ylab("Whales") + 
  ggtitle("Humpback abundance - US West Coast - 2005 to 2018") + 
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.4), 
        legend.justification = "center")

x.timeseries

ggsave(filename = file.out.uswc, plot = x.timeseries, width = 12, height = 6)

###############################################################################
