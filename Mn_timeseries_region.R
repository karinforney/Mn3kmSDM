# Plot time series of humpback abundance, both coastwide and for selected area
#   to compare with MBWWdata

###############################################################################
library(dplyr)
library(ggplot2)
library(RColorBrewer)

source("User_script_local.R")
if (user == "KAF") {
  
  
} else if (user == "SMW") {
  path.mn.bidaily <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_bidaily_wide.rds"
  path.mn.monthly <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_monthly.RDS"
  path.mbww <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/MBWW_Humpback Whale_per_halfday_trip_2003-2019.csv"
  
  file.out.uswc <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_timeseries_coastwide.png"
  file.out.mry.monthly <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_timeseries_monterrey_monthly.png"
  file.out.mry.bidaily <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_timeseries_monterrey_bidaily.png"
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
# vert.lines <- seq(0.5, to = x.max, by = 12)

region.cols <- c(brewer.pal(5, "Set1"), "gray50")


### Plot and save
x.timeseries <- x.summ %>% 
  # mutate(DC_season = factor(1)) %>%
  ggplot(aes(year_mo, mn_abund_sum, colour = region, group = region)) + #, linetype = DC_season)) + 
  geom_point() + 
  geom_path() + 
  # geom_vline(xintercept = vert.lines, col = "black", lwd = 0.35) +
  
  scale_colour_manual(values = region.cols, name = "Region") + 
  # scale_colour_brewer(palette = "Set2", name = "Region", drop = FALSE) +
  # guides(linetype = guide_legend(title = "DC season", label = FALSE, 
  #                                override.aes = list(colour = "black"))) + 
  
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
# Plot timeseries comparing MBWW data and 3km Mn preds
#   Study area: longitude < -122.10, latitude [36.5, 37]

#------------------------------------------------------------------------------
### Data prep
# Read and process MBWW data
y <- read.csv(path.mbww) %>% 
  mutate(WhalesPerHalfDay = ifelse(is.na(WhalesPerHalfDay), 0, WhalesPerHalfDay), 
         Date_orig = Date, 
         Date = mdy(Date_orig), 
         year = lubridate::year(Date), 
         month = lubridate::month(Date)) %>% 
  filter(between(.data$year, 2005, 2018))

y.monthly <- y %>% 
  group_by(year, month) %>% 
  summarise(WhalesPerHalfDay_avg = mean(WhalesPerHalfDay), 
            WhalesPerHalfDay_sum = sum(WhalesPerHalfDay), 
            WhalesPerHalfDay_max = max(WhalesPerHalfDay), 
            .groups = "drop")

# Clip whale data to study area, including loaded bidaily data
x.mry.monthly.summ <- x.monthly.orig %>% 
  filter(mlon < -122.10, between(mlat, 36.5, 37)) %>% 
  mutate(mn_abund = mn_avgdens * areakm) %>% 
  group_by(year, month) %>% 
  summarise(mn_abund_sum = sum(mn_abund, na.rm = TRUE), 
            .groups = "drop")

# Load bidaily data, and sum abundance w/in studyarea
x.bidaily.wide <- readRDS(path.mn.bidaily)

x.mry.bidaily <- x.bidaily.wide %>% 
  select(pixel, mlat, mlon, areakm, starts_with("76.dens.2")) %>% 
  filter(mlon < -122.10, between(mlat, 36.5, 37)) %>% 
  gather(-pixel, -mlat, -mlon, -areakm, key = "col_name", value = "mn_dens") %>% 
  mutate(mn_abund = mn_dens * areakm, 
         date = lubridate::ymd(substr(.data$col_name, 9, 18))) %>% 
  filter(between(lubridate::year(.data$date), 2005, 2018))
length(unique(x.mry.bidaily$pixel)) #2044 grid cells in study area

x.mry.bidaily.summ <- x.mry.bidaily %>% 
  group_by(date) %>% 
  summarise(mn_abund_sum = sum(mn_abund, na.rm = TRUE), 
            .groups = "drop") #%>% 
# mutate(year = lubridate::year(.data$date), 
#        month = lubridate::month(.data$date), 
#        day = lubridate::day(.data$date))


#------------------------------------------------------------------------------
### Plot monthly stuff
z.monthly <- x.mry.monthly.summ %>% 
  left_join(y.monthly, by = c("year", "month")) %>% 
  select(year, month, SDM = mn_abund_sum, MBWW = WhalesPerHalfDay_max) %>% 
  gather(-year, -month, key = "source", value = "mn_abund") %>% 
  mutate(date = ymd(paste0(paste(.data$year, .data$month, 1, sep = "-"))), 
         year_mo = paste(.data$year, sprintf("%02d", .data$month), sep = "-"))


z.monthly.plot <- z.monthly %>% 
  ggplot(aes(date, mn_abund, colour = source, group = source)) + 
  geom_point() + 
  geom_path() + 
  scale_colour_manual(values = c("red", "dodgerblue2"), name = "Source") + 
  scale_x_date(date_labels = "%Y-%m", #expand = c(0.01, 0.01), 
               date_breaks = "3 months", date_minor_breaks = "1 month") + 
  xlab (NULL) + 
  ylab("Whales") + 
  ggtitle("Mn 3km SDM average monthly abundance vs MBWW max value for the month") + 
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.4), 
        legend.justification = "center")

ggsave(filename = file.out.mry.monthly, plot = z.monthly.plot, width = 12, height = 6)


#------------------------------------------------------------------------------
### Plot bidaily stuff
z.bidaily <- x.mry.bidaily.summ %>% 
  rename(SDM = mn_abund_sum) %>% 
  left_join(select(y, Date, MBWW = WhalesPerHalfDay), by = c("date" = "Date")) %>% 
  gather(-date, key = "source", value = "mn_abund") %>% 
  mutate(year_mo_day = paste(lubridate::year(.data$date),
                             sprintf("%02d", lubridate::month(.data$date)),
                             sprintf("%02d", lubridate::day(.data$date)),
                             sep = "-"))

z.bidaily.plot <- z.bidaily %>% 
  ggplot(aes(date, mn_abund, colour = source, group = source)) + 
  geom_point() + 
  geom_path() + 
  scale_colour_manual(values = c("red", "dodgerblue2"), name = "Source") + 
  scale_x_date(date_labels = "%Y-%m-%d", #expand = c(0.01, 0.01), 
               date_breaks = "3 months", date_minor_breaks = "1 month") + 
  xlab (NULL) + 
  ylab("Whales") + 
  ggtitle("Bidaily - Mn 3km SDM abundance vs MBWW value") + 
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = 0.4), 
        legend.justification = "center")

ggsave(filename = file.out.mry.bidaily, plot = z.bidaily.plot, width = 18, height = 6)


###############################################################################
