# Plot time series of humpback abundance for selected area (Monterey) 
#   to compare with MBWW data

###############################################################################
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

source("User_script_local.R")
if (user == "KAF") {
  
  
} else if (user == "SMW") {
  path.mn.bidaily <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_bidaily_wide.rds"
  path.mn.monthly <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_monthly.RDS"
  path.mbww <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/MBWW_Humpback Whale_per_halfday_trip_2003-2019.csv"
  
  file.out.mry.monthly <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_timeseries_monterrey_monthly.png"
  file.out.mry.bidaily <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_timeseries_monterrey_bidaily.png"
}


###############################################################################
### MBWW data prep
y <- read.csv(path.mbww) %>% 
  mutate(WhalesPerHalfDay = ifelse(is.na(WhalesPerHalfDay), 0, WhalesPerHalfDay), 
         Date_orig = Date, 
         Date = mdy(Date_orig), 
         year = lubridate::year(Date), 
         month = lubridate::month(Date)) %>% 
  filter(between(.data$year, 2005, 2018))

# Summarize by month - provide different summary options
y.monthly <- y %>% 
  group_by(year, month) %>% 
  summarise(WhalesPerHalfDay_avg = mean(WhalesPerHalfDay), 
            WhalesPerHalfDay_sum = sum(WhalesPerHalfDay), 
            WhalesPerHalfDay_max = max(WhalesPerHalfDay), 
            .groups = "drop")


###############################################################################
# Data prep - SDM predictions

### Clip data to MRY study area
mry.lon.min <- -122.10
mry.lat.range <- c(36.5, 37)


#------------------------------------------------------------------------------
### Clip and process monthly whale data
x.monthly.orig <- readRDS(path.mn.monthly)
x.mry.monthly.summ <- x.monthly.orig %>% 
  filter(mry.lon.min <= mlon, between(mlat, mry.lat.range[1], mry.lat.range[2])) %>% 
  mutate(mn_abund = mn_avgdens * areakm) %>% 
  group_by(year, month) %>% 
  summarise(mn_abund_sum = sum(mn_abund, na.rm = TRUE), 
            .groups = "drop")


#------------------------------------------------------------------------------
### Clip and process bidaily whale data
x.bidaily.wide <- readRDS(path.mn.bidaily)

x.mry.bidaily <- x.bidaily.wide %>% 
  select(pixel, mlat, mlon, areakm, starts_with("76.dens.2")) %>% 
  filter(mry.lon.min <= mlon, between(mlat, mry.lat.range[1], mry.lat.range[2])) %>% 
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


###############################################################################
# Plot timeseries comparing MDWW data and 3km Mn preds

#------------------------------------------------------------------------------
### Plot monthly stuff
z.monthly <- x.mry.monthly.summ %>% 
  left_join(y.monthly, by = c("year", "month")) %>% 
  select(year, month, SDM = mn_abund_sum, MBWW = WhalesPerHalfDay_avg) %>% 
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
  ggtitle("Mn 3km SDM average monthly abundance vs MBWW avg value for the month") + 
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
