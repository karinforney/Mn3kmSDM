# Mn3kmSDM
Code for figures for 3km humpback SDM

## Prep files

* Mn_aggregate_monthly.R: Average bidaily 3km predictions by month for use throughout the rest of the repo. Remove pixels that have NA predictions acorss entire time series of predictions (i.e. those on land)

## Figures

* File: Mn_monthly_multipanel. Todo. Multi-panel with a set of monthly-averaged whale models –  maybe Jan, Apr, Jul, Oct of as columns, and ~2012-2017 (or 2013-2016?) as rows, to show variation in predicted whale distributions? Also: S2-S15 - 12-panel monthly model outputs for all years 2005-2018?

* File: Mn_monthly_multipanel.R: Multi-panel plots of monthly averaged predictions. Contains code for 1) a subset on a single page - January, April, July, and October for 2012-2017 and 2) all year-months, each year is a single page of plots

* File: Mn_timeseries_USWC.R. Time series plot of humpback whale abundance by region along the whole US West Coast

* File: Mn_timeseries_MRY.R. Time series plot of humpback whale abundance and MBWW data for MRY region (currently 36.5 – 37.0, -122.10). 

* File: todo. Subset of aerial survey comparisons for cenCA, OR/WA - Karin still needs to process the airDAS file for plotting in R.
