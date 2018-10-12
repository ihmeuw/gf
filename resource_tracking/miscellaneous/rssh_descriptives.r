# -------------------------------------------------
# David Phillips
#
# 9/25/2018
# Simple analysis of RSSH for 2018 country reports
# -------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
library(readxl)
# ------------------


# -------------------------------------------------------------------------
# Directories and files

# directory
dir = 'J:/Project/Evaluation/GF/resource_tracking/multi_country/mapping/'

# files
inFile = paste0(dir, 'total_resource_tracking_data.csv')
# -------------------------------------------------------------------------


# -------------------------------------------
# Load/prep data

# load resource tracking data
data = fread(inFile)

# subset to recent budgets only
data = data[data_source=='fpm' & year>2013]

# identify windows
data[country=='Guatemala' & year>2017, window:='GTM 2018-2020']
data[country=='Guatemala' & year<=2017, window:='GTM 2014-2017']
data[country=='Uganda' & year>2016, window:='UGA 2017-2020']
data[country=='Uganda' & year<=2016, window:='UGA 2014-2016']
data[country=='Congo (Democratic Republic)' & year>2016, window:='DRC 2017-2020']
data[country=='Congo (Democratic Republic)' & year<=2016, window:='DRC 2014-2016']

# ignore program management
data[gf_module=='Program management', code:=NA]

# replace "all" with "unspecified" because it comes from summary budgets
data[sda_activity=='All', sda_activity:='Unspecified (Summary budget)']
data[grant_number %in% c('UGD-708-G07-H', 'UGD-011-G10-S'), sda_activity:='Unspecified (Summary budget)']
# -------------------------------------------


# -------------------------------------------
# Descriptives

# largest RSSH modules by country and window
byVars = by=c('window','gf_module')
modTotals = data[grepl('R', code), .(budget=sum(budget), 
		grants=paste(unique(grant_number), collapse=', ')), byVars]
modTotals = modTotals[order(-budget)]
modTotals[, rank:=seq(.N), by='window']
modTotals[grepl('DRC',window) & rank<4][order(window)]
modTotals[grepl('UGA',window) & rank<4][order(window)]
modTotals[grepl('GTM',window) & rank<4][order(window)]

# largest RSSH interventions by country and window
byVars = by=c('window','abbrev_module','abbrev_intervention')
intTotals = data[grepl('R', code), .(budget=sum(budget), 
		grants=paste(unique(grant_number), collapse=', ')), byVars]
intTotals = intTotals[order(-budget)]
intTotals[, rank:=seq(.N), by='window']
intTotals[grepl('DRC',window) & rank<4][order(window)]
intTotals[grepl('UGA',window) & rank<4][order(window)]
intTotals[grepl('GTM',window) & rank<4][order(window)]

# largest RSSH activities by country and window
byVars = by=c('window','abbrev_module','abbrev_intervention', 'sda_activity')
activityTotals = data[grepl('R', code), .(budget=sum(budget), 
		grants=paste(unique(grant_number), collapse=', ')), byVars]
activityTotals = activityTotals[order(-budget)]
activityTotals[, rank:=seq(.N), by='window']
activityTotals[grepl('DRC',window) & rank<4][order(window)]
activityTotals[grepl('UGA',window) & rank<4][order(window)]
activityTotals[grepl('GTM',window) & rank<4][order(window)]

# largest RSSH activities in latest window
vars = c('abbrev_module','abbrev_intervention','sda_activity','budget','grants')
activityTotals[window=='UGA 2017-2020' & rank<9, vars]
activityTotals[window=='DRC 2017-2020' & rank<9, vars]
activityTotals[window=='GTM 2018-2020' & rank<9, vars]

# largest RSSH activities by country and window
byVars = by=c('country','start_date', 'sda_activity')
activityTotalsY = data[grepl('R', code), .(budget=sum(budget)), byVars]
activityTotalsY = activityTotalsY[order(-budget)]
bigActivities = unique(activityTotals[window=='UGA 2017-2020' & rank<10]$sda_activity)
tmp = activityTotalsY[country=='Uganda' & sda_activity %in% bigActivities]
dcast(tmp, country+sda_activity~start_date)
# -------------------------------------------
