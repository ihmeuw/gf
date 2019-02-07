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
library(stringr)
# ------------------


# -------------------------------------------------------------------------
# Directories and files

# directory
j = ifelse(Sys.info()[1]=='Windows','J:','/home/j')
dir = paste0(j, '/Project/Evaluation/GF/resource_tracking/multi_country/mapping/')

# files
inFile = paste0(dir, 'total_resource_tracking_data.csv')

# output files for reference
codRefFile = paste0(j,'/Project/Evaluation/GF/resource_tracking/cod/rssh_activities.csv')
# -------------------------------------------------------------------------


# -------------------------------------------
# Load/prep data

# load resource tracking data
allData = fread(inFile)
data = copy(allData)

# subset to recent budgets only
data = data[data_source=='fpm' & as.numeric(str_sub(grant_period,1,4))>2013]

# ignore program management
data[gf_module=='Program management', code:=NA]

# replace "all" with "unspecified" because it comes from summary budgets
data[sda_activity=='All', sda_activity:='Unspecified (Summary budget)']
data[grant_number %in% c('UGD-708-G07-H', 'UGD-011-G10-S'), sda_activity:='Unspecified (Summary budget)']

# subset to RSSH
data = data[grepl('R', code)]

# iso codes
data[country=='Guatemala', iso3:='GTM']
data[country=='Uganda', iso3:='UGA']
data[grepl('Congo', country), iso3:='COD']
# -------------------------------------------


# -------------------------------------------
# Descriptives

# largest RSSH modules by country and grant_period
byVars = by=c('grant_period','abbrev_module','iso3')
modTotals = data[, .(budget=sum(budget), 
		grants=paste(unique(grant_number), collapse=', ')), byVars]
modTotals = modTotals[order(-budget)]
modTotals[, rank:=seq(.N), by='grant_period']
modTotals[iso3=='COD' & rank<4][order(grant_period)]
modTotals[iso3=='UGA' & rank<4][order(grant_period)]
modTotals[iso3=='GTM' & rank<4][order(grant_period)]

# largest RSSH interventions by country and grant_period
byVars = by=c('grant_period','abbrev_module','abbrev_intervention','iso3')
intTotals = data[, .(budget=sum(budget), 
		grants=paste(unique(grant_number), collapse=', ')), byVars]
intTotals = intTotals[order(-budget)]
intTotals[, rank:=seq(.N), by='grant_period']
intTotals[iso3=='COD' & rank<4][order(grant_period)]
intTotals[iso3=='UGA' & rank<4][order(grant_period)]
intTotals[iso3=='GTM' & rank<4][order(grant_period)]

# largest RSSH activities by country and grant_period
byVars = by=c('grant_period','abbrev_module','abbrev_intervention', 'sda_activity','iso3')
activityTotals = data[, .(budget=sum(budget), 
		grants=paste(unique(grant_number), collapse=', ')), byVars]
activityTotals = activityTotals[order(-budget)]
activityTotals[, rank:=seq(.N), by='grant_period']
l = 70
activityTotals[, sda_activity:=str_sub(sda_activity,1,l)]
activityTotals[iso3=='COD' & rank<4][order(grant_period)]
activityTotals[iso3=='UGA' & rank<4][order(grant_period)]
activityTotals[iso3=='GTM' & rank<4][order(grant_period)]

# largest RSSH activities in latest grant_period
vars = c('abbrev_module','abbrev_intervention','sda_activity','budget','grants','iso3')
activityTotals[iso3=='COD' & grant_period=='2018-2020' & rank<9, vars, with=F]

# largest RSSH activities by country and grant_period
byVars = by=c('iso3','start_date', 'sda_activity')
activityTotalsY = data[grant_period=='2018-2020']
activityTotalsY = activityTotalsY[, .(budget=sum(budget)), byVars]
activityTotalsY[, sda_activity:=str_sub(sda_activity,1,l)]
activityTotalsY = activityTotalsY[order(-budget)]
bigActivities = unique(activityTotals[iso3=='COD' & grant_period=='2018-2020' & rank<10]$sda_activity)
tmp = activityTotalsY[iso3=='COD' & sda_activity %in% bigActivities]
tmp = tmp[, .(budget=sum(budget)), by=byVars]
dcast(tmp, iso3+sda_activity~start_date)
# -------------------------------------------


# -------------------------------------------
# Save reference files
write.csv(activityTotals[iso3=='COD'], codRefFile, row.names=F)
# -------------------------------------------
