# ------------------------------------------------------
# David Phillips
# 
# 11/6/2019
# Check for evidence that stockouts affect utilization
# ------------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
# -------------------

# --------------------------------------------------------------------------
# Files and directories
# (suspected cases for 2017 is in base, suspected cases after 2017 is in its own file for some reason)
dir = 'J:/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/3_prepped/'
baseFile = paste0(dir, 'base/base_services_prepped.rds')
suspectFile = paste0(dir, 'base/base_services_prepped_suspectedCases.rds')
siglFile = paste0(dir, 'sigl/sigl_prepped.rds')
# --------------------------------------------------------------------------


# ------------------------------------
# Load data
base = readRDS(baseFile)
suspectedCases = readRDS(suspectFile)
sigl = readRDS(siglFile)

# list element IDs of interest
stockoutElement = 'o3S4pDMPNoO'
utilizationElement = 'CIzQAR8IWH1'
suspectedElement ='aZwnLALknnj'
# ------------------------------------


# --------------------------------------------------------------------------
# Prep data

# subset rows
data = base[element_id %in% c(utilizationElement, suspectedElement)]
stockouts = sigl[element_id == stockoutElement]

# rbind data sets together
data = rbind(data, stockouts, fill=TRUE)
data = rbind(data, suspectedCases, fill=TRUE)

# collapse out subpopulations and subset columns
byVars = c('dps','health_zone','org_unit','org_unit_id','date','element_id')
data = data[, .(value=sum(value, na.rm=T)), by=byVars]

# add nicer labels
data[element_id==stockoutElement, variable:='days_out_of_stock']
data[element_id==utilizationElement, variable:='rdts_performed']
data[element_id==suspectedElement, variable:='suspected_cases']

# reshape wide
data = dcast(data, dps+health_zone+org_unit+org_unit_id+date~variable)

# display national numbers
data[, .(days_out_of_stock=sum(days_out_of_stock,na.rm=T), 
		rdts_performed=sum(rdts_performed,na.rm=T), 
		suspected_cases=sum(suspected_cases,na.rm=T)), by=date]

# compute proportion of suspected cases tested
data[, prop_tested:=rdts_performed/suspected_cases]
data[prop_tested>1, prop_tested:=NA]

# get proportion of time stocked out
data[, prop_stocked_out:=days_out_of_stock/31]
data[prop_stocked_out>1, prop_stocked_out:=NA]

# compute any stockouts
data[, any_stockouts:=prop_stocked_out>0]

# create lag variables
byVars = c('dps','health_zone','org_unit','org_unit_id')
data[, prop_stocked_out_lag:=shift(prop_stocked_out), by=byVars]
data[, any_stockouts_lag:=prop_stocked_out_lag>0]
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Run analysis

# view a sample of facilities
sample = sample(unique(data[days_out_of_stock!=0 & !is.na(prop_tested)]$org_unit), 9)
ggplot(data[org_unit %in% sample], aes(y=prop_tested, x=date)) + 
	geom_line() + 
	geom_line(aes(y=prop_stocked_out), color='red') + 
	facet_wrap(~org_unit) + 
	theme_bw()

# check if having any stockouts correlates with lower testing coverage
agg = data[, .(rdts_performed=sum(rdts_performed, na.rm=T), 
				suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts]
agg[, prop_tested:=rdts_performed/suspected_cases]
agg

# check if having any stockouts in the previous month correlates with lower testing coverage
agg_lag = data[, .(rdts_performed=sum(rdts_performed, na.rm=T), 
					suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts_lag]
agg_lag[, prop_tested:=rdts_performed/suspected_cases]
agg_lag

# check for statistical significance using OLS
summary(lm(prop_tested~any_stockouts, data))
summary(lm(prop_tested~any_stockouts_lag, data))
# --------------------------------------------------------------------------

