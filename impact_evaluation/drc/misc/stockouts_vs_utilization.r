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
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/3_prepped/')
baseFile = paste0(dir, 'base/base_services_prepped.rds')
suspectFile = paste0(dir, 'base/base_services_prepped_suspectedCases.RDS')
siglFile = paste0(dir, 'sigl/sigl_prepped.rds')
# --------------------------------------------------------------------------


# ------------------------------------
# Load data
base = readRDS(baseFile)
suspectedCases = readRDS(suspectFile)
sigl = readRDS(siglFile)

# list element IDs of interest
rdtStockoutElement = 'o3S4pDMPNoO'
alStockoutElement = 'vA9TrUHkDoP'
asaqStockoutElement = 'o3S4pDMPNoO'
utilizationElement = 'CIzQAR8IWH1'
suspectedElement ='aZwnLALknnj'
confirmedElement = 'rfeqp2kdOGi'
treatedElement = 'pMbC0FJPkcm'
# ------------------------------------


# --------------------------------------------------------------------------
# Prep data

# subset rows
data = base[element_id %in% c(utilizationElement, suspectedElement, confirmedElement, treatedElement)]
stockouts = sigl[element_id == rdtStockoutElement]

# rbind data sets together
data = rbind(data, stockouts, fill=TRUE)
data = rbind(data, suspectedCases, fill=TRUE)

# collapse out subpopulations and subset columns
byVars = c('dps','health_zone','org_unit','org_unit_id','date','element_id')
data = data[, .(value=sum(value, na.rm=T)), by=byVars]

# add nicer labels
data[element_id==rdtStockoutElement, variable:='days_out_of_stock']
data[element_id==utilizationElement, variable:='rdts_performed']
data[element_id==suspectedElement, variable:='suspected_cases']
data[element_id==confirmedElement, variable:='confirmed_cases']
data[element_id==treatedElement, variable:='confirmed_cases_treated']

# reshape wide
data = dcast(data, dps+health_zone+org_unit+org_unit_id+date~variable)

# display national numbers
national = data[, .(days_out_of_stock=sum(days_out_of_stock,na.rm=T), 
		mean_days_out_of_stock=mean(days_out_of_stock,na.rm=T),
		mean_facilities_with_any_stockouts=mean(days_out_of_stock>0,na.rm=T),
		rdts_performed=sum(rdts_performed,na.rm=T), 
		suspected_cases=sum(suspected_cases,na.rm=T), 
		confirmed_cases=sum(confirmed_cases,na.rm=T)), by=date]
national
ggplot(national, aes(y=days_out_of_stock, x=date)) + geom_point() + geom_smooth()
ggplot(national, aes(y=mean_days_out_of_stock, x=date)) + geom_point() + geom_smooth()
ggplot(national, aes(y=mean_facilities_with_any_stockouts, x=date)) + geom_point() + geom_smooth()

# compute proportion of suspected cases tested, confirmed and treated
data[, prop_tested:=rdts_performed/suspected_cases]
data[, prop_confirmed:=confirmed_cases/suspected_cases]
data[, prop_treated:=confirmed_cases_treated/suspected_cases]
data[prop_tested>1, prop_tested:=NA]
data[prop_confirmed>1, prop_confirmed:=NA]
data[prop_treated>1, prop_treated:=NA]

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
# sample = sample(unique(data[days_out_of_stock!=0 & !is.na(prop_tested)]$org_unit), 9)
# ggplot(data[org_unit %in% sample], aes(y=prop_tested, x=date)) + 
	# geom_line() + 
	# geom_line(aes(y=prop_stocked_out), color='red') + 
	# facet_wrap(~org_unit) + 
	# theme_bw()

# check if having any stockouts correlates with lower testing coverage
agg1 = data[, .(rdts_performed=sum(rdts_performed, na.rm=T), 
				suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts]
agg1[, prop_tested:=rdts_performed/suspected_cases]
agg1

# check if having any stockouts in the previous month correlates with lower testing coverage
agg1_lag = data[, .(rdts_performed=sum(rdts_performed, na.rm=T), 
					suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts_lag]
agg1_lag[, prop_tested:=rdts_performed/suspected_cases]
agg1_lag

# check if having any stockouts correlates with lower confirmation rate
agg2 = data[, .(confirmed_cases=sum(confirmed_cases, na.rm=T), 
				suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts]
agg2[, prop_confirmed:=confirmed_cases/suspected_cases]
agg2

# check if having any stockouts in the previous month correlates with lower confirmation rate
agg2_lag = data[, .(confirmed_cases=sum(confirmed_cases, na.rm=T), 
					suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts_lag]
agg2_lag[, prop_confirmed:=confirmed_cases/suspected_cases]
agg2_lag

# check for statistical significance using OLS
summary(lm(prop_tested~any_stockouts, data))
summary(lm(prop_tested~any_stockouts_lag, data))
summary(lm(prop_confirmed~any_stockouts, data))
summary(lm(prop_confirmed~any_stockouts_lag, data))
# --------------------------------------------------------------------------

