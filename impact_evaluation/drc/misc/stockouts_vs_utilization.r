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
dir = paste0(j, '/Project/Evaluation/GF/outcome_measurement/cod/dhis_data/')

baseFile = paste0(dir, '6_final_prepped/base/baseFile.rds')
siglFile = paste0(dir, '3_prepped/sigl/sigl_prepped.rds')
# --------------------------------------------------------------------------


# ------------------------------------
# Load data
base = readRDS(baseFile)
sigl = readRDS(siglFile)

# list element IDs of interest
rdtStockoutElement = 'o3S4pDMPNoO'
utilizationElement = 'CIzQAR8IWH1'
suspectedElement ='aZwnLALknnj'
confirmedElement = 'rfeqp2kdOGi'
presumedElement = 'aK0QXqm8Zxn'
# ------------------------------------


# --------------------------------------------------------------------------
# Prep data

# subset rows
data = base[element_id %in% c(utilizationElement, suspectedElement, confirmedElement, presumedElement)]
stockouts = sigl[element_id == rdtStockoutElement]

# rbind data sets together
data = rbind(data, stockouts, fill=TRUE)

# collapse out subpopulations and subset columns
byVars = c('dps','health_zone','org_unit','org_unit_id','date','element_id')
data = data[, .(value=sum(value, na.rm=T)), by=byVars]

# add nicer labels
data[element_id==rdtStockoutElement, variable:='days_out_of_stock']
data[element_id==utilizationElement, variable:='rdts_performed']
data[element_id==suspectedElement, variable:='suspected_cases']
data[element_id==confirmedElement, variable:='confirmed_cases']
data[element_id==presumedElement, variable:='presumed_cases_treated']

# reshape wide
data = dcast(data, dps+health_zone+org_unit+org_unit_id+date~variable)

# exclude facilities that have inexplicably more than 31 days stocked out
data[month(date)==2 & days_out_of_stock>28, days_out_of_stock:=NA]
data[month(date) %in% c(4, 6, 9, 11) & days_out_of_stock>30, days_out_of_stock:=NA]
data[! month(date) %in% c(2, 4, 6, 9, 11) & days_out_of_stock>31, days_out_of_stock:=NA]

# exclude health zones from org list (because this brings the S1 2019 % down to 24 from 26, in agreement with the PUDR)
data = data[!grepl('Zone de Sant', org_unit)]

# get proportion of time stocked out
data[month(date)==2, prop_stocked_out:=days_out_of_stock/28]
data[month(date) %in% c(4, 6, 9, 11), prop_stocked_out:=days_out_of_stock/30]
data[! month(date) %in% c(2, 4, 6, 9, 11), prop_stocked_out:=days_out_of_stock/31]

# compute any stockouts
data[, any_stockouts:=prop_stocked_out>0]

# display national numbers
national = data[, .(days_out_of_stock=sum(days_out_of_stock,na.rm=T), 
		mean_days_out_of_stock=mean(days_out_of_stock,na.rm=T),
		prop_facilities_with_any_stockouts=mean(any_stockouts,na.rm=T),
		rdts_performed=sum(rdts_performed,na.rm=T), 
		suspected_cases=sum(suspected_cases,na.rm=T), 
		confirmed_cases=sum(confirmed_cases,na.rm=T), 
		presumed_cases_treated=sum(presumed_cases_treated,na.rm=T), 
		n_facilities=.N), by=date]
national
ggplot(national, aes(y=days_out_of_stock, x=date)) + geom_point() + geom_smooth()

# graph national trends
p1 = ggplot(national[year(date)!=2017], aes(y=mean_days_out_of_stock, x=date))	+ 
	geom_point() + 
	geom_smooth() + 
	labs(title='Average Number of Days Each Facility Experienced RDT Stockouts', 
		subtitle='Among health facilities that reported to DHIS2', 
		y='Average Number of Days Stocked Out', x='Month', 
		caption='Line indicates moving average (LOESS)\nNumber of days stocked out not necessarily a single continuous stockout') + 
	theme_bw(base_size=16)

p2 = ggplot(national[year(date)!=2017], aes(y=prop_facilities_with_any_stockouts*100, x=date)) + 
	geom_point() + 
	geom_smooth() + 
	labs(title='Percentage of Health Facilities with Any Stockouts of RDTs', 
		subtitle='Among health facilities that reported to DHIS2', 
		y='Percentage of Health Facilities', x='', caption='') + 
	theme_bw(base_size=16)
library(gridExtra)
grid.arrange(p2, p1)

# compute proportion of suspected cases tested, confirmed and treated
data[, prop_tested:=rdts_performed/suspected_cases]
data[, prop_confirmed:=confirmed_cases/suspected_cases]
data[, prop_presumed:=presumed_cases_treated/suspected_cases]
data[prop_tested>1, prop_tested:=NA]
data[prop_confirmed>1, prop_confirmed:=NA]
data[prop_presumed>1, prop_presumed:=NA]

# check validity. the S1 PUDR says 24% of facilities had any stockouts of RDTs
mean(data[year(date)==2019]$any_stockouts, na.rm=T)

# create lag variables
byVars = c('dps','health_zone','org_unit','org_unit_id')
data[, prop_stocked_out_lag:=shift(prop_stocked_out), by=byVars]
data[, any_stockouts_lag:=prop_stocked_out_lag>0]
# --------------------------------------------------------------------------


# --------------------------------------------------------------------------
# Run analysis

# check if having any stockouts correlates with lower confirmation rate
agg2 = data[, .(confirmed_cases=sum(confirmed_cases, na.rm=T), 
				suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts]
agg2[, prop_confirmed:=confirmed_cases/suspected_cases]
agg2

# check if having any stockouts correlates with higher presumptive treatment
agg3 = data[, .(presumed_cases_treated=mean(presumed_cases_treated, na.rm=T), 
	presumed_cases_treated=sum(presumed_cases_treated, na.rm=T), 
	suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts]
agg3[, pct_presumed:=presumed_cases_treated/suspected_cases*100]
agg3

# check if having any stockouts in the previous month correlates with lower confirmation rate
agg2_lag = data[, .(confirmed_cases=sum(confirmed_cases, na.rm=T), 
					suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts_lag]
agg2_lag[, prop_confirmed:=confirmed_cases/suspected_cases]
agg2_lag

# check if having any stockouts in the previous month correlates with higher presumptive treatment
agg3_lag = data[, .(presumed_cases_treated=mean(presumed_cases_treated, na.rm=T), 
	presumed_cases_treated=sum(presumed_cases_treated, na.rm=T), 
	suspected_cases=sum(suspected_cases,na.rm=T)), by=any_stockouts_lag]
agg3_lag[, prop_presumed:=presumed_cases_treated/suspected_cases]
agg3_lag

# check for statistical significance using OLS
summary(lm(prop_tested~any_stockouts, data))
summary(lm(prop_tested~any_stockouts_lag, data))
summary(lm(prop_confirmed~any_stockouts, data))
summary(lm(prop_confirmed~any_stockouts_lag, data))
summary(lm(prop_presumed~any_stockouts, data))
summary(lm(prop_presumed~any_stockouts_lag, data))
# --------------------------------------------------------------------------

