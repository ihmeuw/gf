# ------------------------------------------------------------------------------
# David Phillips
# 
# 7/23/2019
# Set up the data for ssc_impact.r
# The current working directory should be the root of this repo
# ------------------------------------------------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
# -------------------


# ---------------------------------------------------------------------------------------
# Files, directories and settings

# whether or not to run analysis among UNICEF health zones only
unicefOnly = FALSE

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# input file
inFile = paste0(dir, '/impact_evaluation/cod/prepped_data/second_half_data_pre_model.rdata')

# file listing health zones
hzFile = paste0(dir, '/mapping/cod/ssc_lists/prepped_hz_list.csv')

# file identifying dps's
dpsFile = './core/hz_renaming_file.csv'

# output files
outFile = paste0(dir, '/impact_evaluation/cod/prepped_data/ssc_analyses/DiD_input_data.rdata')

# modify output file names if we're running analysis among UNICEF health zones only
if(unicefOnly) outFile = gsub('.rdata', '_UNICEF_HZs_only.rdata', outFile)
# ---------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load/prep data

# load input data
load(inFile)

# load list of health zones with full package
hzList = fread(hzFile)
if(unicefOnly) hzList = hzList[unicef_supported==1]

# compute all cause mortality rate
untransformed[ , allDeaths_under5_rate := totalDeathsAllDiseases_under5/population*100000]

# subset columns
hzList = hzList[,c('health_zone','unicef_supported')]
data = untransformed[, c('health_zone','date','malariaDeaths_under5','malariaDeaths_under5_rate','completeness_totalPatientsTreated', 'totalDeathsAllDiseases_under5', 'allDeaths_under5_rate')]

# drop rows post-2017 because DHIS doesn't have age-specific mortality
data = data[date<2018]

# check for health zones in the list that aren't in the data 
# kikwit-nord is ok because it's combined with kikwit-sud in the data
# there are 10 others that aren't explained
hzList$health_zone[!hzList$health_zone %in% untransformed$health_zone]

# list health zones that will get counted as the control group
unique(untransformed$health_zone[!untransformed$health_zone %in% hzList$health_zone])

# identify 'intervention' health zones in the data
hzList[, intervention:=1]
data = merge(data, hzList, by='health_zone', all.x=TRUE)
data[is.na(intervention), intervention:=0]
data[, intervention_label:=ifelse(intervention==1, '2. Intervention', '1. Control')]

# identify before/after
data[, period:=ifelse(date<2017, 0, 1)]
data[, period_label:=ifelse(period==1, '2. After 2017', '1. Before 2017')]

# subset to only GF DPSs
dpsList = unique(fread(dpsFile)[dps!='0',c('dps','health_zone'),with=FALSE])
gfDPS = c('bas-uele', 'equateur', 'haut-uele', 'ituri', 'kinshasa', 'kongo-central', 
		'kwango', 'kwilu', 'mai-ndombe', 'maniema', 'mongala', 'nord-kivu', 
		'nord-ubangi', 'sud-ubangi', 'tshopo', 'tshuapa')
dpsList[dps=='kasai-central' & health_zone=='lubunga', health_zone:='lubunga2'] # this is about to get dropped because PMI
dpsList[dps=='nord-ubangi' & health_zone=='bili', health_zone:='bili2'] # both "bili" health zones are in the intervention
data = merge(data, dpsList, by='health_zone', all.x=TRUE)
data = data[dps %in% gfDPS]
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Aggregate data

# take averages by intervention/period to have a data frame to predict amongst
means = data[, .(malariaDeaths_under5_rate=mean(malariaDeaths_under5_rate), 
	allDeaths_under5_rate=mean(allDeaths_under5_rate),	
	lower_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.2), 
	upper_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.8), 
	lower_pctle_all_cause=quantile(allDeaths_under5_rate, 0.2), 
	upper_pctle_all_cause=quantile(allDeaths_under5_rate, 0.8)), 
	by=c('period_label','intervention_label', 'period', 'intervention')]
	
# take averages by intervention/date for time series graph of data
means_ts = data[, .(malariaDeaths_under5_rate=median(malariaDeaths_under5_rate), 
	allDeaths_under5_rate=median(allDeaths_under5_rate),	
	lower_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.2), 
	upper_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.8), 
	lower_pctle_all_cause=quantile(allDeaths_under5_rate, 0.2), 
	upper_pctle_all_cause=quantile(allDeaths_under5_rate, 0.8)), 
	by=c('intervention_label', 'date', 'intervention')]
# ------------------------------------------------------------------------------


# ----------------------------------------------------
# Save workspace (excluding temporary objects)
dropObjs = c('cut','preds1','preds2','untransformed','outFile','inFile')
save(list=ls()[!ls() %in% dropObjs], file=outFile)
# ----------------------------------------------------
