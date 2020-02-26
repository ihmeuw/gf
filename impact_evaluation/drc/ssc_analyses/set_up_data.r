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
fullpackageOnly = FALSE

# root directory
j = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')
dir = paste0(j, '/Project/Evaluation/GF/')

# input file
inFile = paste0(dir, '/impact_evaluation/cod/prepped_data/second_half_data_pre_model.rdata')
inFileUnadjusted = paste0(dir, '/impact_evaluation/cod/prepped_data/outcomes_impact_corrected.RDS')

# file listing health zones
hzFile = paste0(dir, '/mapping/cod/ssc_lists/prepped_hz_list.csv')

# file identifying dps's
dpsFile = './core/hz_renaming_file.csv'

# output files
outFile = paste0(dir, '/impact_evaluation/cod/prepped_data/ssc_analyses/DiD_input_data.rdata')

# modify output file names if we're running analysis among UNICEF health zones only
if(fullpackageOnly) outFile = gsub('.rdata', '_full_package_HZs_only.rdata', outFile)
# ---------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load/prep data

# load input data
load(inFile)
unadjusted = readRDS(inFileUnadjusted)

# load list of health zones with full package
hzList = fread(hzFile)

# add extra health zones to full-package list
# (list emailed to us by Eugene Nsambu July 29, 2019)
# (these were manually updated in the spreadsheet as of 2/25/2020)
extraHZs = c('basoko','isangi','yabahondo','yaleko','yakusu','aketi','buta')
hzList[, full_package:=unicef_supported]
hzList[health_zone %in% extraHZs, full_package:=1]

# subset to only health zones with the full package if specified
if(fullpackageOnly) hzList = hzList[full_package==1]

# compute all cause mortality rate
untransformed[ , allDeaths_under5_rate := totalDeathsAllDiseases_under5/population*100000]

# compute proportion of estiamted cases detected (annually)
unadjusted[, year:=floor(date)]
annual = unadjusted[, .(incidence=mean(incidence), newCasesMalaria=sum(newCasesMalariaMild+newCasesMalariaSevere)), by=c('health_zone','year')]
annual[, proportion_estimated_cases_detected:=newCasesMalaria/incidence]

# subset columns
hzList = hzList[,c('health_zone','full_package')]
data = untransformed[, c('health_zone','date','mildMalariaTreated_under5_rate','severeMalariaTreated_under5_rate','malariaDeaths_under5','malariaDeaths_under5_rate','completeness_totalPatientsTreated', 'totalDeathsAllDiseases_under5', 'allDeaths_under5_rate')]
unadjusted = unadjusted[,c('health_zone','date','year','newCasesMalariaMild_under5_rate','newCasesMalariaSevere_under5_rate'), with=FALSE]
data = merge(data, unadjusted, by=c('health_zone','date'), all.x=TRUE)
data = merge(data, annual, by=c('health_zone','year'), all.x=TRUE)

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
data[, intervention_label:=ifelse(intervention==1, '2. Health Zones with SSCs (intervention)', '1. Health Zones without SSCs (control)')]
if(fullpackageOnly)  data[, intervention_label:=ifelse(intervention==1, '2. Health Zones with iCCM (intervention)', '1. Health Zones without iCCM (control)')]

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
means = data[, .(
	mildMalariaTreated_under5_rate=mean(mildMalariaTreated_under5_rate), 
	severeMalariaTreated_under5_rate=mean(mildMalariaTreated_under5_rate), 
	newCasesMalariaMild_under5_rate=mean(newCasesMalariaMild_under5_rate), 
	newCasesMalariaSevere_under5_rate=mean(newCasesMalariaSevere_under5_rate), 
	proportion_estimated_cases_detected=mean(proportion_estimated_cases_detected), 
	malariaDeaths_under5_rate=mean(malariaDeaths_under5_rate), 
	allDeaths_under5_rate=mean(allDeaths_under5_rate),	
	lower_pctle_mild_detection=quantile(newCasesMalariaMild_under5_rate, 0.2), 
	upper_pctle_mild_detection=quantile(newCasesMalariaMild_under5_rate, 0.8), 
	lower_pctle_severe_detection=quantile(newCasesMalariaSevere_under5_rate, 0.2), 
	upper_pctle_severe_detection=quantile(newCasesMalariaSevere_under5_rate, 0.8), 
	lower_pctle_detection_prop=quantile(proportion_estimated_cases_detected, 0.2), 
	upper_pctle_detection_prop=quantile(proportion_estimated_cases_detected, 0.8), 
	lower_pctle_mild_coverage=quantile(mildMalariaTreated_under5_rate, 0.2), 
	upper_pctle_mild_coverage=quantile(mildMalariaTreated_under5_rate, 0.8), 
	lower_pctle_severe_coverage=quantile(severeMalariaTreated_under5_rate, 0.2), 
	upper_pctle_severe_coverage=quantile(severeMalariaTreated_under5_rate, 0.8), 
	lower_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.2), 
	upper_pctle_malaria=quantile(malariaDeaths_under5_rate, 0.8), 
	lower_pctle_all_cause=quantile(allDeaths_under5_rate, 0.2), 
	upper_pctle_all_cause=quantile(allDeaths_under5_rate, 0.8)), 
	by=c('period_label','intervention_label', 'period', 'intervention')]
	
# take averages by intervention/date for time series graph of data
means_ts = data[, .(
	mildMalariaTreated_under5_rate=median(mildMalariaTreated_under5_rate), 
	severeMalariaTreated_under5_rate=median(mildMalariaTreated_under5_rate), 
	newCasesMalariaMild_under5_rate=median(newCasesMalariaMild_under5_rate), 
	newCasesMalariaSevere_under5_rate=median(newCasesMalariaSevere_under5_rate), 
	proportion_estimated_cases_detected=median(proportion_estimated_cases_detected), 
	malariaDeaths_under5_rate=median(malariaDeaths_under5_rate), 
	allDeaths_under5_rate=median(allDeaths_under5_rate),	
	lower_pctle_mild_detection=quantile(newCasesMalariaMild_under5_rate, 0.2), 
	upper_pctle_mild_detection=quantile(newCasesMalariaMild_under5_rate, 0.8), 
	lower_pctle_severe_detection=quantile(newCasesMalariaSevere_under5_rate, 0.2), 
	upper_pctle_severe_detection=quantile(newCasesMalariaSevere_under5_rate, 0.8), 
	lower_pctle_detection_prop=quantile(proportion_estimated_cases_detected, 0.2), 
	upper_pctle_detection_prop=quantile(proportion_estimated_cases_detected, 0.8), 
	lower_pctle_mild_coverage=quantile(mildMalariaTreated_under5_rate, 0.2), 
	upper_pctle_mild_coverage=quantile(mildMalariaTreated_under5_rate, 0.8), 
	lower_pctle_severe_coverage=quantile(severeMalariaTreated_under5_rate, 0.2), 
	upper_pctle_severe_coverage=quantile(severeMalariaTreated_under5_rate, 0.8), 
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
