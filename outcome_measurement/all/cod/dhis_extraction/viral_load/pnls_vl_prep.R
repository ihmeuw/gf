# Prep & remove outliers from the COD DHIS2 PNLS Viral Load data 
#
# Caitlin O'Brien-Carelli
# 10/1/2018
# ----------------------------------------------

# --------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(stringr) 
#-----------------------------------
# set working directories

# detect if operating on windows or on the cluster 
root = ifelse(Sys.info()[1]=='Windows', 'J:', '/home/j')

# set the directory for input and output
dir <- paste0(root, '/Project/Evaluation/GF/outcome_measurement/cod/dhis/')

#-------------------------------------
# read in the subset of PNLS data specific to viral load 

vl <- readRDS(paste0(dir, 'prepped/viral_load_pnls.rds'))

#------------------------
# demarcate 'support' entries compared to regular entries and remove support

vl[ , element_eng:=tolower(element_eng)]
vl[grep(element_eng, pattern='support'), support:=TRUE]
vl[is.na(support), support:=FALSE]
vl <- vl[support==FALSE]

#----------------------------------
# merge subgroups to create distinct elements 

# create stratifications by population
vl[grep(element_eng, pattern='lactating'), group:='Lactating women']
vl[grep(element_eng, pattern='pregnant'), group:='Pregnant women']
vl[grep(element_eng, pattern='fe'), group:='Pregnant women']
vl[grep(element_eng, pattern='initial'), group:='Initial test']
vl[grep(element_eng, pattern='initiation'), group:='Initial test']
vl[grep(element_eng, pattern='6'), group:='After 6 months']
vl[grep(element_eng, pattern='other'), group:='Other']
vl[grep(element_eng, pattern='male'), group:='MSM']

#-----------------
# change category to sex and case status
vl[ , category:=tolower(category)]
vl[grep(category, pattern='nc'), case:='New' ]
vl[grep(category, pattern='ac'), case:='Old' ]

vl[grep(category, pattern='féminin'), sex:='Female' ]
vl[grep(category, pattern='masculin'), sex:='Male']
vl[grep(group, pattern='women'), sex:='Female' ]
vl[grep(group, pattern='MSM'), sex:='Male']

vl[ , category:=NULL]

#-----------------------
# restructure the data to consist of single data points with stratifications

vl[grep(element_eng, pattern='received'), variable:='PLHIV who received a VL test']
vl[grep(element_eng, pattern='undetectable'), variable:='PLHIV with undetectable VL']

#-----------------------
# remove facilities that only reported 0 tests performed
zeroes = vl[variable=='PLHIV who received a VL test',.(total=sum(value)), by=org_unit]
zeroes = zeroes[total > 0]
org_0s = zeroes$org_unit

vl = vl[org_unit %in% org_0s]

#-----------------------
# restrcture the data to have two variables and associated risk groups
# do not lose any values (same number of unique rows)

vl = vl[  ,.(value=sum(value)), by=.(variable, 
              date, org_unit_id, org_unit,level, 
              health_zone, dps, mtk, group, case, sex)]

#-----------------------
# drop any values that violate equality constraints

# create a new data set for each variable
und <- vl[variable=='PLHIV with undetectable VL']
tests <- vl[variable=='PLHIV who received a VL test']

# drop values in which one variable is present but not the other
setnames(und, 'value', 'und')
und[ ,variable:=NULL]
setnames(tests, 'value', 'test')
tests[ ,variable:=NULL]

rat <- merge(und, tests)

# maintain equality constraints
rat <- rat[test >= und]

# reshape long
idVars = c("date", "org_unit_id", "org_unit", "level", "health_zone", "dps", "mtk", "group", "case", "sex")
rat = melt(rat, id.vars=idVars)

rat[variable=='und', variable:='PLHIV with undetectable VL']
rat[variable=='test', variable:='PLHIV who received a VL test']

# reassign the new data set to vl
vl = rat

#------------------------
# save the interim output to use for outlier removal

saveRDS(vl, paste0(dir, 'prepped/viral_load_pnls_interim.rds'))

#------------------------

